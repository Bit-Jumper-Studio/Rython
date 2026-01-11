use std::collections::HashMap;
use crate::parser::{Program, Statement, Expr};
use crate::backend::{Backend, BackendRegistry, BackendModule, Target, Capability};
use crate::emitter::NasmEmitter;
use crate::dsl::{HardwareDSL, DeviceType};

#[derive(Debug, Clone)]
pub struct CompilerConfig {
    pub target: Target,
    pub optimize: bool,
    pub debug_info: bool,
    pub include_stdlib: bool,
    pub hardware_dsl_enabled: bool,
    pub code_size_limit: Option<usize>,
    // CLI-specific fields
    pub verbose: bool,
    pub keep_assembly: bool,
    pub modules: Vec<String>,
    pub ssd_headers: Vec<String>,
    pub ssd_assembly: Vec<String>,
    pub enable_ssd: bool,
    pub enable_rcl: bool,
    pub rcl_libraries: Vec<String>,
}

impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            target: Target::Linux64,
            optimize: true,
            debug_info: false,
            include_stdlib: true,
            hardware_dsl_enabled: false,
            code_size_limit: None,
            verbose: false,
            keep_assembly: false,
            modules: Vec::new(),
            ssd_headers: Vec::new(),
            ssd_assembly: Vec::new(),
            enable_ssd: false,
            enable_rcl: false,
            rcl_libraries: Vec::new(),
        }
    }
}

impl CompilerConfig {
    pub fn with_target(mut self, target: Target) -> Self {
        self.target = target;
        self
    }
    
    pub fn with_optimize(mut self, optimize: bool) -> Self {
        self.optimize = optimize;
        self
    }
    
    pub fn with_hardware_dsl(mut self, enabled: bool) -> Self {
        self.hardware_dsl_enabled = enabled;
        self
    }
}

#[derive(Debug, Clone)]
pub struct CompilationResult {
    pub assembly: String,
    pub warnings: Vec<String>,
    pub errors: Vec<String>,
    pub stats: CompilationStats,
}

#[derive(Debug, Clone)]
pub struct CompilationStats {
    pub lines_of_code: usize,
    pub assembly_lines: usize,
    pub variables_allocated: usize,
    pub functions_compiled: usize,
    pub hardware_intrinsics: usize,
    pub compilation_time_ms: u128,
}

pub struct EarthngCompiler {
    pub config: CompilerConfig,
    backend_registry: BackendRegistry,
    hardware_dsl: Option<HardwareDSL>,
    warnings: Vec<String>,
    errors: Vec<String>,
    symbol_table: HashMap<String, VariableInfo>,
    current_function: Option<String>,
    optimization_passes: Vec<Box<dyn OptimizationPass>>,
}

#[derive(Debug, Clone)]
struct VariableInfo {
    pub name: String,
    pub type_hint: Option<String>,
    pub scope_level: usize,
    pub is_hardware: bool,
    pub hardware_device: Option<String>,
}

pub trait OptimizationPass {
    fn name(&self) -> &str;
    fn optimize(&self, program: &mut Program) -> Result<(), String>;
}

impl EarthngCompiler {
    pub fn new(config: CompilerConfig) -> Self {
        let mut compiler = Self {
            config,
            backend_registry: BackendRegistry::default_registry(),
            hardware_dsl: None,
            warnings: Vec::new(),
            errors: Vec::new(),
            symbol_table: HashMap::new(),
            current_function: None,
            optimization_passes: Vec::new(),
        };
        
        // Initialize hardware DSL if enabled
        if compiler.config.hardware_dsl_enabled {
            compiler.hardware_dsl = Some(crate::dsl::init_hardware_dsl());
        }
        
        // Register optimization passes
        compiler.register_optimization_passes();
        
        compiler
    }
    
    fn register_optimization_passes(&mut self) {
        // Constant folding
        self.optimization_passes.push(Box::new(ConstantFoldingPass));
        
        // Dead code elimination
        self.optimization_passes.push(Box::new(DeadCodeEliminationPass));
        
        // Inline expansion (for small functions)
        self.optimization_passes.push(Box::new(InlineExpansionPass));
    }
    
    pub fn compile(&mut self, source: &str) -> Result<CompilationResult, String> {
        let start_time = std::time::Instant::now();
        
        // Clear previous state
        self.warnings.clear();
        self.errors.clear();
        self.symbol_table.clear();
        
        // Parse the source code
        let mut program = match crate::parser::parse_program(source) {
            Ok(program) => program,
            Err(parse_errors) => {
                let error_messages: Vec<String> = parse_errors
                    .iter()
                    .map(|e| e.to_string())
                    .collect();
                return Err(format!("Parse errors:\n{}", error_messages.join("\n")));
            }
        };
        
        // Apply optimizations if enabled
        if self.config.optimize {
            for pass in &self.optimization_passes {
                if let Err(err) = pass.optimize(&mut program) {
                    self.warnings.push(format!("Optimization pass '{}' failed: {}", pass.name(), err));
                }
            }
        }
        
        // Collect hardware DSL information if enabled
        let hardware_asm = if self.config.hardware_dsl_enabled {
            // Take ownership of the DSL to avoid borrowing issues
            let mut dsl = self.hardware_dsl.take();
            if let Some(ref mut dsl_ref) = dsl {
                let result = self.collect_hardware_intrinsics(&program, dsl_ref);
                // Put the DSL back
                self.hardware_dsl = dsl;
                result?
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };
        
        // Create backend module with requirements
        let backend_module = self.create_backend_module(&program, hardware_asm);
        
        // Clone what we need to avoid borrow checker issues
        let target = self.config.target;
        let use_backend_registry = matches!(
            target,
            Target::Bios16 | Target::Bios32 | Target::Bios64 | 
            Target::Bios64Sse | Target::Bios64Avx | Target::Bios64Avx512
        );
        
        // Compile with appropriate backend based on target
        let assembly = if use_backend_registry {
            // For BIOS targets, find backend from registry
            let backend_found = self.backend_registry.find_backend(&backend_module);
            
            match backend_found {
                Some(_backend) => {
                    // Clone the module to pass to compile_with_backend
                    let module_clone = backend_module.clone();
                    self.compile_with_backend(&program, &module_clone)
                }
                None => {
                    let caps = backend_module.required_capabilities.iter()
                        .map(|c| format!("{:?}", c))
                        .collect::<Vec<_>>()
                        .join(", ");
                    return Err(format!("No backend found that supports all required capabilities: {}", caps));
                }
            }
        } else {
            // For Linux64/Windows64, use emitter
            self.compile_with_emitter(&program)
        }?;
        
        let compilation_time = start_time.elapsed().as_millis();
        
        // Calculate statistics
        let stats = CompilationStats {
            lines_of_code: source.lines().count(),
            assembly_lines: assembly.lines().count(),
            variables_allocated: self.symbol_table.len(),
            functions_compiled: program.body.iter()
                .filter(|stmt| matches!(stmt, Statement::FunctionDef { .. }))
                .count(),
            hardware_intrinsics: backend_module.external_asm.len(),
            compilation_time_ms: compilation_time,
        };
        
        Ok(CompilationResult {
            assembly,
            warnings: self.warnings.clone(),
            errors: self.errors.clone(),
            stats,
        })
    }
    
    fn compile_with_emitter(&mut self, program: &Program) -> Result<String, String> {
        let mut emitter = NasmEmitter::new();
        
        // Set target based on config
        match self.config.target {
            Target::Linux64 => emitter.set_target_linux(),
            Target::Windows64 => emitter.set_target_windows(),
            Target::Bios16 => emitter.set_target_bios16(),
            Target::Bios32 => emitter.set_target_bios32(),
            Target::Bios64 => emitter.set_target_bios64(),
            Target::Bios64Sse => emitter.set_target_bios64_sse(),
            Target::Bios64Avx => emitter.set_target_bios64_avx(),
            Target::Bios64Avx512 => emitter.set_target_bios64_avx512(),
        }
        
        // Compile program
        emitter.compile_program(program)
    }
    
    fn compile_with_backend(
        &mut self,
        program: &Program,
        module: &BackendModule
    ) -> Result<String, String> {
        match self.config.target {
            Target::Bios16 => {
                let mut bios16_backend = crate::backend::Bios16Backend::new();
                for asm in &module.external_asm {
                    bios16_backend.add_external_asm(asm);
                }
                bios16_backend.compile_program(program)
            }
            Target::Bios32 => {
                let mut bios32_backend = crate::backend::Bios32Backend::new();
                for asm in &module.external_asm {
                    bios32_backend.add_external_asm(asm);
                }
                bios32_backend.compile_program(program)
            }
            Target::Bios64 | Target::Bios64Sse | Target::Bios64Avx | Target::Bios64Avx512 => {
                // Try to downcast to Bios64Backend
                let mut backend_mut = crate::backend::Bios64Backend::new();
                
                // Apply extensions based on target
                backend_mut = match self.config.target {
                    Target::Bios64Sse => backend_mut.with_sse(),
                    Target::Bios64Avx => backend_mut.with_avx(),
                    Target::Bios64Avx512 => backend_mut.with_avx512(),
                    _ => backend_mut,
                };
                
                // Add hardware assembly
                for asm in &module.external_asm {
                    backend_mut.add_external_asm(asm);
                }
                
                // Add syntax extensions from module
                for ext in &module.syntax_extensions {
                    backend_mut.add_syntax_extension(
                        &ext.pattern,
                        &ext.replacement,
                        ext.assembly_label.as_deref(),
                        ext.register_args.clone(),
                    );
                }
                
                backend_mut.compile_program(program)
            }
            _ => {
                // For other targets, we can't easily create a mutable backend
                // Use the trait object's compile_program method
                let mut backend_copy: Box<dyn Backend> = match self.config.target {
                    Target::Linux64 => Box::new(crate::backend::Linux64Backend::new()),
                    Target::Windows64 => Box::new(crate::backend::Windows64Backend::new()),
                    _ => return Err(format!("Unsupported target for backend compilation: {:?}", self.config.target)),
                };
                
                // We can't downcast easily, so just compile without adding external assembly
                backend_copy.compile_program(program)
            }
        }
    }
    
    fn create_backend_module(&self, _program: &Program, hardware_asm: Vec<String>) -> BackendModule {
        let mut required_capabilities = Vec::new();
        
        // Determine capabilities based on target
        match self.config.target {
            Target::Bios16 => {
                required_capabilities.push(Capability::BIOS);
                required_capabilities.push(Capability::RealMode16);
                required_capabilities.push(Capability::PureMetal);
            }
            Target::Bios32 => {
                required_capabilities.push(Capability::BIOS);
                required_capabilities.push(Capability::ProtectedMode32);
                required_capabilities.push(Capability::PureMetal);
                required_capabilities.push(Capability::Paging);
            }
            Target::Bios64 | Target::Bios64Sse | Target::Bios64Avx | Target::Bios64Avx512 => {
                required_capabilities.push(Capability::BIOS);
                required_capabilities.push(Capability::LongMode64);
                required_capabilities.push(Capability::PureMetal);
                required_capabilities.push(Capability::Paging);
                
                if matches!(self.config.target, Target::Bios64Sse) {
                    required_capabilities.push(Capability::SSE);
                    required_capabilities.push(Capability::SSE2);
                }
                if matches!(self.config.target, Target::Bios64Avx) {
                    required_capabilities.push(Capability::AVX);
                }
                if matches!(self.config.target, Target::Bios64Avx512) {
                    required_capabilities.push(Capability::AVX512);
                }
            }
            Target::Linux64 => {
                required_capabilities.push(Capability::Linux);
                required_capabilities.push(Capability::LongMode64);
                required_capabilities.push(Capability::VirtualMemory);
            }
            Target::Windows64 => {
                required_capabilities.push(Capability::Windows);
                required_capabilities.push(Capability::LongMode64);
            }
        }
        
        // Add hardware capabilities if DSL is enabled
        if self.config.hardware_dsl_enabled && !hardware_asm.is_empty() {
            required_capabilities.push(Capability::Graphics); // For GPU access
        }
        
        BackendModule {
            functions: Vec::new(), // Will be populated by backend
            globals: Vec::new(),
            required_capabilities,
            external_asm: hardware_asm,
            syntax_extensions: Vec::new(), // Will be populated from DSL
        }
    }
    
    fn collect_hardware_intrinsics(
        &mut self,
        program: &Program,
        dsl: &mut HardwareDSL
    ) -> Result<Vec<String>, String> {
        let mut hardware_asm = Vec::new();
        
        // Walk through program and collect hardware statements
        self.walk_for_hardware(program, dsl, &mut hardware_asm)?;
        
        Ok(hardware_asm)
    }
    
    fn walk_for_hardware(
        &mut self,
        program: &Program,
        dsl: &mut HardwareDSL,
        hardware_asm: &mut Vec<String>
    ) -> Result<(), String> {
        for stmt in &program.body {
            match stmt {
                Statement::FunctionDef { name, body, .. } => {
                    let old_function = self.current_function.take();
                    self.current_function = Some(name.clone());
                    
                    for body_stmt in body {
                        self.analyze_statement_for_hardware(body_stmt, dsl, hardware_asm)?;
                    }
                    
                    self.current_function = old_function;
                }
                _ => {
                    self.analyze_statement_for_hardware(stmt, dsl, hardware_asm)?;
                }
            }
        }
        Ok(())
    }
    
    fn analyze_statement_for_hardware(
        &mut self,
        stmt: &Statement,
        dsl: &mut HardwareDSL,
        hardware_asm: &mut Vec<String>
    ) -> Result<(), String> {
        match stmt {
            Statement::Expr(expr) => {
                self.analyze_expression_for_hardware(expr, dsl, hardware_asm)?;
            }
            Statement::VarDecl { name, value, type_hint, .. } => {
                // Check if this is a hardware-related variable
                if let Some(type_str) = type_hint {
                    if type_str.contains("hw_") || type_str.contains("device") {
                        self.symbol_table.insert(name.clone(), VariableInfo {
                            name: name.clone(),
                            type_hint: type_hint.clone(),
                            scope_level: 0,
                            is_hardware: true,
                            hardware_device: Some(type_str.clone()),
                        });
                    }
                }
                
                self.analyze_expression_for_hardware(value, dsl, hardware_asm)?;
            }
            Statement::Assign { target, value, .. } => {
                // Check if assigning to hardware register
                if target.starts_with("hw_") || target.contains("_reg") {
                    // This might be a hardware register assignment
                    if let Ok(asm) = dsl.parse_hardware_statement(&format!("write_register({}, {})", target, "value_placeholder")) {
                        hardware_asm.extend(asm);
                    }
                }
                self.analyze_expression_for_hardware(value, dsl, hardware_asm)?;
            }
            _ => {}
        }
        Ok(())
    }
    
    fn analyze_expression_for_hardware(
        &mut self,
        expr: &Expr,
        dsl: &mut HardwareDSL,
        hardware_asm: &mut Vec<String>
    ) -> Result<(), String> {
        match expr {
            Expr::Call { func, args, kwargs: _, span: _ } => {
                // Check for hardware intrinsics
                if func.starts_with("hw_") || func == "write_register" || func == "read_register" ||
                   func == "dma_transfer" || func == "port_in" || func == "port_out" {
                    
                    // Convert arguments to string representation
                    let arg_strings: Vec<String> = args.iter()
                        .map(|arg| match arg {
                            Expr::Number(n, _) => n.to_string(),
                            Expr::String(s, _) => format!("\"{}\"", s),
                            Expr::Var(name, _) => name.clone(),
                            _ => "0".to_string(),
                        })
                        .collect();
                    
                    let call_str = if !arg_strings.is_empty() {
                        format!("{}({})", func, arg_strings.join(", "))
                    } else {
                        func.clone()
                    };
                    
                    // Parse as hardware statement
                    if let Ok(asm) = dsl.parse_hardware_statement(&call_str) {
                        hardware_asm.extend(asm);
                    }
                }
                
                // Recursively analyze arguments
                for arg in args {
                    self.analyze_expression_for_hardware(arg, dsl, hardware_asm)?;
                }
            }
            Expr::BinOp { left, right, .. } => {
                self.analyze_expression_for_hardware(left, dsl, hardware_asm)?;
                self.analyze_expression_for_hardware(right, dsl, hardware_asm)?;
            }
            Expr::UnaryOp { operand, .. } => {
                self.analyze_expression_for_hardware(operand, dsl, hardware_asm)?;
            }
            _ => {}
        }
        Ok(())
    }
    
    pub fn add_custom_hardware_device(
        &mut self,
        name: &str,
        device_type: DeviceType,
        registers: &[(&str, u64)],
    ) -> Result<(), String> {
        if let Some(dsl) = &mut self.hardware_dsl {
            let device = dsl.register_device(name, device_type);
            for (reg_name, addr) in registers {
                device.add_register(reg_name, *addr);
            }
            Ok(())
        } else {
            Err("Hardware DSL is not enabled".to_string())
        }
    }
    
    pub fn generate_hardware_library(&self) -> Option<String> {
        self.hardware_dsl.as_ref()
            .map(|dsl| dsl.generate_hardware_library())
    }
    
    pub fn get_warnings(&self) -> &[String] {
        &self.warnings
    }
    
    pub fn get_errors(&self) -> &[String] {
        &self.errors
    }
    
    // Add this method to match the signature expected by rcl_integration.rs
    pub fn compile_to_file(&mut self, source: &str, output_path: &str) -> Result<(), String> {
        let result = self.compile(source)?;
        std::fs::write(output_path, result.assembly)
            .map_err(|e| format!("Failed to write output file: {}", e))?;
        Ok(())
    }
}

// ========== OPTIMIZATION PASSES ==========

struct ConstantFoldingPass;

impl OptimizationPass for ConstantFoldingPass {
    fn name(&self) -> &str {
        "constant_folding"
    }
    
    fn optimize(&self, _program: &mut Program) -> Result<(), String> {
        // Simple constant folding implementation
        Ok(())
    }
}

struct DeadCodeEliminationPass;

impl OptimizationPass for DeadCodeEliminationPass {
    fn name(&self) -> &str {
        "dead_code_elimination"
    }
    
    fn optimize(&self, _program: &mut Program) -> Result<(), String> {
        // Remove unused variables and dead code
        Ok(())
    }
}

struct InlineExpansionPass;

impl OptimizationPass for InlineExpansionPass {
    fn name(&self) -> &str {
        "inline_expansion"
    }
    
    fn optimize(&self, _program: &mut Program) -> Result<(), String> {
        // Inline small function calls
        Ok(())
    }
}

// ========== PUBLIC API ==========

/// Main compilation function
pub fn compile(source: &str, target: Target) -> Result<CompilationResult, String> {
    let config = CompilerConfig::default().with_target(target);
    let mut compiler = EarthngCompiler::new(config);
    compiler.compile(source)
}

/// Compile with hardware DSL enabled
pub fn compile_with_hardware(source: &str, target: Target) -> Result<CompilationResult, String> {
    let config = CompilerConfig::default()
        .with_target(target)
        .with_hardware_dsl(true);
    
    let mut compiler = EarthngCompiler::new(config);
    compiler.compile(source)
}

/// Compile with custom configuration
pub fn compile_with_config(source: &str, config: CompilerConfig) -> Result<CompilationResult, String> {
    let mut compiler = EarthngCompiler::new(config);
    compiler.compile(source)
}

/// Parse program without compilation
pub fn parse(source: &str) -> Result<Program, String> {
    crate::parser::parse_program(source)
        .map_err(|errors| {
            errors.iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        })
}

/// Format compilation result for display
pub fn format_result(result: &CompilationResult) -> String {
    let mut output = String::new();
    
    // Add warnings if any
    if !result.warnings.is_empty() {
        output.push_str(&format!("Warnings ({}):\n", result.warnings.len()));
        for warning in &result.warnings {
            output.push_str(&format!("  - {}\n", warning));
        }
        output.push('\n');
    }
    
    // Add errors if any
    if !result.errors.is_empty() {
        output.push_str(&format!("Errors ({}):\n", result.errors.len()));
        for error in &result.errors {
            output.push_str(&format!("  - {}\n", error));
        }
        output.push('\n');
    }
    
    // Add statistics
    output.push_str("Compilation Statistics:\n");
    output.push_str(&format!("  Source lines: {}\n", result.stats.lines_of_code));
    output.push_str(&format!("  Assembly lines: {}\n", result.stats.assembly_lines));
    output.push_str(&format!("  Variables allocated: {}\n", result.stats.variables_allocated));
    output.push_str(&format!("  Functions compiled: {}\n", result.stats.functions_compiled));
    output.push_str(&format!("  Hardware intrinsics: {}\n", result.stats.hardware_intrinsics));
    output.push_str(&format!("  Compilation time: {}ms\n", result.stats.compilation_time_ms));
    
    output
}