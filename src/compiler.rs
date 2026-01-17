
use std::collections::HashMap;
use crate::parser::{Program, Statement, Expr};
use crate::backend::{Backend, BackendRegistry, BackendModule, Target, Capability};
use crate::emitter::NasmEmitter;
use crate::dsl::{HardwareDSL, DeviceType};
use crate::extension::{ExtensionRegistry, EarthngModule, BasicAssemblyEmitter, MathModule, StringModule, SystemModule};

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

pub struct EarthangCompiler {
    pub config: CompilerConfig,
    backend_registry: BackendRegistry,
    hardware_dsl: Option<HardwareDSL>,
    warnings: Vec<String>,
    errors: Vec<String>,
    symbol_table: HashMap<String, VariableInfo>,
    current_function: Option<String>,
    optimization_passes: Vec<Box<dyn OptimizationPass>>,
    extension_registry: ExtensionRegistry,
}

#[derive(Debug, Clone)]
struct VariableInfo {
    #[allow(dead_code)]
    pub name: String,
    #[allow(dead_code)]
    pub type_hint: Option<String>,
    #[allow(dead_code)]
    pub scope_level: usize,
    #[allow(dead_code)]
    pub is_hardware: bool,
    #[allow(dead_code)]
    pub hardware_device: Option<String>,
}

pub trait OptimizationPass {
    fn name(&self) -> &str;
    fn optimize(&self, program: &mut Program) -> Result<(), String>;
}

impl EarthangCompiler {
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
            extension_registry: ExtensionRegistry::new(),
        };
        
        if compiler.config.hardware_dsl_enabled {
            compiler.hardware_dsl = Some(crate::dsl::init_hardware_dsl());
        }
        
        compiler.register_optimization_passes();
        compiler.register_builtin_extensions();
        
        compiler
    }
    
    fn register_optimization_passes(&mut self) {
        self.optimization_passes.push(Box::new(ConstantFoldingPass));
        self.optimization_passes.push(Box::new(DeadCodeEliminationPass));
        self.optimization_passes.push(Box::new(InlineExpansionPass));
    }
    
    fn register_builtin_extensions(&mut self) {
        self.extension_registry.register_module(Box::new(MathModule::new()));
        self.extension_registry.register_module(Box::new(StringModule::new()));
        self.extension_registry.register_module(Box::new(SystemModule::new()));
    }
    
    fn statement_has_extension_call(&self, stmt: &Statement) -> bool {
        match stmt {
            Statement::Expr(expr) => self.expression_has_extension_call(expr),
            Statement::VarDecl { value, .. } => self.expression_has_extension_call(value),
            Statement::Assign { value, .. } => self.expression_has_extension_call(value),
            Statement::FunctionDef { body, .. } => {
                body.iter().any(|s| self.statement_has_extension_call(s))
            }
            Statement::HardwareFunctionDef { body, .. } => {
                body.iter().any(|s| self.statement_has_extension_call(s))
            }
            Statement::If { condition, then_block, elif_blocks, else_block, .. } => {
                self.expression_has_extension_call(condition) ||
                then_block.iter().any(|s| self.statement_has_extension_call(s)) ||
                elif_blocks.iter().any(|(c, b)| {
                    self.expression_has_extension_call(c) ||
                    b.iter().any(|s| self.statement_has_extension_call(s))
                }) ||
                else_block.as_ref().map_or(false, |b| {
                    b.iter().any(|s| self.statement_has_extension_call(s))
                })
            }
            Statement::While { condition, body, .. } => {
                self.expression_has_extension_call(condition) ||
                body.iter().any(|s| self.statement_has_extension_call(s))
            }
            Statement::Return(expr) => {
                expr.as_ref().map_or(false, |e| self.expression_has_extension_call(e))
            }
            _ => false,
        }
    }
    
    fn expression_has_extension_call(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Call { func, .. } => self.extension_registry.has_function(func),
            Expr::BinOp { left, right, .. } => {
                self.expression_has_extension_call(left) || self.expression_has_extension_call(right)
            }
            Expr::UnaryOp { operand, .. } => self.expression_has_extension_call(operand),
            Expr::BoolOp { values, .. } => {
                values.iter().any(|e| self.expression_has_extension_call(e))
            }
            Expr::Compare { left, comparators, .. } => {
                self.expression_has_extension_call(left) || 
                comparators.iter().any(|e| self.expression_has_extension_call(e))
            }
            _ => false,
        }
    }
    
    #[allow(dead_code)]
    fn handle_extension_call(
        &self,
        func: &str,
        args: &[Expr],
        target: &Target,
    ) -> Result<Option<String>, String> {
        if let Some(module) = self.extension_registry.find_module_for_function(func) {
            let mut emitter = BasicAssemblyEmitter::new(*target);
            match module.compile_function(func, args, target, &mut emitter) {
                Ok(asm) => Ok(Some(asm)),
                Err(_e) => Ok(None),
            }
        } else {
            Ok(None)
        }
    }
    
    fn compile_with_extensions(&mut self, program: &Program) -> Result<String, String> {
        let mut emitter = NasmEmitter::new();
        
        match self.config.target {
            Target::Linux64 => emitter.set_target_linux(),
            Target::Bios16 => emitter.set_target_bios16(),
            Target::Bios32 => emitter.set_target_bios32(),
            Target::Bios64 => emitter.set_target_bios64(),
            Target::Bios64Sse => emitter.set_target_bios64_sse(),
            Target::Bios64Avx => emitter.set_target_bios64_avx(),
            Target::Bios64Avx512 => emitter.set_target_bios64_avx512(),
        }
        
        let mut asm = emitter.compile_program(program)?;
        
        asm.push_str("\n; ========== EXTENSION FUNCTIONS ==========\n; Note: Extension functions are supported via plugin system\n");
        
        Ok(asm)
    }
    
    pub fn compile(&mut self, source: &str) -> Result<CompilationResult, String> {
        let start_time = std::time::Instant::now();
        
        self.warnings.clear();
        self.errors.clear();
        self.symbol_table.clear();
        
        let mut program = match crate::lua_frontend::parse_program(source) {
            Ok(program) => program,
            Err(parse_errors) => {
                let error_messages: Vec<String> = parse_errors
                    .iter()
                    .map(|e| e.format_error(source))
                    .collect();
                return Err(format!("Parse errors:\n{}", error_messages.join("\n")));
            }
        };
        
        if self.config.optimize {
            for pass in &self.optimization_passes {
                if let Err(err) = pass.optimize(&mut program) {
                    self.warnings.push(format!("Optimization pass '{}' failed: {}", pass.name(), err));
                }
            }
        }
        
        let hardware_asm = if self.config.hardware_dsl_enabled {
            let mut dsl = self.hardware_dsl.take();
            if let Some(ref mut dsl_ref) = dsl {
                let result = self.collect_hardware_intrinsics(&program, dsl_ref);
                self.hardware_dsl = dsl;
                result?
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };
        
        let backend_module = self.create_backend_module(&program, hardware_asm);
        let target = self.config.target;
        let use_backend_registry = matches!(
            target,
            Target::Bios16 | Target::Bios32 | Target::Bios64 | 
            Target::Bios64Sse | Target::Bios64Avx | Target::Bios64Avx512
        );
        
        let has_extensions = self.statement_has_extension_call(&Statement::Expr(
            Expr::Var("dummy".to_string(), crate::parser::Span::single(crate::parser::Position::new(0, 0, 0)))
        ));
        
        let assembly_result = if has_extensions {
            self.compile_with_extensions(&program)
        } else if use_backend_registry {
            let backend_found = self.backend_registry.find_backend(&backend_module);
            
            match backend_found {
                Some(_backend) => {
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
            match self.config.target {
                Target::Linux64 => {
                    let mut backend = crate::backend::Linux64Backend::new();
                    backend.compile_program(&program)
                }
                _ => {
                    self.compile_with_emitter(&program)
                }
            }
        };
        
        let assembly = assembly_result?;
        
        let compilation_time = start_time.elapsed().as_millis();
        
        let stats = CompilationStats {
            lines_of_code: source.lines().count(),
            assembly_lines: assembly.lines().count(),
            variables_allocated: self.symbol_table.len(),
            functions_compiled: program.body.iter()
                .filter(|stmt| matches!(stmt, Statement::FunctionDef { .. }))
                .count(),
            hardware_intrinsics: 0, // No longer tracking hardware intrinsics separately
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
        
        match self.config.target {
            Target::Linux64 => emitter.set_target_linux(),
            Target::Bios16 => emitter.set_target_bios16(),
            Target::Bios32 => emitter.set_target_bios32(),
            Target::Bios64 => emitter.set_target_bios64(),
            Target::Bios64Sse => emitter.set_target_bios64_sse(),
            Target::Bios64Avx => emitter.set_target_bios64_avx(),
            Target::Bios64Avx512 => emitter.set_target_bios64_avx512(),
        }
        
        emitter.compile_program(program)
    }
    
    fn compile_with_backend(
        &mut self,
        program: &Program,
        _module: &BackendModule
    ) -> Result<String, String> {
        match self.config.target {
            Target::Bios16 => {
                let mut bios16_backend = crate::backend::Bios16Backend::new();
                bios16_backend.compile_program(program)
            }
            Target::Bios32 => {
                let mut bios32_backend = crate::backend::Bios32Backend::new();
                bios32_backend.compile_program(program)
            }
            Target::Bios64 | Target::Bios64Sse | Target::Bios64Avx | Target::Bios64Avx512 => {
                let mut backend_mut = crate::backend::Bios64Backend::new();
                
                backend_mut = match self.config.target {
                    Target::Bios64Sse => backend_mut.with_sse(),
                    Target::Bios64Avx => backend_mut.with_avx(),
                    Target::Bios64Avx512 => backend_mut.with_avx512(),
                    _ => backend_mut,
                };
                
                backend_mut.compile_program(program)
            }
            _ => {
                let mut backend_copy: Box<dyn Backend> = match self.config.target {
                    Target::Linux64 => Box::new(crate::backend::Linux64Backend::new()),
                    _ => return Err(format!("Unsupported target for backend compilation: {:?}", self.config.target)),
                };
                
                backend_copy.compile_program(program)
            }
        }
    }
    
    fn create_backend_module(&self, _program: &Program, _hardware_asm: Vec<String>) -> BackendModule {
        let mut required_capabilities = Vec::new();
        
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
        }
        
        BackendModule {
            functions: Vec::new(),
            globals: Vec::new(),
            required_capabilities,
        }
    }
    
    fn collect_hardware_intrinsics(
        &mut self,
        program: &Program,
        dsl: &mut HardwareDSL
    ) -> Result<Vec<String>, String> {
        let mut hardware_asm = Vec::new();
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
                if target.starts_with("hw_") || target.contains("_reg") {
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
                if func.starts_with("hw_") || func == "write_register" || func == "read_register" ||
                   func == "dma_transfer" || func == "port_in" || func == "port_out" {
                    
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
                    
                    if let Ok(asm) = dsl.parse_hardware_statement(&call_str) {
                        hardware_asm.extend(asm);
                    }
                }
                
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
    
    pub fn compile_to_file(&mut self, source: &str, output_path: &str) -> Result<(), String> {
        let result = self.compile(source)?;
        std::fs::write(output_path, result.assembly)
            .map_err(|e| format!("Failed to write output file: {}", e))?;
        Ok(())
    }
    
    pub fn register_extension_module(&mut self, module: Box<dyn EarthngModule>) {
        self.extension_registry.register_module(module);
    }
    
    pub fn extension_registry(&self) -> &ExtensionRegistry {
        &self.extension_registry
    }
    
    pub fn extension_registry_mut(&mut self) -> &mut ExtensionRegistry {
        &mut self.extension_registry
    }
}

struct ConstantFoldingPass;

impl OptimizationPass for ConstantFoldingPass {
    fn name(&self) -> &str {
        "constant_folding"
    }
    
    fn optimize(&self, _program: &mut Program) -> Result<(), String> {
        Ok(())
    }
}

struct DeadCodeEliminationPass;

impl OptimizationPass for DeadCodeEliminationPass {
    fn name(&self) -> &str {
        "dead_code_elimination"
    }
    
    fn optimize(&self, _program: &mut Program) -> Result<(), String> {
        Ok(())
    }
}

struct InlineExpansionPass;

impl OptimizationPass for InlineExpansionPass {
    fn name(&self) -> &str {
        "inline_expansion"
    }
    
    fn optimize(&self, _program: &mut Program) -> Result<(), String> {
        Ok(())
    }
}

pub fn compile(source: &str, target: Target) -> Result<CompilationResult, String> {
    let config = CompilerConfig::default().with_target(target);
    let mut compiler = EarthangCompiler::new(config);
    compiler.compile(source)
}

pub fn compile_with_hardware(source: &str, target: Target) -> Result<CompilationResult, String> {
    let config = CompilerConfig::default()
        .with_target(target)
        .with_hardware_dsl(true);
    
    let mut compiler = EarthangCompiler::new(config);
    compiler.compile(source)
}

pub fn compile_with_config(source: &str, config: CompilerConfig) -> Result<CompilationResult, String> {
    let mut compiler = EarthangCompiler::new(config);
    compiler.compile(source)
}

pub fn parse(source: &str) -> Result<Program, String> {
    crate::parser::parse_program(source)
        .map_err(|errors| {
            errors.iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        })
}

pub fn format_result(result: &CompilationResult) -> String {
    let mut output = String::new();
    
    if !result.warnings.is_empty() {
        output.push_str(&format!("Warnings ({}):\n", result.warnings.len()));
        for warning in &result.warnings {
            output.push_str(&format!("  - {}\n", warning));
        }
        output.push('\n');
    }
    
    if !result.errors.is_empty() {
        output.push_str(&format!("Errors ({}):\n", result.errors.len()));
        for error in &result.errors {
            output.push_str(&format!("  - {}\n", error));
        }
        output.push('\n');
    }
    
    output.push_str("Compilation Statistics:\n");
    output.push_str(&format!("  Source lines: {}\n", result.stats.lines_of_code));
    output.push_str(&format!("  Assembly lines: {}\n", result.stats.assembly_lines));
    output.push_str(&format!("  Variables allocated: {}\n", result.stats.variables_allocated));
    output.push_str(&format!("  Functions compiled: {}\n", result.stats.functions_compiled));
    output.push_str(&format!("  Compilation time: {}ms\n", result.stats.compilation_time_ms));
    
    output
}

pub fn compile_with_extensions(source: &str, target: Target) -> Result<CompilationResult, String> {
    let config = CompilerConfig::default().with_target(target);
    let mut compiler = EarthangCompiler::new(config);
    compiler.compile(source)
}