/*
    Copyright (C) 2026 Emanuel

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
*/
use std::collections::HashMap;
use std::path::PathBuf;
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
    pub verbose: bool,
    pub keep_assembly: bool,
    pub modules: Vec<String>,
    pub search_paths: Vec<PathBuf>,
}

impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            target: Target::Linux64,
            optimize: true,
            debug_info: false,
            include_stdlib: true,
            hardware_dsl_enabled: true,
            code_size_limit: None,
            verbose: false,
            keep_assembly: false,
            modules: Vec::new(),
            search_paths: vec![PathBuf::from("."), PathBuf::from("stdlib")],
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
    
    pub fn add_search_path<P: Into<PathBuf>>(mut self, path: P) -> Self {
        self.search_paths.push(path.into());
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

impl EarthangCompiler {
    pub fn new(config: CompilerConfig) -> Self {
        let hardware_dsl_enabled = config.hardware_dsl_enabled;
        let hardware_dsl = if hardware_dsl_enabled {
            Some(crate::dsl::init_hardware_dsl())
        } else {
            None
        };

        let mut compiler = Self {
            config,
            backend_registry: BackendRegistry::default_registry(),
            hardware_dsl,
            warnings: Vec::new(),
            errors: Vec::new(),
            symbol_table: HashMap::new(),
            current_function: None,
            optimization_passes: Vec::new(),
            extension_registry: ExtensionRegistry::new(),
        };
        
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
        
        // Since we only have Linux64 target, use if let
        if let Target::Linux64 = self.config.target {
            emitter.set_target_linux();
        }
        
        let mut asm = emitter.compile_program(program)?;
        
        asm.push_str("\n; ========== EXTENSION FUNCTIONS ==========\n");
        
        Ok(asm)
    }
    
    pub fn compile<P: AsRef<std::path::Path>>(&mut self, file_path: P) -> Result<CompilationResult, String> {
        let _start_time = std::time::Instant::now();
        
        self.warnings.clear();
        self.errors.clear();
        self.symbol_table.clear();
        
        let file_path = file_path.as_ref();
        let source = std::fs::read_to_string(file_path)
            .map_err(|e| format!("Failed to read file {}: {}", file_path.display(), e))?;
        
        self.compile_source(&source, Some(file_path))
    }
    
    pub fn compile_source(&mut self, source: &str, source_path: Option<&std::path::Path>) -> Result<CompilationResult, String> {
        let start_time = std::time::Instant::now();
        
        self.warnings.clear();
        self.errors.clear();
        self.symbol_table.clear();
        
        let mut include_processor = crate::lua_frontend::IncludeProcessor::new();
        for path in &self.config.search_paths {
            include_processor.add_search_path(path);
        }
        
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
        
        let base_dir = source_path.and_then(|p| p.parent().map(|p| p.to_path_buf()));
        program = include_processor.process_includes(&program, base_dir.as_ref())
            .map_err(|e| format!("Include processing error: {}", e))?;
        
        if self.config.optimize {
            for pass in &self.optimization_passes {
                if let Err(err) = pass.optimize(&mut program) {
                    self.warnings.push(format!("Optimization pass '{}' failed: {}", pass.name(), err));
                }
            }
        }
        
        // Create backend with hardware DSL if enabled
        let _backend_module = self.create_backend_module(&program);
        
        let assembly_result = match self.config.target {
            Target::Linux64 => {
                let mut backend = crate::backend::Linux64Backend::new();
                
                // Pass hardware DSL to backend if enabled
                if self.config.hardware_dsl_enabled {
                    if let Some(dsl) = self.hardware_dsl.take() {
                        backend = backend.with_hardware_dsl(dsl);
                    }
                }
                
                backend.compile_program(&program)
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
            hardware_intrinsics: program.body.iter()
                .filter(|stmt| matches!(stmt, Statement::HardwareFunctionDef { .. }))
                .count(),
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
        
        // Since we only have Linux64 target, use if let
        if let Target::Linux64 = self.config.target {
            emitter.set_target_linux();
        }
        
        emitter.compile_program(program)
    }
    
    fn compile_with_backend(
        &mut self,
        program: &Program,
        _module: &BackendModule
    ) -> Result<String, String> {
        match self.config.target {
            Target::Linux64 => {
                let mut backend = crate::backend::Linux64Backend::new();
                
                // Pass hardware DSL to backend if enabled
                if self.config.hardware_dsl_enabled {
                    if let Some(dsl) = self.hardware_dsl.take() {
                        backend = backend.with_hardware_dsl(dsl);
                    }
                }
                
                backend.compile_program(program)
            }
        }
    }
    
    fn create_backend_module(&self, _program: &Program) -> BackendModule {
        let mut required_capabilities = Vec::new();
        
        match self.config.target {
            Target::Linux64 => {
                required_capabilities.push(Capability::Linux);
                required_capabilities.push(Capability::LongMode64);
                required_capabilities.push(Capability::VirtualMemory);
                
                // Add hardware capabilities if DSL is enabled
                if self.config.hardware_dsl_enabled {
                    required_capabilities.push(Capability::Graphics);
                }
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
    
    pub fn compile_to_file<P: AsRef<std::path::Path>>(&mut self, source_path: P, output_path: P) -> Result<(), String> {
        let result = self.compile(source_path)?;
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

pub fn compile<P: AsRef<std::path::Path>>(source_path: P, target: Target) -> Result<CompilationResult, String> {
    let config = CompilerConfig::default().with_target(target);
    let mut compiler = EarthangCompiler::new(config);
    compiler.compile(source_path)
}

pub fn compile_with_hardware<P: AsRef<std::path::Path>>(source_path: P, target: Target) -> Result<CompilationResult, String> {
    let config = CompilerConfig::default()
        .with_target(target)
        .with_hardware_dsl(true);
    
    let mut compiler = EarthangCompiler::new(config);
    compiler.compile(source_path)
}

pub fn compile_with_config<P: AsRef<std::path::Path>>(source_path: P, config: CompilerConfig) -> Result<CompilationResult, String> {
    let mut compiler = EarthangCompiler::new(config);
    compiler.compile(source_path)
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
    output.push_str(&format!("  Hardware intrinsics: {}\n", result.stats.hardware_intrinsics));
    output.push_str(&format!("  Compilation time: {}ms\n", result.stats.compilation_time_ms));
    
    output
}

pub fn compile_with_extensions<P: AsRef<std::path::Path>>(source_path: P, target: Target) -> Result<CompilationResult, String> {
    let config = CompilerConfig::default().with_target(target);
    let mut compiler = EarthangCompiler::new(config);
    compiler.compile(source_path)
}