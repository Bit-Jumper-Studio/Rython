// ==================== RCL INTEGRATION WITH RYTHON COMPILER ====================

use std::path::Path;
use crate::parser::{Program, Statement, Expr};
use crate::compiler::{RythonCompiler, CompilerConfig, Target};
use crate::emitter::NasmEmitter;
use crate::backend::{Backend, BackendRegistry, Bios64Backend, BackendModule, Capability};
use super::rcl_compiler::{RclCompiler, RclImportManager, RclLibrary, RclEntry};

/// Extended Rython compiler with RCL support
pub struct RythonCompilerWithRcl {
    base_config: CompilerConfig,
    rcl_manager: RclImportManager,
    rcl_mode: bool,
    include_rcl_assembly: bool,
}

impl RythonCompilerWithRcl {
    pub fn new(config: CompilerConfig) -> Self {
        Self {
            base_config: config,
            rcl_manager: RclImportManager::new(),
            rcl_mode: false,
            include_rcl_assembly: true,
        }
    }
    
    /// Enable RCL mode
    pub fn enable_rcl(&mut self) {
        self.rcl_mode = true;
    }
    
    /// Disable including RCL assembly in output
    pub fn exclude_rcl_assembly(&mut self) {
        self.include_rcl_assembly = false;
    }
    
    /// Add RCL import path
    pub fn add_rcl_path(&mut self, path: &str) {
        self.rcl_manager.add_import_path(path);
    }
    
    /// Pre-load RCL libraries
    pub fn preload_libraries(&mut self, libraries: &[&str]) -> Result<(), String> {
        for lib in libraries {
            self.rcl_manager.import_library(lib)?;
        }
        Ok(())
    }
    
    /// Compile Rython code with RCL support
    pub fn compile_with_rcl(&mut self, source: &str, output_path: &str) -> Result<(), String> {
        println!("[RCL] Compiling with RCL support...");
        
        // 1. Parse source code
        let program = crate::parser::parse_program(source)
            .map_err(|e| format!("Parse error: {}", e))?;
        
        // 2. Extract and process imports
        let (program_without_imports, imported_libs) = self.extract_imports(&program)?;
        
        // 3. Load imported libraries
        for lib_name in &imported_libs {
            self.rcl_manager.import_library(lib_name)?;
        }
        
        // 4. Select backend
        let module = self.create_backend_module(&program_without_imports)?;
        let mut backend = self.select_backend(&module)?;
        
        // 5. Generate base assembly
        let mut asm = backend.compile_program(&program_without_imports)
            .map_err(|e| format!("Backend failed: {}", e))?;
        
        // 6. Append RCL assembly if needed
        if self.include_rcl_assembly {
            asm = self.append_rcl_assembly(asm);
        }
        
        // 7. Write and assemble
        let asm_file = format!("{}.asm", output_path);
        std::fs::write(&asm_file, &asm)
            .map_err(|e| format!("Failed to write assembly: {}", e))?;
        
        // 8. Assemble based on target
        self.assemble_based_on_target(&asm_file, output_path)?;
        
        // 9. Cleanup
        if !self.base_config.keep_assembly {
            std::fs::remove_file(&asm_file)
                .map_err(|e| format!("Failed to remove assembly file: {}", e))?;
        }
        
        println!("[RCL] Compilation successful!");
        Ok(())
    }
    
    fn extract_imports(&self, program: &Program) -> Result<(Program, Vec<String>), String> {
        let mut filtered_statements = Vec::new();
        let mut imported_libs = Vec::new();
        
        for stmt in &program.body {
            match stmt {
                Statement::Expr(Expr::Call { func, args, .. }) if func == "import" => {
                    if let Some(Expr::String(lib_name)) = args.get(0) {
                        imported_libs.push(lib_name.clone());
                    }
                }
                Statement::Expr(Expr::Call { func, args, .. }) if func == "from" => {
                    if let (Some(Expr::String(lib_name)), Some(Expr::Call { func: import_func, args: import_args, .. })) = 
                        (args.get(0), args.get(1)) {
                        if import_func == "import" {
                            if let Some(Expr::String(_symbol)) = import_args.get(0) {
                                imported_libs.push(lib_name.clone());
                                // Store the symbol for later use
                            }
                        }
                    }
                }
                _ => {
                    filtered_statements.push(stmt.clone());
                }
            }
        }
        
        Ok((Program { body: filtered_statements }, imported_libs))
    }
    
    fn create_backend_module(&self, _program: &Program) -> Result<BackendModule, String> {
        // Create a simple module with required capabilities
        let mut required_capabilities = Vec::new();
        
        match self.base_config.target {
            Target::Bios16 => {
                required_capabilities.push(Capability::BIOS);
                required_capabilities.push(Capability::RealMode16);
            }
            Target::Bios32 => {
                required_capabilities.push(Capability::BIOS);
                required_capabilities.push(Capability::ProtectedMode32);
            }
            Target::Bios64 => {
                required_capabilities.push(Capability::BIOS);
                required_capabilities.push(Capability::LongMode64);
            }
            Target::Bios64Sse => {
                required_capabilities.push(Capability::BIOS);
                required_capabilities.push(Capability::LongMode64);
                required_capabilities.push(Capability::SSE);
            }
            Target::Bios64Avx => {
                required_capabilities.push(Capability::BIOS);
                required_capabilities.push(Capability::LongMode64);
                required_capabilities.push(Capability::AVX);
            }
            Target::Bios64Avx512 => {
                required_capabilities.push(Capability::BIOS);
                required_capabilities.push(Capability::LongMode64);
                required_capabilities.push(Capability::AVX512);
            }
            Target::Linux64 => {
                required_capabilities.push(Capability::Linux);
                required_capabilities.push(Capability::LongMode64);
            }
            Target::Windows64 => {
                required_capabilities.push(Capability::Windows);
                required_capabilities.push(Capability::LongMode64);
            }
        }
        
        Ok(BackendModule {
            functions: Vec::new(),
            globals: Vec::new(),
            required_capabilities,
        })
    }
    
    fn select_backend(&self, module: &BackendModule) -> Result<Box<dyn Backend>, String> {
        let registry = BackendRegistry::default_registry();
        
        // Find compatible backend
        for backend in &registry.backends {
            if backend.can_compile(module) {
                match backend.name() {
                    "bios64" => {
                        let mut bios_backend = Bios64Backend::new();
                        
                        // Enable extensions if requested
                        if module.required_capabilities.contains(&Capability::SSE) {
                            bios_backend = bios_backend.with_sse();
                        }
                        if module.required_capabilities.contains(&Capability::AVX) {
                            bios_backend = bios_backend.with_avx();
                        }
                        
                        return Ok(Box::new(bios_backend));
                    }
                    _ => continue,
                }
            }
        }
        
        Err("No compatible backend found".to_string())
    }
    
    fn append_rcl_assembly(&self, base_asm: String) -> String {
        let mut final_asm = base_asm;
        
        // Get all loaded libraries
        let libraries = self.rcl_manager.get_loaded_libraries();
        
        if !libraries.is_empty() {
            final_asm.push_str("\n; ========== RCL LIBRARIES ==========\n\n");
            
            for library in libraries {
                // Only include libraries for current target
                let target_str = match self.base_config.target {
                    Target::Bios16 => "bios16",
                    Target::Bios32 => "bios32",
                    Target::Bios64 => "bios64",
                    Target::Bios64Sse => "bios64",
                    Target::Bios64Avx => "bios64",
                    Target::Bios64Avx512 => "bios64",
                    Target::Linux64 => "linux64",
                    Target::Windows64 => "windows64",
                };
                
                if library.metadata.target == target_str {
                    final_asm.push_str(&format!("; Library: {}\n", library.metadata.name));
                    
                    for entry in &library.entries {
                        match entry {
                            RclEntry::Function(func) => {
                                if let Some(asm_code) = &func.assembly {
                                    final_asm.push_str(asm_code);
                                    final_asm.push_str("\n");
                                }
                            }
                            RclEntry::Assembly(asm_entry) if asm_entry.target == target_str => {
                                final_asm.push_str(&asm_entry.code);
                                final_asm.push_str("\n");
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
        
        final_asm
    }
    
    fn assemble_based_on_target(&self, asm_file: &str, output_path: &str) -> Result<(), String> {
        // Use existing compiler for assembly
        let base_compiler = RythonCompiler::new(self.base_config.clone());
        
        match self.base_config.target {
            Target::Bios16 | Target::Bios32 | Target::Bios64 | 
            Target::Bios64Sse | Target::Bios64Avx | Target::Bios64Avx512 => 
                base_compiler.assemble_bios(asm_file, output_path),
            Target::Linux64 => base_compiler.assemble_linux(asm_file, output_path),
            Target::Windows64 => base_compiler.assemble_windows(asm_file, output_path),
        }
    }
}

/// Command-line interface for RCL operations
pub struct RclCli {
    pub verbose: bool,
}

impl RclCli {
    pub fn new(verbose: bool) -> Self {
        Self { verbose }
    }
    
    /// Compile Rython source to RCL library
    pub fn compile_to_rcl(&self, source_file: &str, output_file: &str, target: &str) -> Result<(), String> {
        if self.verbose {
            println!("[RCL] Compiling {} to RCL library...", source_file);
        }
        
        // Read source
        let source = std::fs::read_to_string(source_file)
            .map_err(|e| format!("Failed to read source: {}", e))?;
        
        // Parse
        let program = crate::parser::parse_program(&source)
            .map_err(|e| format!("Parse error: {}", e))?;
        
        // Extract library name from filename
        let lib_name = Path::new(source_file)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("library")
            .to_string();
        
        // Create RCL compiler
        let mut rcl_compiler = RclCompiler::new(&lib_name, target);
        rcl_compiler.export_all();
        
        // Compile to RCL
        rcl_compiler.compile_program(&program)?;
        rcl_compiler.finalize()?;
        
        // Save to file
        rcl_compiler.save_to_file(output_file)?;
        
        println!("[RCL] Successfully created RCL library: {}", output_file);
        
        if self.verbose {
            println!("[RCL] Library exports:");
            for export in &rcl_compiler.library.metadata.exports {
                println!("  - {}", export);
            }
        }
        
        Ok(())
    }
    
    /// Display RCL library info
    pub fn show_rcl_info(&self, rcl_file: &str) -> Result<(), String> {
        let library = RclCompiler::load_from_file(rcl_file)?;
        
        println!("╔══════════════════════════════════════════════╗");
        println!("║            RCL Library Information           ║");
        println!("╠══════════════════════════════════════════════╣");
        println!("║ Library: {}", format!("{: <36}", library.metadata.name).trim_end());
        println!("║ Version: {}", format!("{: <36}", library.metadata.version).trim_end());
        println!("║ Target:  {}", format!("{: <36}", library.metadata.target).trim_end());
        println!("║ RCL Ver: {}", format!("{: <36}", library.metadata.rcl_version).trim_end());
        
        if let Some(author) = &library.metadata.author {
            println!("║ Author:  {}", format!("{: <36}", author).trim_end());
        }
        
        if let Some(desc) = &library.metadata.description {
            println!("║ Desc:    {}", format!("{: <36}", desc).trim_end());
        }
        
        println!("╠══════════════════════════════════════════════╣");
        println!("║ Exports: {} symbols", library.metadata.exports.len());
        
        if !library.metadata.exports.is_empty() && self.verbose {
            for export in &library.metadata.exports {
                if let Some(entry_type) = library.symbol_table.get(export) {
                    println!("║   • {} ({})", export, entry_type);
                }
            }
        }
        
        if !library.metadata.dependencies.is_empty() {
            println!("╠══════════════════════════════════════════════╣");
            println!("║ Dependencies: {}", library.metadata.dependencies.len());
            if self.verbose {
                for dep in &library.metadata.dependencies {
                    println!("║   • {}", dep);
                }
            }
        }
        
        println!("╠══════════════════════════════════════════════╣");
        println!("║ Entries: {} total", library.entries.len());
        
        if self.verbose {
            let mut func_count = 0;
            let mut var_count = 0;
            let mut const_count = 0;
            let mut type_count = 0;
            let mut asm_count = 0;
            
            for entry in &library.entries {
                match entry {
                    RclEntry::Function(_) => func_count += 1,
                    RclEntry::Variable(_) => var_count += 1,
                    RclEntry::Constant(_) => const_count += 1,
                    RclEntry::Type(_) => type_count += 1,
                    RclEntry::Assembly(_) => asm_count += 1,
                }
            }
            
            println!("║   • Functions: {}", func_count);
            println!("║   • Variables: {}", var_count);
            println!("║   • Constants: {}", const_count);
            println!("║   • Types:     {}", type_count);
            println!("║   • Assembly:  {}", asm_count);
        }
        
        println!("╚══════════════════════════════════════════════╝");
        
        Ok(())
    }
    
    /// Extract assembly from RCL library
    pub fn extract_assembly(&self, rcl_file: &str, function_name: &str) -> Result<(), String> {
        let library = RclCompiler::load_from_file(rcl_file)?;
        
        // Find the function
        for entry in &library.entries {
            if let RclEntry::Function(func) = entry {
                if func.name == function_name {
                    if let Some(assembly) = &func.assembly {
                        println!("; Assembly for {} from {}", function_name, rcl_file);
                        println!("; Target: {}", library.metadata.target);
                        println!("; Signature: {}", func.signature);
                        println!("; Inlineable: {}, Pure: {}", func.inlineable, func.pure);
                        println!();
                        println!("{}", assembly);
                        return Ok(());
                    } else {
                        return Err(format!("No assembly available for {}", function_name));
                    }
                }
            }
        }
        
        Err(format!("Function '{}' not found in library", function_name))
    }
    
    /// List all functions in RCL library
    pub fn list_functions(&self, rcl_file: &str) -> Result<(), String> {
        let library = RclCompiler::load_from_file(rcl_file)?;
        
        println!("Functions in {}:", library.metadata.name);
        println!("{:<20} {:<10} {:<8} {:<6}", "Name", "Params", "Inline", "Pure");
        println!("{:-<60}", "");
        
        for entry in &library.entries {
            if let RclEntry::Function(func) = entry {
                let param_count = func.parameters.len();
                println!("{:<20} {:<10} {:<8} {:<6}", 
                    func.name, 
                    param_count,
                    func.inlineable,
                    func.pure);
            }
        }
        
        Ok(())
    }
    
    /// Create a test RCL library
    pub fn create_test_library(&self, output_file: &str) -> Result<(), String> {
        let test_lib = super::rcl_compiler::create_test_library();
        
        let rcl_text = serde_json::to_string_pretty(&test_lib)
            .map_err(|e| format!("Failed to serialize: {}", e))?;
        
        std::fs::write(output_file, rcl_text)
            .map_err(|e| format!("Failed to write: {}", e))?;
        
        println!("Created test library: {}", output_file);
        Ok(())
    }
}

/// Integration with existing emitter for RCL support
pub trait RclEmitter {
    fn emit_with_rcl(&mut self, program: &Program, rcl_libs: &[RclLibrary]) -> String;
}

impl RclEmitter for NasmEmitter {
    fn emit_with_rcl(&mut self, program: &Program, rcl_libs: &[RclLibrary]) -> String {
        let mut asm = self.compile_program(program);
        
        // Append RCL library assembly
        for lib in rcl_libs {
            asm.push_str(&format!("; RCL Library: {}\n", lib.metadata.name));
            
            for entry in &lib.entries {
                match entry {
                    RclEntry::Assembly(assembly) => {
                        if assembly.target == self.target.format {
                            asm.push_str(&format!("; Assembly from RCL: {}\n", assembly.label));
                            asm.push_str(&assembly.code);
                            asm.push_str("\n");
                        }
                    }
                    RclEntry::Function(func) => {
                        if let Some(func_asm) = &func.assembly {
                            asm.push_str(&format!("; Function from RCL: {}\n", func.name));
                            asm.push_str(func_asm);
                            asm.push_str("\n");
                        }
                    }
                    _ => {}
                }
            }
        }
        
        asm
    }
}

/// Public API functions for RCL
pub fn compile_with_rcl(source: &str, output_path: &str, target: Target) -> Result<(), String> {
    let config = CompilerConfig {
        target,
        verbose: true,
        keep_assembly: true,
        optimize: true,
    };
    
    let mut compiler = RythonCompilerWithRcl::new(config);
    compiler.enable_rcl();
    compiler.compile_with_rcl(source, output_path)
}

pub fn create_rcl_library(source: &str, output_file: &str, target: &str) -> Result<(), String> {
    let cli = RclCli::new(true);
    cli.compile_to_rcl(source, output_file, target)
}