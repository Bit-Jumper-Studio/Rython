use std::path::Path;
use crate::parser::parse_program;
use crate::compiler::{EarthngCompiler, CompilerConfig};
use crate::rcl_compiler::{RclCompiler, RclAssemblyGenerator};

// Update the import
use crate::backend::Target;

/// Extended Rython compiler with RCL support
pub struct EarthngCompilerWithRcl {
    base_config: CompilerConfig,
    rcl_mode: bool,
}

impl EarthngCompilerWithRcl {
    pub fn new(config: CompilerConfig) -> Self {
        Self {
            base_config: config,
            rcl_mode: false,
        }
    }
    
    pub fn enable_rcl(&mut self) {
        self.rcl_mode = true;
    }
    
    pub fn compile_with_rcl(&mut self, source: &str, output_path: &str) -> Result<(), String> {
        if self.base_config.verbose {
            println!("[RCL] Compiling with RCL support...");
        }
        
        let mut base_compiler = EarthngCompiler::new(self.base_config.clone());
        base_compiler.config.enable_rcl = true;
        
        // Use the new compile_to_file method
        base_compiler.compile_to_file(source, output_path)?;

        if self.base_config.verbose {
            println!("[RCL] Compilation successful!");
        }
        Ok(())
    }
}

/// Public API functions for RCL
pub fn compile_with_rcl(source: &str, output_path: &str, target: Target) -> Result<(), String> {
    let config = CompilerConfig {
        target,
        verbose: true,
        keep_assembly: true,
        optimize: true,
        modules: Vec::new(),
        ssd_headers: Vec::new(),
        ssd_assembly: Vec::new(),
        enable_ssd: false,
        enable_rcl: true,
        rcl_libraries: Vec::new(),
        debug_info: false,
        include_stdlib: false,
        hardware_dsl_enabled: false,
        code_size_limit: None,
    };
    
    let mut compiler = EarthngCompiler::new(config);
    compiler.compile_to_file(source, output_path)
}

pub fn create_rcl_library(source: &str, output_file: &str, target: &str) -> Result<(), String> {
    let cli = RclCli::new(true);
    cli.compile_to_rcl(source, output_file, target)
}

// Command-line interface for RCL operations
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
        
        let source = std::fs::read_to_string(source_file)
            .map_err(|e| format!("Failed to read source: {}", e))?;
        
        let program = parse_program(&source)
            .map_err(|e| format!("Parse error: {:?}", e))?;
        
        let lib_name = Path::new(source_file)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("library")
            .to_string();
        
        let mut rcl_compiler = RclCompiler::new(&lib_name, target);
        rcl_compiler.compile_program(&program)?;
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
        
        println!("╠══════════════════════════════════════════════╣");
        println!("║ Exports: {} symbols", library.metadata.exports.len());
        
        if !library.metadata.exports.is_empty() && self.verbose {
            for export in &library.metadata.exports {
                println!("║   • {}", export);
            }
        }
        
        println!("╠══════════════════════════════════════════════╣");
        println!("║ Entries: {} total", library.entries.len());
        
        if self.verbose {
            let mut func_count = 0;
            let mut var_count = 0;
            
            for entry in &library.entries {
                match entry {
                    crate::rcl_compiler::RclEntry::Function(_) => func_count += 1,
                    crate::rcl_compiler::RclEntry::Variable(_) => var_count += 1,
                    _ => {}
                }
            }
            
            println!("║   • Functions: {}", func_count);
            println!("║   • Variables: {}", var_count);
        }
        
        println!("╚══════════════════════════════════════════════╝");
        
        Ok(())
    }
    
    /// Extract assembly from RCL library
    pub fn extract_assembly(&self, rcl_file: &str, function_name: &str) -> Result<(), String> {
        let library = RclCompiler::load_from_file(rcl_file)?;
        
        for entry in &library.entries {
            if let crate::rcl_compiler::RclEntry::Function(func) = entry {
                if func.name == function_name {
                    let generator = RclAssemblyGenerator::new(&library.metadata.target);
                    let assembly = generator.generate_library_assembly(&library);
                    
                    println!("; Assembly for {} from {}", function_name, rcl_file);
                    println!("; Target: {}", library.metadata.target);
                    println!();
                    println!("{}", assembly);
                    return Ok(());
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
        
        let mut functions_found = false;
        for entry in &library.entries {
            if let crate::rcl_compiler::RclEntry::Function(func) = entry {
                functions_found = true;
                let param_count = func.parameters.len();
                println!("{:<20} {:<10} {:<8} {:<6}", 
                    func.name, 
                    param_count,
                    func.inlineable,
                    func.pure);
            }
        }
        
        if !functions_found {
            println!("No functions found in this library");
        }
        
        Ok(())
    }
    
    /// Create a test RCL library with real functions
    pub fn create_test_library(&self, output_file: &str) -> Result<(), String> {
        let library = crate::rcl_compiler::create_test_library();
        
        let file = std::fs::File::create(output_file)
            .map_err(|e| format!("Failed to create file: {}", e))?;
        
        let writer = std::io::BufWriter::new(file);
        serde_json::to_writer_pretty(writer, &library)
            .map_err(|e| format!("Failed to serialize: {}", e))?;
        
        println!("Created test library with real functions: {}", output_file);
        Ok(())
    }
}