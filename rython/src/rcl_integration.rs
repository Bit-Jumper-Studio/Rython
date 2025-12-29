
use std::path::Path;
use std::fs;
use crate::parser::parse_program;
use crate::compiler::{RythonCompiler, CompilerConfig, Target};
use crate::modules::ModuleRegistry;
use crate::rcl_compiler::{RclImportManager, RclCompiler, create_test_library};
use serde_json;

/// Extended Rython compiler with RCL support
pub struct RythonCompilerWithRcl {
    base_config: CompilerConfig,
    rcl_manager: RclImportManager,
    rcl_mode: bool,
    include_rcl_assembly: bool,
    module_registry: ModuleRegistry,
}

impl RythonCompilerWithRcl {
    pub fn new(config: CompilerConfig) -> Self {
        Self {
            base_config: config,
            rcl_manager: RclImportManager::new(),
            rcl_mode: false,
            include_rcl_assembly: true,
            module_registry: ModuleRegistry::default_registry(),
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
    
    /// Compile with RCL support (now includes module conversion to RCL)
    pub fn compile_with_rcl(&mut self, source: &str, output_path: &str) -> Result<(), String> {
        if self.base_config.verbose {
            println!("[RCL] Compiling with RCL support...");
        }
        
        // Parse source
        let program = parse_program(source)
            .map_err(|e| format!("Parse error: {:?}", e))?;
        
        // Extract required modules
        let required_modules = self.module_registry.extract_required_modules(&program);
        
        if !required_modules.is_empty() && self.base_config.verbose {
            println!("[RCL] Detected modules: {:?}", required_modules);
        }
        
        // Create RCL libraries for modules if in RCL mode
        if self.rcl_mode {
            for module_name in &required_modules {
                if let Some(module) = self.module_registry.get_module(module_name) {
                    let rcl_lib = module.to_rcl_library(&self.base_config.target.to_string());
                    let json = serde_json::to_string_pretty(&rcl_lib)
                        .map_err(|e| format!("Failed to serialize RCL: {}", e))?;
                    
                    let rcl_file = format!("{}.rcl", module_name);
                    fs::write(&rcl_file, &json)
                        .map_err(|e| format!("Failed to write RCL file: {}", e))?;
                    
                    if self.base_config.verbose {
                        println!("[RCL] Created RCL library: {}", rcl_file);
                    }
                    
                    // Import the generated RCL
                    self.rcl_manager.import_library(&rcl_file)?;
                }
            }
        }
        
        // Use base compiler for the main compilation
        let mut base_compiler = RythonCompiler::new(self.base_config.clone());
        
        // Pass the required modules to the base compiler
        base_compiler.config_mut().modules = required_modules.clone();
        
        // Compile the main program
        base_compiler.compile(source, output_path)?;

        // If RCL mode is enabled and we have RCL assembly to include
        if self.rcl_mode && self.include_rcl_assembly {
            let extra_asm = self.generate_rcl_assembly();
            if !extra_asm.is_empty() {
                // Append RCL assembly to the binary
                self.append_rcl_to_binary(output_path, &extra_asm)?;
                    
                if self.base_config.verbose {
                    println!("[RCL] Added RCL assembly to binary");
                }
            }
        }

        if self.base_config.verbose {
            println!("[RCL] Compilation successful!");
        }
        Ok(())
    }

    /// Append RCL assembly to binary as a data section
    fn append_rcl_to_binary(&self, output_path: &str, assembly: &str) -> Result<(), String> {
        // Read the original binary
        let mut binary = fs::read(output_path)
            .map_err(|e| format!("Failed to read binary: {}", e))?;
        
        // For BIOS targets, append as data after boot sector
        if matches!(self.base_config.target, 
            Target::Bios16 | Target::Bios32 | Target::Bios64 | 
            Target::Bios64Sse | Target::Bios64Avx | Target::Bios64Avx512) {
            
            // Ensure we're not overwriting boot signature
            if binary.len() >= 510 {
                // Pad to 510 bytes if needed
                while binary.len() < 510 {
                    binary.push(0);
                }
                // Keep boot signature
                if binary.len() == 510 {
                    binary.push(0x55);
                    binary.push(0xAA);
                }
                
                // Append RCL data
                binary.extend(b"RCL_DATA");
                binary.extend(assembly.as_bytes());
            }
        } else {
            // For other targets, just append
            binary.extend(b"RCL_DATA");
            binary.extend(assembly.as_bytes());
        }
        
        // Write back
        fs::write(output_path, binary)
            .map_err(|e| format!("Failed to write final binary: {}", e))
    }

    /// Generate assembly from all loaded RCL libraries
    fn generate_rcl_assembly(&self) -> String {
        use crate::rcl_compiler::RclEntry;
        
        let mut asm = String::new();
        for lib in self.rcl_manager.get_loaded_libraries() {
            asm.push_str(&format!("; RCL Library: {}\n", lib.metadata.name));
            for entry in &lib.entries {
                match entry {
                    RclEntry::Assembly(assm) => {
                        asm.push_str(&assm.code);
                        asm.push_str("\n");
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
        modules: Vec::new(),
    };
    
    let mut compiler = RythonCompilerWithRcl::new(config);
    compiler.enable_rcl();
    compiler.compile_with_rcl(source, output_path)
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
        
        // Read source
        let source = std::fs::read_to_string(source_file)
            .map_err(|e| format!("Failed to read source: {}", e))?;
        
        // Parse
        let program = parse_program(&source)
            .map_err(|e| format!("Parse error: {:?}", e))?;
        
        // Extract library name from filename
        let lib_name = Path::new(source_file)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("library")
            .to_string();
        
        // Create RCL compiler with real compilation
        let mut rcl_compiler = RclCompiler::new(&lib_name, target);
        rcl_compiler.compile_program(&program)?;
        
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
        use crate::rcl_compiler::RclEntry;
        
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
                println!("║   • {}", export);
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
        use crate::rcl_compiler::RclEntry;
        
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
        use crate::rcl_compiler::RclEntry;
        
        let library = RclCompiler::load_from_file(rcl_file)?;
        
        println!("Functions in {}:", library.metadata.name);
        println!("{:<20} {:<10} {:<8} {:<6}", "Name", "Params", "Inline", "Pure");
        println!("{:-<60}", "");
        
        let mut functions_found = false;
        for entry in &library.entries {
            if let RclEntry::Function(func) = entry {
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
        let library = create_test_library();
        
        let json = serde_json::to_string_pretty(&library)
            .map_err(|e| format!("Failed to serialize: {}", e))?;
        
        std::fs::write(output_file, json)
            .map_err(|e| format!("Failed to write: {}", e))?;
        
        println!("Created test library with real functions: {}", output_file);
        Ok(())
    }
}