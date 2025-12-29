// ==================== COMPILER ====================

use std::fs;
use std::process::Command;
use crate::backend::{Backend, BackendRegistry, BackendModule, Capability};
use crate::modules::ModuleRegistry;
use crate::parser::{Program, parse_program, Statement, format_parse_errors};
use crate::utils::{find_nasm, find_linker};
use crate::linker::manual_link;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Target {
    Bios16,     // 512-byte bootloader (16-bit real mode)
    Bios32,     // 32-bit protected mode bootloader
    Bios64,     // 64-bit long mode bootloader
    Bios64Sse,  // 64-bit with SSE enabled
    Bios64Avx,  // 64-bit with AVX enabled
    Bios64Avx512, // 64-bit with AVX-512 enabled
    Linux64,    // Linux ELF64
    Windows64,  // Windows PE64
}

impl Target {
    pub fn to_string(&self) -> String {
        match self {
            Target::Bios16 => "bios16".to_string(),
            Target::Bios32 => "bios32".to_string(),
            Target::Bios64 => "bios64".to_string(),
            Target::Bios64Sse => "bios64_sse".to_string(),
            Target::Bios64Avx => "bios64_avx".to_string(),
            Target::Bios64Avx512 => "bios64_avx512".to_string(),
            Target::Linux64 => "linux64".to_string(),
            Target::Windows64 => "windows64".to_string(),
        }
    }
    
    pub fn get_format(&self) -> &'static str {
        match self {
            Target::Bios16 | Target::Bios32 | Target::Bios64 | 
            Target::Bios64Sse | Target::Bios64Avx | Target::Bios64Avx512 => "bin",
            Target::Linux64 => "elf64",
            Target::Windows64 => "win64",
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompilerConfig {
    pub target: Target,
    pub verbose: bool,
    pub keep_assembly: bool,
    pub optimize: bool,
    pub modules: Vec<String>, // List of modules to include (e.g., from CLI)
}

impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            target: Target::Bios64,
            verbose: false,
            keep_assembly: false,
            optimize: true,
            modules: Vec::new(),
        }
    }
}

pub struct RythonCompiler {
    config: CompilerConfig,
    backend_registry: BackendRegistry,
    module_registry: ModuleRegistry,
}

impl RythonCompiler {
    pub fn new(config: CompilerConfig) -> Self {
        Self {
            config,
            backend_registry: BackendRegistry::default_registry(),
            module_registry: ModuleRegistry::default_registry(),
        }
    }
    
    pub fn config(&self) -> &CompilerConfig {
        &self.config
    }
    
    pub fn config_mut(&mut self) -> &mut CompilerConfig {
        &mut self.config
    }

    pub fn compile(&mut self, source: &str, output_path: &str) -> Result<(), String> {
        // Parse the source code
        let program = parse_program(source)
            .map_err(|errors| format_parse_errors(&errors, source))?;

        if self.config.verbose {
            println!("[Rython] Parsed AST successfully");
        }
        
        // Extract required modules
        let required_modules = self.extract_required_modules(&program);
        
        // Combine with config.modules
        let all_modules = [&self.config.modules[..], &required_modules[..]].concat();
        
        if !all_modules.is_empty() && self.config.verbose {
            println!("[Rython] Required modules: {:?}", all_modules);
        }

        // Create BackendModule with capabilities from selected modules
        let mut backend_module = BackendModule {
            functions: Vec::new(),
            globals: Vec::new(),
            required_capabilities: Vec::new(),
        };

        // Add capabilities from modules
        for module_name in &all_modules {
            if let Some(module) = self.module_registry.get_module(module_name) {
                backend_module.required_capabilities.extend(module.get_required_capabilities());
            }
        }
        
        // Add target-specific capabilities
        match self.config.target {
            Target::Bios16 => backend_module.required_capabilities.push(Capability::BIOS),
            Target::Bios32 => backend_module.required_capabilities.push(Capability::BIOS),
            Target::Bios64 => backend_module.required_capabilities.push(Capability::BIOS),
            Target::Bios64Sse => backend_module.required_capabilities.push(Capability::BIOS),
            Target::Bios64Avx => backend_module.required_capabilities.push(Capability::BIOS),
            Target::Bios64Avx512 => backend_module.required_capabilities.push(Capability::BIOS),
            Target::Linux64 => backend_module.required_capabilities.push(Capability::Linux),
            Target::Windows64 => backend_module.required_capabilities.push(Capability::Windows),
        }

        // Find suitable backend based on target and capabilities
        let backend = self.backend_registry.find_backend(&backend_module)
            .ok_or("No suitable backend found")?;

        if self.config.verbose {
            println!("[Rython] Selected backend: {}", backend.name());
        }

        // Generate assembly (include module assembly)
        let assembly = match self.config.target {
            Target::Bios16 => {
                let mut bios16 = crate::backend::Bios16Backend::new();
                bios16.compile_program(&program)?
            }
            Target::Bios32 => {
                let mut bios32 = crate::backend::Bios32Backend::new();
                bios32.compile_program(&program)?
            }
            Target::Bios64 => {
                let mut bios64 = crate::backend::Bios64Backend::new();
                bios64.compile_program(&program)?
            }
            Target::Bios64Sse => {
                let mut bios64_sse = crate::backend::Bios64Backend::new().with_sse();
                bios64_sse.compile_program(&program)?
            }
            Target::Bios64Avx => {
                let mut bios64_avx = crate::backend::Bios64Backend::new().with_avx();
                bios64_avx.compile_program(&program)?
            }
            Target::Bios64Avx512 => {
                let mut bios64_avx512 = crate::backend::Bios64Backend::new().with_avx512();
                bios64_avx512.compile_program(&program)?
            }
            Target::Linux64 => {
                let mut linux64 = crate::backend::Linux64Backend::new();
                linux64.compile_program(&program)?
            }
            Target::Windows64 => {
                let mut windows64 = crate::backend::Windows64Backend::new();
                windows64.compile_program(&program)?
            }
        };

        // Append assembly from modules
        let mut final_assembly = assembly;
        let target_str = self.config.target.to_string();
        for module_name in &all_modules {
            if let Some(module) = self.module_registry.get_module(module_name) {
                let module_asm = module.get_assembly(&target_str);
                if !module_asm.is_empty() {
                    final_assembly.push_str(&format!("\n; ========== MODULE: {} ==========\n\n{}", module_name, module_asm));
                }
            }
        }

        // Write assembly to file
        let asm_file = format!("{}.asm", output_path);
        if self.config.verbose {
            println!("[Rython] Writing assembly to: {}", asm_file);
        }
        
        fs::write(&asm_file, &final_assembly)
            .map_err(|e| format!("Failed to write assembly: {}", e))?;

        // Assemble with NASM
        let nasm_path = find_nasm();
        let format = self.config.target.get_format();
        
        if self.config.verbose {
            println!("[Rython] Assembling with NASM, format: {}", format);
        }
        
        let nasm_output = Command::new(&nasm_path)
            .args(&["-f", format, "-o", output_path, &asm_file])
            .output()
            .map_err(|e| format!("Failed to run NASM: {}", e))?;

        if !nasm_output.status.success() {
            let error_msg = String::from_utf8_lossy(&nasm_output.stderr);
            return Err(format!("NASM failed: {}", error_msg));
        }

        // For Linux/Windows, we need linking
        if matches!(self.config.target, Target::Linux64 | Target::Windows64) {
            if self.config.verbose {
                println!("[Rython] Linking required for target");
            }
            
            let object_file = output_path;
            let final_output = format!("{}_linked", output_path);
            
            let linker_path = find_linker();
            let link_result = Command::new(&linker_path)
                .args(&["-o", &final_output, object_file])
                .output();
                
            match link_result {
                Ok(output) if output.status.success() => {
                    // Replace with linked output
                    fs::rename(&final_output, output_path)
                        .map_err(|e| format!("Failed to rename: {}", e))?;
                }
                _ => {
                    // Fallback to manual linking
                    manual_link(object_file, output_path)?;
                }
            }
        }

        // Cleanup
        if !self.config.keep_assembly {
            fs::remove_file(asm_file)
                .map_err(|e| format!("Failed to remove assembly file: {}", e))?;
        }

        if self.config.verbose {
            println!("[Rython] Compilation successful!");
        }
        
        Ok(())
    }

    // Helper to extract modules from program (e.g., from import statements)
    pub fn extract_required_modules(&self, program: &Program) -> Vec<String> {
        let mut modules = Vec::new();
        
        // Check for import-like patterns in expressions
        for stmt in &program.body {
            if let Statement::Expr(crate::parser::Expr::Call { func, args, .. }) = stmt {
                if func == "import" && !args.is_empty() {
                    if let crate::parser::Expr::String(module_name, _) = &args[0] {
                        if self.module_registry.get_module(module_name).is_some() {
                            modules.push(module_name.clone());
                        }
                    }
                }
            }
        }
        
        modules
    }
    
    pub fn assemble_bios(&self, asm_file: &str, output_path: &str) -> Result<(), String> {
        let nasm = find_nasm();
        
        let status = Command::new(&nasm)
            .arg("-f")
            .arg("bin")
            .arg("-o")
            .arg(output_path)
            .arg(asm_file)
            .status()
            .map_err(|e| format!("Failed to run NASM: {}", e))?;
        
        if status.success() {
            Ok(())
        } else {
            Err("NASM assembly failed".to_string())
        }
    }
    
    pub fn assemble_linux(&self, asm_file: &str, output_path: &str) -> Result<(), String> {
        let nasm = find_nasm();
        let obj_file = format!("{}.o", output_path);
        
        // Assemble with NASM
        let status = Command::new(&nasm)
            .arg("-f")
            .arg("elf64")
            .arg("-o")
            .arg(&obj_file)
            .arg(asm_file)
            .status()
            .map_err(|e| format!("Failed to run NASM: {}", e))?;
        
        if !status.success() {
            return Err("NASM assembly failed".to_string());
        }
        
        // Link
        let result = Command::new("ld")
            .arg("-o")
            .arg(output_path)
            .arg(&obj_file)
            .status();
        
        match result {
            Ok(status) if status.success() => {
                fs::remove_file(&obj_file).map_err(|e| e.to_string())?;
                Ok(())
            }
            _ => {
                // Try manual linking
                manual_link(&obj_file, output_path)?;
                fs::remove_file(&obj_file).map_err(|e| e.to_string())?;
                Ok(())
            }
        }
    }
    
    pub fn assemble_windows(&self, asm_file: &str, output_path: &str) -> Result<(), String> {
        let nasm = find_nasm();
        let obj_file = format!("{}.obj", output_path);
        
        let status = Command::new(&nasm)
            .arg("-f")
            .arg("win64")
            .arg("-o")
            .arg(&obj_file)
            .arg(asm_file)
            .status()
            .map_err(|e| format!("Failed to run NASM: {}", e))?;
        
        if !status.success() {
            return Err("NASM assembly failed".to_string());
        }
        
        // Try manual linking first
        match manual_link(&obj_file, output_path) {
            Ok(_) => {
                fs::remove_file(&obj_file).map_err(|e| e.to_string())?;
                Ok(())
            }
            Err(e) => {
                println!("Manual linking failed: {}", e);
                // Fallback
                let result = Command::new("link")
                    .arg("/subsystem:console")
                    .arg("/entry:main")
                    .arg(&obj_file)
                    .arg(format!("/out:{}", output_path))
                    .status();
                
                result.map_err(|e| format!("Linking failed: {}", e))
                    .and_then(|status| {
                        if status.success() {
                            fs::remove_file(&obj_file).map_err(|e| e.to_string())?;
                            Ok(())
                        } else {
                            Err("All linking attempts failed".to_string())
                        }
                    })
            }
        }
    }
    
    pub fn assemble_based_on_target(&self, asm_file: &str, output_path: &str) -> Result<(), String> {
        match self.config.target {
            Target::Bios16 | Target::Bios32 | Target::Bios64 | 
            Target::Bios64Sse | Target::Bios64Avx | Target::Bios64Avx512 => 
                self.assemble_bios(asm_file, output_path),
            Target::Linux64 => self.assemble_linux(asm_file, output_path),
            Target::Windows64 => self.assemble_windows(asm_file, output_path),
        }
    }
}

// Public functions with real implementations
pub fn compile(source_code: &str, output_path: &str) -> Result<(), String> {
    let config = CompilerConfig {
        verbose: true,
        ..Default::default()
    };
    
    let mut compiler = RythonCompiler::new(config);
    compiler.compile(source_code, output_path)
}

// Keep the original compile_code function for backward compatibility
pub fn compile_code<S: AsRef<str>>(source: S, output_path: S) -> Result<(), String> {
    let source_code = source.as_ref();
    let output_str = output_path.as_ref();
    
    let mut compiler = RythonCompiler::new(CompilerConfig::default());
    compiler.compile(source_code, output_str)
}

pub fn compile_to_bios16(source: &str, output_path: &str) -> Result<(), String> {
    let config = CompilerConfig {
        target: Target::Bios16,
        verbose: true,
        keep_assembly: true,
        ..Default::default()
    };
    
    let mut compiler = RythonCompiler::new(config);
    compiler.compile(source, output_path)
}

pub fn compile_to_bios32(source: &str, output_path: &str) -> Result<(), String> {
    let config = CompilerConfig {
        target: Target::Bios32,
        verbose: true,
        keep_assembly: true,
        ..Default::default()
    };
    
    let mut compiler = RythonCompiler::new(config);
    compiler.compile(source, output_path)
}

pub fn compile_to_bios64(source: &str, output_path: &str) -> Result<(), String> {
    let config = CompilerConfig {
        target: Target::Bios64,
        verbose: true,
        keep_assembly: true,
        ..Default::default()
    };
    
    let mut compiler = RythonCompiler::new(config);
    compiler.compile(source, output_path)
}

pub fn compile_to_bios64_sse(source: &str, output_path: &str) -> Result<(), String> {
    let config = CompilerConfig {
        target: Target::Bios64Sse,
        verbose: true,
        keep_assembly: true,
        ..Default::default()
    };
    
    let mut compiler = RythonCompiler::new(config);
    compiler.compile(source, output_path)
}

pub fn compile_to_bios64_avx(source: &str, output_path: &str) -> Result<(), String> {
    let config = CompilerConfig {
        target: Target::Bios64Avx,
        verbose: true,
        keep_assembly: true,
        ..Default::default()
    };
    
    let mut compiler = RythonCompiler::new(config);
    compiler.compile(source, output_path)
}

pub fn compile_to_bios64_avx512(source: &str, output_path: &str) -> Result<(), String> {
    let config = CompilerConfig {
        target: Target::Bios64Avx512,
        verbose: true,
        keep_assembly: true,
        ..Default::default()
    };
    
    let mut compiler = RythonCompiler::new(config);
    compiler.compile(source, output_path)
}

pub fn compile_to_linux(source: &str, output_path: &str) -> Result<(), String> {
    let config = CompilerConfig {
        target: Target::Linux64,
        verbose: true,
        ..Default::default()
    };
    
    let mut compiler = RythonCompiler::new(config);
    compiler.compile(source, output_path)
}

pub fn compile_to_windows(source: &str, output_path: &str) -> Result<(), String> {
    let config = CompilerConfig {
        target: Target::Windows64,
        verbose: true,
        ..Default::default()
    };
    
    let mut compiler = RythonCompiler::new(config);
    compiler.compile(source, output_path)
}

pub fn compile_to_bootloader(source_code: &str) -> Result<Vec<u8>, String> {
    let config = CompilerConfig {
        target: Target::Bios64,
        verbose: true,
        ..Default::default()
    };
    
    let mut compiler = RythonCompiler::new(config);
    
    // Create temporary file
    let temp_file = "temp_boot.bin";
    compiler.compile(source_code, temp_file)?;
    
    // Read binary back
    let binary = fs::read(temp_file)
        .map_err(|e| format!("Failed to read binary: {}", e))?;
    
    // Clean up
    fs::remove_file(temp_file)
        .map_err(|e| format!("Failed to remove temp file: {}", e))?;
    
    Ok(binary)
}