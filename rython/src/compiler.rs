// ==================== COMPILER ====================

use std::fs;
use std::process::Command;
use crate::backend::{BackendRegistry, Backend, Bios64Backend, Linux64Backend, BackendModule, Capability};

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

#[derive(Debug, Clone)]
pub struct CompilerConfig {
    pub target: Target,
    pub verbose: bool,
    pub keep_assembly: bool,
    pub optimize: bool,
}

impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            target: Target::Bios64,
            verbose: false,
            keep_assembly: false,
            optimize: true,
        }
    }
}

pub struct RythonCompiler {
    config: CompilerConfig,
    backend_registry: BackendRegistry,
}

impl RythonCompiler {
    pub fn new(config: CompilerConfig) -> Self {
        Self {
            config,
            backend_registry: BackendRegistry::default_registry(),
        }
    }
    
    /// Main compilation entry point
    pub fn compile(&mut self, source: &str, output_path: &str) -> Result<(), String> {
        // 1. Parse source code
        let program = crate::parser::parse_program(source)
            .map_err(|e| format!("Parse error: {}", e))?;
        
        if self.config.verbose {
            println!("[Rython] Parsed AST successfully");
        }
        
        // 2. Create backend module
        let module = self.create_backend_module(&program)?;
        
        // 3. Select backend based on target and capabilities
        let mut backend = self.select_backend(&module)?;
        
        // 4. Generate assembly
        let asm = backend.compile_program(&program)?;
        
        if self.config.verbose {
            println!("[Rython] Generated assembly (first 50 lines):");
            for (i, line) in asm.lines().take(50).enumerate() {
                println!("{:3}: {}", i + 1, line);
            }
        }
        
        let asm_file = format!("{}.asm", output_path);
        
        // 5. Write assembly
        fs::write(&asm_file, &asm)
            .map_err(|e| format!("Failed to write assembly: {}", e))?;
        
        // 6. Assemble based on target
        match self.config.target {
            Target::Bios16 | Target::Bios32 | Target::Bios64 | 
            Target::Bios64Sse | Target::Bios64Avx | Target::Bios64Avx512 => 
                self.assemble_bios(&asm_file, output_path),
            Target::Linux64 => self.assemble_linux(&asm_file, output_path),
            Target::Windows64 => self.assemble_windows(&asm_file, output_path),
        }?;
        
        if !self.config.keep_assembly {
            fs::remove_file(&asm_file)
                .map_err(|e| format!("Failed to remove assembly file: {}", e))?;
        }
        
        Ok(())
    }
    
    fn create_backend_module(&self, _program: &crate::parser::Program) -> Result<BackendModule, String> {
        // Create a simple module with required capabilities
        let mut required_capabilities = Vec::new();
        
        match self.config.target {
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
                required_capabilities.push(Capability::SSE2);
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
        // Find compatible backend
        for backend in &self.backend_registry.backends {
            if backend.can_compile(module) {
                // Create a mutable clone for compilation
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
                    "linux64" => return Ok(Box::new(Linux64Backend::new())),
                    _ => continue,
                }
            }
        }
        
        // No compatible backend found
        let required: Vec<String> = module.required_capabilities.iter()
            .map(|c| format!("{:?}", c))
            .collect();
        
        let available: Vec<String> = self.backend_registry.backends.iter()
            .map(|b| b.name().to_string())
            .collect();
        
        Err(format!(
            "No backend supports all required capabilities: {:?}\n\
             Available backends: {}",
            required,
            available.join(", ")
        ))
    }
    
    pub(crate) fn assemble_bios(&self, asm_file: &str, output_path: &str) -> Result<(), String> {
        let nasm = crate::utils::find_nasm();
        
        Command::new(&nasm)
            .arg("-f")
            .arg("bin")
            .arg("-o")
            .arg(output_path)
            .arg(asm_file)
            .status()
            .map_err(|e| format!("Failed to run NASM: {}", e))
            .and_then(|status| {
                if status.success() {
                    Ok(())
                } else {
                    Err("NASM assembly failed".to_string())
                }
            })
    }
    
    pub(crate) fn assemble_linux(&self, asm_file: &str, output_path: &str) -> Result<(), String> {
        let nasm = crate::utils::find_nasm();
        let obj_file = format!("{}.o", output_path);
        
        // Assemble with NASM
        Command::new(&nasm)
            .arg("-f")
            .arg("elf64")
            .arg("-o")
            .arg(&obj_file)
            .arg(asm_file)
            .status()
            .map_err(|e| format!("Failed to run NASM: {}", e))
            .and_then(|status| {
                if !status.success() {
                    return Err("NASM assembly failed".to_string());
                }
                Ok(())
            })?;
        
        // Link
        let result = Command::new("ld")
            .arg("-o")
            .arg(output_path)
            .arg(&obj_file)
            .status();
        
        match result {
            Ok(status) if status.success() => {
                fs::remove_file(&obj_file).ok();
                Ok(())
            }
            _ => {
                // Try manual linking
                crate::linker::manual_link(&obj_file, output_path)?;
                fs::remove_file(&obj_file).ok();
                Ok(())
            }
        }
    }
    
    pub(crate) fn assemble_windows(&self, asm_file: &str, output_path: &str) -> Result<(), String> {
        let nasm = crate::utils::find_nasm();
        let obj_file = format!("{}.obj", output_path);
        
        Command::new(&nasm)
            .arg("-f")
            .arg("win64")
            .arg("-o")
            .arg(&obj_file)
            .arg(asm_file)
            .status()
            .map_err(|e| format!("Failed to run NASM: {}", e))
            .and_then(|status| {
                if !status.success() {
                    return Err("NASM assembly failed".to_string());
                }
                Ok(())
            })?;
        
        // Try manual linking first
        match crate::linker::manual_link(&obj_file, output_path) {
            Ok(_) => {
                fs::remove_file(&obj_file).ok();
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
                            fs::remove_file(&obj_file).ok();
                            Ok(())
                        } else {
                            Err("All linking attempts failed".to_string())
                        }
                    })
            }
        }
    }
}

// ========== PUBLIC API FUNCTIONS ==========

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
    fs::remove_file(temp_file).ok();
    
    Ok(binary)
}

// Keep the original compile_code function for backward compatibility
pub fn compile_code<S: AsRef<str>>(source: S, output_path: S) -> Result<(), String> {
    let source_code = source.as_ref();
    let output_str = output_path.as_ref();
    
    let mut compiler = RythonCompiler::new(CompilerConfig::default());
    compiler.compile(source_code, output_str)
}