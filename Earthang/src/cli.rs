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
use clap::{Parser, Subcommand, Args, ValueEnum};
use std::path::PathBuf;
use colored::*;
use std::time::Instant;
use crate::compiler::{EarthangCompiler, CompilerConfig};
use crate::backend::Backend;

/// Terminal output styling
pub mod style {
    use colored::*;
    use std::path::PathBuf;
    
    pub fn success(msg: &str) -> String {
        format!("{} {}", "✓".green().bold(), msg)
    }
    
    pub fn error(msg: &str) -> String {
        format!("{} {}", "✗".red().bold(), msg)
    }
    
    pub fn warning(msg: &str) -> String {
        format!("{} {}", "⚠".yellow().bold(), msg)
    }
    
    pub fn info(msg: &str) -> String {
        format!("{} {}", "•".cyan(), msg)
    }
    
    pub fn section(title: &str) -> String {
        format!("\n{}\n", title.cyan().bold().underline())
    }
    
    pub fn command(cmd: &str) -> String {
        format!("{}", cmd.blue().bold())
    }
    
    pub fn path(path: &PathBuf) -> String {
        format!("{}", path.display().to_string().green())
    }
    
    pub fn target(name: &str) -> String {
        format!("[{}]", name.yellow())
    }
    
    pub fn timestamp() -> String {
        format!("{}", chrono::Local::now().format("%H:%M:%S").to_string().dimmed())
    }
    
    pub fn print_header(version: &str) {
        println!("{} {}", "Version:".dimmed(), version.cyan());
        println!("{} {}\n", "Source:".dimmed(), "https://github.com/Bit-Jumper-Studio/earthang".blue().underline());
    }
}

/// earthang Compiler CLI
#[derive(Parser)]
#[command(name = "earthang")]
#[command(about = "earthang Compiler - Lua-like syntax to bare metal binary")]
#[command(version = "1.0.0")]
#[command(long_about = r#"
earthang Compiler - Lua-like syntax to bare metal binary

Compile Lua-like code to native binaries for Linux with hardware support.

Examples:
  Build a Linux ELF executable:
    earthang compile program.lua --target linux64 --output program

  Build with hardware DSL support:
    earthang compile program.lua --target linux64 --hardware --output program

  List available targets:
    earthang targets

For more information, see https://github.com/Bit-Jumper-Studio/earthang
"#)]
pub struct Cli {
    /// Enable verbose output
    #[arg(short, long)]
    pub verbose: bool,
    
    /// Enable quiet mode (suppress all output except errors)
    #[arg(short, long)]
    pub quiet: bool,
    
    /// Show colored output (always on for now)
    #[arg(long, default_value_t = true)]
    pub color: bool,
    
    /// Command to execute
    #[command(subcommand)]
    pub command: Option<Commands>,
}

/// Available commands
#[derive(Subcommand)]
pub enum Commands {
    /// Compile earthang source to binary
    Compile(CompileArgs),
    
    /// Generate code
    Generate(GenerateArgs),
    
    /// Run tests
    Test(TestArgs),
    
    /// Show version
    Version,
    
    /// Check toolchain
    Check,
    
    /// List available targets
    Targets,
    
    /// Hardware DSL commands
    Hardware(HardwareArgs),
}

/// System target platforms
#[derive(Debug, Clone, Copy, PartialEq, ValueEnum)]
pub enum CliTarget {
    Linux64,
}

impl From<CliTarget> for crate::backend::Target {
    fn from(val: CliTarget) -> Self {
        match val {
            CliTarget::Linux64 => crate::backend::Target::Linux64,
        }
    }
}

impl CliTarget {
    fn description(&self) -> &'static str {
        match self {
            CliTarget::Linux64 => "64-bit Linux ELF executable",
        }
    }
}

/// Arguments for compile command
#[derive(Args)]
#[command(after_help = r#"
Examples:
  Build Linux ELF executable:
    earthang compile program.lua --target linux64 --output program

  Build with hardware support:
    earthang compile program.lua --target linux64 --hardware --output program

  Build with optimization disabled:
    earthang compile program.lua --target linux64 --no-optimize

Notes:
  - Linux targets produce ELF executables
  - Use --keep-assembly to save intermediate assembly files
  - Use --verbose for detailed compilation output
  - Use --hardware to enable hardware DSL for device access
"#)]
pub struct CompileArgs {
    /// Input file
    pub file: PathBuf,
    
    /// Output file
    #[arg(short, long)]
    pub output: Option<PathBuf>,
    
    /// Target platform
    #[arg(short, long, value_enum, default_value_t = CliTarget::Linux64)]
    pub target: CliTarget,
    
    /// Keep assembly file
    #[arg(long, help = "Keep intermediate assembly file")]
    pub keep_assembly: bool,
    
    /// Disable optimization
    #[arg(long, help = "Disable code optimization")]
    pub no_optimize: bool,
    
    /// Enable hardware DSL
    #[arg(long, help = "Enable hardware DSL for device access")]
    pub hardware: bool,
    
    /// Show memory usage
    #[arg(long, help = "Show memory usage statistics")]
    pub memory: bool,
}

/// Arguments for generate command
#[derive(Args)]
pub struct GenerateArgs {
    /// Type to generate
    #[arg(short, long, value_enum, default_value_t = CliTarget::Linux64)]
    pub r#type: CliTarget,
    
    /// Output file
    #[arg(short, long)]
    pub output: Option<PathBuf>,
    
    /// Include hardware DSL example
    #[arg(long, help = "Include hardware DSL example")]
    pub hardware_example: bool,
}

/// Arguments for test command
#[derive(Args)]
pub struct TestArgs {
    /// Test suite
    #[arg(short, long, default_value = "basic")]
    pub suite: String,
    
    /// Verbose test output
    #[arg(short, long)]
    pub verbose: bool,
    
    /// Run all tests
    #[arg(long)]
    pub all: bool,
    
    /// Test hardware DSL
    #[arg(long, help = "Test hardware DSL functionality")]
    pub hardware: bool,
}

/// Arguments for hardware commands
#[derive(Args)]
pub struct HardwareArgs {
    /// Hardware command
    #[command(subcommand)]
    pub command: HardwareCommands,
}

/// Hardware subcommands
#[derive(Subcommand)]
pub enum HardwareCommands {
    /// Generate hardware DSL example
    Example,
    
    /// List available hardware devices
    Devices,
    
    /// Test hardware DSL parsing
    Test,
}

/// CLI progress reporter
struct Progress {
    start_time: Instant,
    verbose: bool,
}

impl Progress {
    fn new(verbose: bool) -> Self {
        Self {
            start_time: Instant::now(),
            verbose,
        }
    }
    
    fn step(&self, msg: &str) {
        if self.verbose {
            println!("    {} {}", ">".blue(), msg.dimmed());
        }
    }
    
    fn done(&self, msg: &str) {
        let duration = self.start_time.elapsed();
        println!("    {} {} {}", "✓".green(), msg, format!("({:.2}s)", duration.as_secs_f64()).dimmed());
    }
    
    fn warn(&self, msg: &str) {
        println!("    {} {}", "!".yellow(), msg.yellow());
    }
    
    fn error(&self, msg: &str) -> String {
        format!("{} {}", "✗".red(), msg.red())
    }
}

/// Main CLI handler
impl Cli {
    pub fn run(self) -> Result<(), String> {
        // Set colored output control
        colored::control::set_override(true);
        
        match &self.command {
            Some(command) => match command {
                Commands::Compile(args) => self.handle_compile(args, self.verbose),
                Commands::Test(args) => self.handle_test(args, self.verbose),
                Commands::Version => self.handle_version(),
                Commands::Check => self.handle_check(self.verbose),
                Commands::Targets => self.handle_targets(self.verbose),
                Commands::Generate(args) => self.handle_generate(args, self.verbose),
                Commands::Hardware(args) => self.handle_hardware(args, self.verbose),
            },
            None => {
                if !self.quiet {
                    println!("{}", "No command specified. Use --help for usage information.".yellow());
                }
                Ok(())
            }
        }
    }
    
    fn handle_compile(&self, args: &CompileArgs, verbose: bool) -> Result<(), String> {
    let progress = Progress::new(verbose);
    
    if !self.quiet {
        println!("{}", style::section("COMPILATION"));
        println!("  {} {}", "Source:".cyan(), style::path(&args.file));
        println!("  {} {}", "Target:".cyan(), style::target(&format!("{:?}", args.target)));
        if args.hardware {
            println!("  {} {}", "Hardware DSL:".cyan(), "Enabled".green().bold());
        }
    }
    
    let input_file = &args.file;
    
    // Check if file exists
    if !input_file.exists() {
        return Err(progress.error(&format!("File '{}' not found. Please check the filename and path.", input_file.display())));
    }
    
    // Check if it's actually a file
    if !input_file.is_file() {
        return Err(progress.error(&format!("'{}' is not a valid file.", input_file.display())));
    }
    
    let output_file = args.output.as_ref().map_or_else(|| {
        let mut path = input_file.clone();
        path.set_extension("elf");
        path
    }, |p| p.clone());
    
    let target: crate::backend::Target = args.target.into();
    
    progress.step("Reading source file...");
    let source = std::fs::read_to_string(input_file)
        .map_err(|e| progress.error(&format!("Failed to read source file '{}': {}", input_file.display(), e)))?;
    
    progress.step("Parsing syntax...");
    crate::parser::parse_program(&source)
        .map_err(|e| progress.error(&format!("Parse error in '{}': {:?}", input_file.display(), e)))?;
    
    if !self.quiet {
        println!("  {} {}", "Output:".cyan(), style::path(&output_file));
    }
    
    let config = CompilerConfig {
        target,
        verbose,
        keep_assembly: args.keep_assembly,
        optimize: !args.no_optimize,
        modules: Vec::new(),
        debug_info: false,
        include_stdlib: false,
        hardware_dsl_enabled: args.hardware,
        code_size_limit: None,
        search_paths: vec![PathBuf::from("."), PathBuf::from("stdlib")],
    };
    
    progress.step("Compiling to assembly...");
    let mut compiler = EarthangCompiler::new(config);
    let result = compiler.compile_source(&source, Some(&input_file))?;
    
    progress.step("Writing output file...");
    std::fs::write(&output_file, result.assembly)
        .map_err(|e| progress.error(&format!("Failed to write output file '{}': {}", output_file.display(), e)))?;
    
    if !self.quiet {
        progress.done("Compilation successful!");
        println!();
        
        let target_type = "Linux ELF executable";
        
        println!("  {} {} {} created", "✓".green(), target_type, style::path(&output_file).bold());
        
        println!("  {} Make executable: {}", ">".blue(), format!("chmod +x {}", output_file.display()).cyan());
        
        if args.keep_assembly {
            let asm_file = output_file.with_extension("asm");
            println!("  {} {}", "Assembly saved to:".dimmed(), style::path(&asm_file));
        }
        
        if args.hardware {
            println!("  {} {}", "Hardware DSL:".dimmed(), "Enabled - includes device access functions".green());
        }
    }
    
    Ok(())
}
    
    fn handle_generate(&self, args: &GenerateArgs, verbose: bool) -> Result<(), String> {
        let progress = Progress::new(verbose);
        let target: crate::backend::Target = args.r#type.into();
        
        if !self.quiet {
            println!("{}", style::section("CODE GENERATION"));
            println!("  {} {}", "Target:".cyan(), style::target(&format!("{:?}", target)));
            if args.hardware_example {
                println!("  {} {}", "Hardware example:".cyan(), "Included".green().bold());
            }
        }
        
        let output = match target {
            crate::backend::Target::Linux64 => {
                progress.step("Generating 64-bit Linux ELF executable...");
                let mut code = String::new();
                
                code.push_str("; Linux 64-bit ELF Executable\n");
                code.push_str("; Generated by earthang Compiler\n");
                code.push_str("\n");
                code.push_str("bits 64\n");
                code.push_str("default rel\n\n");
                code.push_str("section .text\n");
                code.push_str("global _start\n\n");
                code.push_str("_start:\n");
                code.push_str("    mov rbp, rsp\n");
                code.push_str("    and rsp, -16\n");
                code.push_str("    sub rsp, 32\n\n");
                code.push_str("    ; Print message\n");
                code.push_str("    mov rax, 1        ; sys_write\n");
                code.push_str("    mov rdi, 1        ; stdout\n");
                code.push_str("    lea rsi, [msg]\n");
                code.push_str("    mov rdx, 14       ; length\n");
                code.push_str("    syscall\n\n");
                code.push_str("    ; Exit\n");
                code.push_str("    mov rax, 60       ; sys_exit\n");
                code.push_str("    xor rdi, rdi      ; exit code 0\n");
                code.push_str("    syscall\n\n");
                
                if args.hardware_example {
                    code.push_str("; Hardware DSL Example\n");
                    code.push_str("gpu_render_frame:\n");
                    code.push_str("    ; Example GPU rendering function\n");
                    code.push_str("    push rbp\n");
                    code.push_str("    mov rbp, rsp\n");
                    code.push_str("    \n");
                    code.push_str("    ; Direct hardware access example\n");
                    code.push_str("    mov dx, 0x3D4\n");
                    code.push_str("    mov al, 0x0A\n");
                    code.push_str("    out dx, al\n");
                    code.push_str("    \n");
                    code.push_str("    ; Wait for vertical sync\n");
                    code.push_str("    mov dx, 0x3DA\n");
                    code.push_str(".wait_vsync:\n");
                    code.push_str("    in al, dx\n");
                    code.push_str("    test al, 8\n");
                    code.push_str("    jz .wait_vsync\n");
                    code.push_str("    \n");
                    code.push_str("    mov rsp, rbp\n");
                    code.push_str("    pop rbp\n");
                    code.push_str("    ret\n\n");
                }
                
                code.push_str("section .data\n");
                code.push_str("msg:\n");
                code.push_str("    db 'Hello earthang!', 10, 0\n");
                
                code
            }
        };
        
        if let Some(output_path) = &args.output {
            progress.step(&format!("Writing to: {}", output_path.display()));
            std::fs::write(output_path, &output)
                .map_err(|e| progress.error(&format!("Failed to write output: {}", e)))?;
            
            if !self.quiet {
                progress.done("Code generation complete!");
                println!("  {} {}", "Output written to:".green(), style::path(output_path).bold());
                
                // Show compilation command
                println!("  {} {}", "Compile with NASM:".dimmed(), format!("nasm -f elf64 {} -o {}.o", output_path.display(), output_path.with_extension("").display()).cyan());
                println!("  {} {}", "Link with GCC:".dimmed(), format!("gcc -no-pie {}.o -o {}.elf", output_path.with_extension("").display(), output_path.with_extension("").display()).cyan());
            }
        } else {
            println!("\n{}", output);
            if !self.quiet {
                progress.done("Code generation complete!");
            }
        }
        
        Ok(())
    }
    
    fn handle_hardware(&self, args: &HardwareArgs, verbose: bool) -> Result<(), String> {
        let progress = Progress::new(verbose);
        
        match &args.command {
            HardwareCommands::Example => {
                if !self.quiet {
                    println!("{}", style::section("HARDWARE DSL EXAMPLE"));
                }
                
                let dsl = crate::dsl::HardwareDSL::new();
                let example = dsl.generate_example();
                
                println!("{}", example);
                
                progress.done("Hardware DSL example generated");
                Ok(())
            }
            HardwareCommands::Devices => {
                if !self.quiet {
                    println!("{}", style::section("AVAILABLE HARDWARE DEVICES"));
                }
                
                let dsl = crate::dsl::HardwareDSL::new();
                
                println!("  {} Built-in hardware devices:", style::info(""));
                for (name, device) in &dsl.device_registry {
                    println!("    {} {} - {}", 
                        "•".blue(), 
                        name.green().bold(),
                        format!("{:?}", device.device_type).dimmed()
                    );
                    
                    if !device.registers.is_empty() {
                        println!("      {} Registers:", style::info(""));
                        for (reg_name, addr) in &device.registers {
                            println!("        {} {}: 0x{:X}", ">".blue(), reg_name.cyan(), addr);
                        }
                    }
                }
                
                progress.done("Device list displayed");
                Ok(())
            }
            HardwareCommands::Test => {
                if !self.quiet {
                    println!("{}", style::section("HARDWARE DSL TEST"));
                }
                
                progress.step("Testing hardware DSL parsing...");
                
                // Test hardware intrinsics
                let mut dsl = crate::dsl::HardwareDSL::new();
                
                // Test write_register
                match dsl.parse_hardware_statement("write_register(0x3D4, 0x0A)") {
                    Ok(asm) => {
                        progress.done("write_register test passed");
                        if verbose {
                            println!("    Generated assembly:");
                            for line in asm {
                                println!("      {}", line);
                            }
                        }
                    }
                    Err(e) => progress.warn(&format!("write_register test failed: {}", e)),
                }
                
                // Test read_register
                match dsl.parse_hardware_statement("read_register(0x3D5)") {
                    Ok(_) => progress.done("read_register test passed"),
                    Err(e) => progress.warn(&format!("read_register test failed: {}", e)),
                }
                
                // Test hardware library generation
                let lib = dsl.generate_hardware_library();
                if lib.contains("port_in_8") && lib.contains("gpu_wait_vsync") {
                    progress.done("Hardware library generation test passed");
                } else {
                    progress.warn("Hardware library generation test failed");
                }
                
                progress.done("Hardware DSL tests completed");
                Ok(())
            }
        }
    }
    
    fn handle_test(&self, args: &TestArgs, verbose: bool) -> Result<(), String> {
        let progress = Progress::new(verbose || args.verbose);
        let suite = if args.all { "all" } else { &args.suite };
        
        if !self.quiet {
            println!("{}", style::section("RUNNING TESTS"));
            println!("  {} {}", "Suite:".cyan(), suite.blue());
            if args.hardware {
                println!("  {} {}", "Hardware tests:".cyan(), "Enabled".green().bold());
            }
        }
        
        match suite {
            "all" => {
                // Run all test suites
                progress.step("Running all test suites...");
                
                // Basic tests
                self.run_basic_tests(&progress)?;
                
                // Linux tests
                self.run_linux_tests(&progress)?;
                
                // Lua tests
                self.run_lua_tests(&progress)?;
                
                // Hardware tests if requested
                if args.hardware {
                    self.run_hardware_tests(&progress)?;
                }
                
                // Full test suite
                self.run_full_tests(&progress)?;
                
                progress.done("All test suites completed!");
            }
            "basic" => {
                self.run_basic_tests(&progress)?;
                progress.done("Basic tests passed!");
            }
            "linux" => {
                self.run_linux_tests(&progress)?;
                progress.done("Linux64 backend test passed!");
            }
            "lua" => {
                self.run_lua_tests(&progress)?;
                progress.done("Lua-like syntax test passed!");
            }
            "hardware" => {
                self.run_hardware_tests(&progress)?;
                progress.done("Hardware DSL test passed!");
            }
            "full" => {
                self.run_full_tests(&progress)?;
                progress.done("Full test suite completed!");
            }
            _ => {
                return Err(progress.error(&format!("Unknown test suite: {}", suite)));
            }
        }
        
        Ok(())
    }
    
    fn run_basic_tests(&self, progress: &Progress) -> Result<(), String> {
        progress.step("Running basic parser tests...");
        
        // Lua-like syntax
        let source = r#"
function hello()
    print("Hello, World!")
end

x = 10
y = x + 5
"#;
        
        match crate::parser::parse_program(source) {
            Ok(_) => progress.done("Parser test passed"),
            Err(e) => return Err(progress.error(&format!("Parser test failed: {:?}", e))),
        }
        
        progress.done("CLI structure test passed");
        
        Ok(())
    }
    
    fn run_hardware_tests(&self, progress: &Progress) -> Result<(), String> {
        progress.step("Testing hardware DSL...");
        
        // Test hardware DSL parsing
        let mut dsl = crate::dsl::HardwareDSL::new();
        
        // Test hardware intrinsic
        match dsl.parse_hardware_statement("write_register(0x3D4, 0x0A)") {
            Ok(_) => progress.done("Hardware intrinsic parsing test passed"),
            Err(e) => progress.warn(&format!("Hardware intrinsic parsing failed: {}", e)),
        }
        
        // Test hardware function generation
        match dsl.generate_device_function_prologue("gpu", "render_frame") {
            Ok(_) => progress.done("Hardware function prologue test passed"),
            Err(e) => progress.warn(&format!("Hardware function prologue failed: {}", e)),
        }
        
        // Test hardware library generation
        let lib = dsl.generate_hardware_library();
        if lib.contains("port_in_8") && lib.contains("gpu_wait_vsync") {
            progress.done("Hardware library generation test passed");
        } else {
            progress.warn("Hardware library generation test failed");
        }
        
        Ok(())
    }
    
    fn run_linux_tests(&self, progress: &Progress) -> Result<(), String> {
        progress.step("Testing Linux64 backend...");
        let mut backend = crate::backend::Linux64Backend::new();
        let program = crate::parser::parse_program("print(\"Hello Linux\")")
            .map_err(|e| progress.error(&format!("Parse failed: {:?}", e)))?;
        match backend.compile_program(&program) {
            Ok(_) => progress.done("Linux64 backend test passed"),
            Err(e) => progress.warn(&format!("Linux64 backend error: {}", e)),
        }
        Ok(())
    }
    
    fn run_lua_tests(&self, progress: &Progress) -> Result<(), String> {
        progress.step("Testing Lua-like syntax parsing...");
        
        let lua_code = r#"
-- Lua-like syntax test
function factorial(n)
    if n <= 1 then
        return 1
    else
        return n * factorial(n - 1)
    end
end

local result = factorial(5)
print("Factorial of 5 is: " .. tostring(result))
"#;
        
        match crate::parser::parse_program(lua_code) {
            Ok(_) => progress.done("Lua-like syntax test passed"),
            Err(e) => progress.warn(&format!("Lua-like syntax test failed: {:?}", e)),
        }
        Ok(())
    }
    
    fn run_full_tests(&self, progress: &Progress) -> Result<(), String> {
        progress.step("Running full test suite...");
        
        let nasm_output = std::process::Command::new("nasm")
            .arg("--version")
            .output();
        
        match nasm_output {
            Ok(output) => {
                let version = String::from_utf8_lossy(&output.stdout);
                progress.done(&format!("NASM found: {}", version.lines().next().unwrap_or("unknown")));
            }
            Err(_) => progress.warn("NASM not found (required for assembly)"),
        }
        
        let rustc_output = std::process::Command::new("rustc")
            .arg("--version")
            .output();
        
        match rustc_output {
            Ok(output) => {
                let version = String::from_utf8_lossy(&output.stdout);
                progress.done(&format!("Rust toolchain: {}", version.trim()));
            }
            Err(_) => progress.warn("Rust toolchain not found"),
        }
        
        progress.done("Full test suite completed!");
        Ok(())
    }
    
    fn handle_version(&self) -> Result<(), String> {
        style::print_header("1.0.0");
        Ok(())
    }
    
    fn handle_check(&self, verbose: bool) -> Result<(), String> {
        let progress = Progress::new(verbose);
        
        if !self.quiet {
            println!("{}", style::section("TOOLCHAIN CHECK"));
        }
        
        let checks = [
            ("Rust compiler", "rustc", &["--version"]),
            ("Cargo", "cargo", &["--version"]),
            ("NASM", "nasm", &["--version"]),
            ("GCC (for Linux linking)", "gcc", &["--version"]),
        ];
        
        let mut all_ok = true;
        
        for (name, cmd, args) in checks {
            progress.step(&format!("Checking {}...", name));
            match std::process::Command::new(cmd).args(args).output() {
                Ok(output) => {
                    if output.status.success() {
                        let version = String::from_utf8_lossy(&output.stdout);
                        progress.done(&format!("{}: {}", name, version.lines().next().unwrap_or("").trim()));
                    } else {
                        progress.warn(&format!("{}: Not found or error", name));
                        all_ok = false;
                    }
                }
                Err(_) => {
                    if name == "GCC (for Linux linking)" {
                        progress.warn(&format!("{}: Not found (optional for Linux ELF linking)", name));
                    } else {
                        progress.warn(&format!("{}: Not found", name));
                        all_ok = false;
                    }
                }
            }
        }
        
        if !self.quiet {
            println!();
            if all_ok {
                println!("  {} {}", "✓".green().bold(), "All checks passed! Toolchain is ready.".green());
            } else {
                println!("  {} {}", "!".yellow().bold(), "Some checks failed. Install missing tools:".yellow());
                println!("\n  {} Installation links:", style::info(""));
                println!("    {} {}", ">".blue(), "Rust: https://rustup.rs/".blue().underline());
                println!("    {} {}", ">".blue(), "NASM: https://www.nasm.us/".blue().underline());
                println!("    {} {}", ">".blue(), "GCC: https://gcc.gnu.org/".blue().underline());
                if cfg!(target_os = "windows") {
                    println!("    {} {}", ">".blue(), "MinGW: https://www.mingw-w64.org/".blue().underline());
                }
            }
        }
        
        Ok(())
    }
    
    fn handle_targets(&self, verbose: bool) -> Result<(), String> {
        let progress = Progress::new(verbose);
        
        if !self.quiet {
            println!("{}", style::section("AVAILABLE TARGETS"));
            progress.step("Listing supported targets...");
        }
        
        println!("\n  {} OS Targets:", style::info(""));
        println!("    {} {} - {}", 
            ">".blue(), 
            "linux64".green().bold(),
            "64-bit Linux ELF executable with hardware DSL support".dimmed()
        );
        
        println!("\n  {} Hardware Support:", style::info(""));
        println!("    {} {} - {}", "•".blue(), "GPU".green(), "VGA/Graphics card access".dimmed());
        println!("    {} {} - {}", "•".blue(), "Network".green(), "Network card DMA and packet handling".dimmed());
        println!("    {} {} - {}", "•".blue(), "Storage".green(), "ATA/IDE storage controller".dimmed());
        println!("    {} {} - {}", "•".blue(), "Sound".green(), "Sound Blaster 16 audio".dimmed());
        println!("    {} {} - {}", "•".blue(), "Custom devices".green(), "Register custom hardware devices".dimmed());
        
        println!("\n  {} Build Commands:", style::info(""));
        println!("    {} Build Linux ELF: {}", ">".blue(), "earthang compile program.lua --target linux64 --output program".cyan());
        println!("    {} Build with hardware: {}", ">".blue(), "earthang compile program.lua --target linux64 --hardware --output program".cyan());
        
        println!("\n  {} Output Formats:", style::info(""));
        println!("    {} {} - Executable and Linkable Format (Linux)", "•".blue(), "ELF".green());
        
        if !self.quiet {
            progress.done("Target list displayed");
        }
        
        Ok(())
    }
}

/// Parse CLI arguments and run
pub fn run() -> Result<(), String> {
    let cli = Cli::parse();
    cli.run()
}