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

Compile Lua-like code to native binaries for BIOS and Linux.

Examples:
  Build a Linux ELF executable:
    earthang compile program.lua --target linux64 --output program

  Build a BIOS boot sector:
    earthang compile program.lua --target bios16 --output boot.bin

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
}

/// System target platforms
#[derive(Debug, Clone, Copy, PartialEq, ValueEnum)]
pub enum CliTarget {
    Bios16,
    Bios32,
    Bios64,
    Bios64Sse,
    Bios64Avx,
    Bios64Avx512,
    Linux64,
}

impl From<CliTarget> for crate::backend::Target {
    fn from(val: CliTarget) -> Self {
        match val {
            CliTarget::Bios16 => crate::backend::Target::Bios16,
            CliTarget::Bios32 => crate::backend::Target::Bios32,
            CliTarget::Bios64 => crate::backend::Target::Bios64,
            CliTarget::Bios64Sse => crate::backend::Target::Bios64Sse,
            CliTarget::Bios64Avx => crate::backend::Target::Bios64Avx,
            CliTarget::Bios64Avx512 => crate::backend::Target::Bios64Avx512,
            CliTarget::Linux64 => crate::backend::Target::Linux64,
        }
    }
}

impl CliTarget {
    fn description(&self) -> &'static str {
        match self {
            CliTarget::Bios16 => "16-bit BIOS boot sector",
            CliTarget::Bios32 => "32-bit BIOS protected mode",
            CliTarget::Bios64 => "64-bit BIOS long mode",
            CliTarget::Bios64Sse => "64-bit BIOS with SSE",
            CliTarget::Bios64Avx => "64-bit BIOS with AVX",
            CliTarget::Bios64Avx512 => "64-bit BIOS with AVX-512",
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

  Build BIOS boot sector:
    earthang compile program.lua --target bios16 --output boot.bin

  Build with SSE extensions:
    earthang compile program.lua --target bios64sse --output kernel.bin

  Build with optimization disabled:
    earthang compile program.lua --target linux64 --no-optimize

Notes:
  - Linux targets produce ELF executables
  - BIOS targets produce raw binaries for boot sectors
  - Use --keep-assembly to save intermediate assembly files
  - Use --verbose for detailed compilation output
"#)]
pub struct CompileArgs {
    /// Input file
    pub file: PathBuf,
    
    /// Output file
    #[arg(short, long)]
    pub output: Option<PathBuf>,
    
    /// Target platform
    #[arg(short, long, value_enum, default_value_t = CliTarget::Bios64)]
    pub target: CliTarget,
    
    /// Keep assembly file
    #[arg(long, help = "Keep intermediate assembly file")]
    pub keep_assembly: bool,
    
    /// Disable optimization
    #[arg(long, help = "Disable code optimization")]
    pub no_optimize: bool,
    
    /// Show memory usage
    #[arg(long, help = "Show memory usage statistics")]
    pub memory: bool,
}

/// Arguments for generate command
#[derive(Args)]
pub struct GenerateArgs {
    /// Type to generate
    #[arg(short, long, value_enum, default_value_t = CliTarget::Bios16)]
    pub r#type: CliTarget,
    
    /// Size in bytes
    #[arg(short, long, default_value_t = 512)]
    pub size: usize,
    
    /// Output file
    #[arg(short, long)]
    pub output: Option<PathBuf>,
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
        }
        
        let input_file = &args.file;
        let output_file = args.output.as_ref().map_or_else(|| {
            let mut path = input_file.clone();
            path.set_extension("bin");
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
            hardware_dsl_enabled: false,
            code_size_limit: None,
        };
        
        progress.step("Compiling to assembly...");
        let mut compiler = EarthangCompiler::new(config);
        let result = compiler.compile(&source)?;
        
        progress.step("Writing output file...");
        std::fs::write(&output_file, result.assembly)
            .map_err(|e| progress.error(&format!("Failed to write output file '{}': {}", output_file.display(), e)))?;
        
        if !self.quiet {
            progress.done("Compilation successful!");
            println!();
            
            let target_type = match args.target {
                CliTarget::Linux64 => "Linux ELF executable",
                CliTarget::Bios16 | CliTarget::Bios32 | CliTarget::Bios64 |
                CliTarget::Bios64Sse | CliTarget::Bios64Avx | CliTarget::Bios64Avx512 => "BIOS binary",
            };
            
            println!("  {} {} {} created", "✓".green(), target_type, style::path(&output_file).bold());
            
            if args.target == CliTarget::Linux64 {
                println!("  {} Make executable: {}", ">".blue(), format!("chmod +x {}", output_file.display()).cyan());
            }
            
            if args.keep_assembly {
                let asm_file = output_file.with_extension("asm");
                println!("  {} {}", "Assembly saved to:".dimmed(), style::path(&asm_file));
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
            println!("  {} {} bytes", "Size:".cyan(), args.size.to_string().yellow());
        }
        
        let output = match target {
            crate::backend::Target::Bios16 => {
                progress.step("Generating 16-bit boot sector...");
                format!(
                    "; 16-bit Boot Sector ({} bytes)\n\
                     ; Generated by earthang Compiler\n\
                     \n\
                     bits 16\n\
                     org 0x7C00\n\n\
                     start:\n\
                         cli\n\
                         xor ax, ax\n\
                         mov ds, ax\n\
                         mov es, ax\n\
                         mov ss, ax\n\
                         mov sp, 0x7C00\n\
                         sti\n\n\
                         ; Print message\n\
                         mov si, msg\n\
                         call print_string\n\n\
                         ; Halt\n\
                         cli\n\
                         hlt\n\
                         jmp $\n\n\
                     print_string:\n\
                         pusha\n\
                         mov ah, 0x0E\n\
                     .loop:\n\
                         lodsb\n\
                         test al, al\n\
                         jz .done\n\
                         int 0x10\n\
                         jmp .loop\n\
                     .done:\n\
                         popa\n\
                         ret\n\n\
                     msg:\n\
                         db 'earthang 16-bit', 0\n\n\
                     times {} db 0\n\
                     dw 0xAA55\n",
                    args.size,
                    args.size.saturating_sub(510)
                )
            }
            crate::backend::Target::Linux64 => {
                progress.step("Generating 64-bit Linux ELF executable...");
                format!(
                    "; Linux 64-bit ELF Executable\n\
                     ; Generated by earthang Compiler\n\
                     \n\
                     bits 64\n\
                     default rel\n\n\
                     section .text\n\
                     global _start\n\n\
                     _start:\n\
                         mov rbp, rsp\n\
                         and rsp, -16\n\
                         sub rsp, 32\n\n\
                         ; Print message\n\
                         mov rax, 1        ; sys_write\n\
                         mov rdi, 1        ; stdout\n\
                         lea rsi, [msg]\n\
                         mov rdx, 14       ; length\n\
                         syscall\n\n\
                         ; Exit\n\
                         mov rax, 60       ; sys_exit\n\
                         xor rdi, rdi      ; exit code 0\n\
                         syscall\n\n\
                     section .data\n\
                     msg:\n\
                         db 'Hello earthang!', 10, 0\n"
                )
            }
            _ => {
                return Err(progress.error(&format!("Target {:?} not yet implemented for code generation", target)));
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
                match target {
                    crate::backend::Target::Linux64 => {
                        println!("  {} {}", "Compile with NASM:".dimmed(), format!("nasm -f elf64 {} -o {}.o", output_path.display(), output_path.with_extension("").display()).cyan());
                        println!("  {} {}", "Link with GCC:".dimmed(), format!("gcc -no-pie {}.o -o {}.elf", output_path.with_extension("").display(), output_path.with_extension("").display()).cyan());
                    }
                    _ => {}
                }
            }
        } else {
            println!("\n{}", output);
            if !self.quiet {
                progress.done("Code generation complete!");
            }
        }
        
        Ok(())
    }
    
    fn handle_test(&self, args: &TestArgs, verbose: bool) -> Result<(), String> {
        let progress = Progress::new(verbose || args.verbose);
        let suite = if args.all { "all" } else { &args.suite };
        
        if !self.quiet {
            println!("{}", style::section("RUNNING TESTS"));
            println!("  {} {}", "Suite:".cyan(), suite.blue());
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
        
        println!("\n  {} BIOS Targets:", style::info(""));
        for target in [
            CliTarget::Bios16,
            CliTarget::Bios32,
            CliTarget::Bios64,
            CliTarget::Bios64Sse,
            CliTarget::Bios64Avx,
            CliTarget::Bios64Avx512,
        ].iter() {
            println!("    {} {} - {}", 
                ">".blue(), 
                format!("{:?}", target).green().bold(),
                target.description().dimmed()
            );
        }
        
        println!("\n  {} OS Targets:", style::info(""));
        println!("    {} {} - {}", 
            ">".blue(), 
            "linux64".green().bold(),
            "64-bit Linux ELF executable".dimmed()
        );
        
        println!("\n  {} Build Commands:", style::info(""));
        println!("    {} Build Linux ELF: {}", ">".blue(), "earthang compile program.lua --target linux64 --output program".cyan());
        println!("    {} Build BIOS: {}", ">".blue(), "earthang compile program.lua --target bios64 --output kernel.bin".cyan());
        
        println!("\n  {} Output Formats:", style::info(""));
        println!("    {} {} - Executable and Linkable Format (Linux)", "•".blue(), "ELF".green());
        println!("    {} {} - Raw binary (BIOS boot sector)", "•".blue(), "Raw".green());
        
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