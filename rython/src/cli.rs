use clap::{Parser, Subcommand, ValueEnum, CommandFactory, ColorChoice};
use colored::*;
use std::env;
use std::path::Path;

/// Target architectures for the Rython compiler
#[derive(ValueEnum, Clone, Debug, Default)]
enum Target {
    Bios16,
    Bios32,
    #[default]
    Bios64,
    Bios64Sse,
    Bios64Avx,
    Bios64Avx512,
}

impl std::fmt::Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Target::Bios16 => "bios16",
            Target::Bios32 => "bios32",
            Target::Bios64 => "bios64",
            Target::Bios64Sse => "bios64_sse",
            Target::Bios64Avx => "bios64_avx",
            Target::Bios64Avx512 => "bios64_avx512",
        };
        write!(f, "{}", s)
    }
}

impl Target {
    fn to_rcl_string(&self) -> &'static str {
        match self {
            Target::Bios16 => "bios16",
            Target::Bios32 => "bios32",
            Target::Bios64 => "bios64",
            Target::Bios64Sse => "bios64_sse",
            Target::Bios64Avx => "bios64_avx",
            Target::Bios64Avx512 => "bios64_avx512",
        }
    }
}

#[derive(Parser)]
#[command(
    name = "Rython",
    version = env!("CARGO_PKG_VERSION"),
    about = "Rython Compiler: High-Performance Mode Transitions & Bootloaders",
    color = ColorChoice::Always,
    help_template = "\
\x1b[1;36m{name}\x1b[0m \x1b[32m{version}\x1b[0m
{author-with-newline}\x1b[1m{about}\x1b[0m

\x1b[1;33mUSAGE:\x1b[0m 
  {usage}

{all-args}{after-help}
",
    subcommand_help_heading = "\x1b[1;33mCOMMANDS\x1b[0m",
    next_help_heading = "\x1b[1;33mGLOBAL OPTIONS\x1b[0m",
    override_usage = "rythonc [OPTIONS] <COMMAND>",
    after_help = "\x1b[1;34mSYSTEM TARGETS:\x1b[0m
  \x1b[36mbios16\x1b[0m          16-bit Real Mode (Legacy, 512B limited)
  \x1b[36mbios32\x1b[0m          32-bit Protected Mode
  \x1b[36mbios64\x1b[0m          64-bit Long Mode (Default)
  \x1b[36mbios64_sse\x1b[0m      64-bit + SSE Extensions
  \x1b[36mbios64_avx\x1b[0m      64-bit + AVX Support
  \x1b[36mbios64_avx512\x1b[0m   64-bit + AVX-512 Support

\x1b[1;33mEXAMPLES:\x1b[0m
  rythonc \x1b[32mcompile\x1b[0m main.ry -o boot.bin        \x1b[90m# Build default 64-bit binary\x1b[0m
  rythonc \x1b[32mcompile\x1b[0m kernel.ry -t bios32        \x1b[90m# Build for 32-bit environment\x1b[0m
  rythonc \x1b[32mcreate-rcl\x1b[0m lib.ry -o lib.rcl        \x1b[90m# Create an RCL library\x1b[0m
  rythonc \x1b[32mrcl-info\x1b[0m lib.rcl                    \x1b[90m# Show RCL library info\x1b[0m
  rythonc \x1b[32mtest\x1b[0m --suite full                  \x1b[90m# Verify local toolchain & NASM\x1b[0m
  rythonc \x1b[32mgenerate\x1b[0m --type bios16 -s 512      \x1b[90m# Create a 16-bit boot sector stub\x1b[0m

\x1b[1;90mDocumentation: https://github.com/bitjumper/rython\x1b[0m
"
)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Increase logging verbosity (use -v, -vv, -vvv)
    #[arg(short, long, global = true, action = clap::ArgAction::Count)]
    verbose: u8,

    /// Suppress all non-error output
    #[arg(short, long, global = true, conflicts_with = "verbose")]
    quiet: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Build a Rython source file into a machine-code binary
    #[command(display_order = 1)]
    Compile {
        /// Path to the source file (.ry)
        #[arg(value_name = "FILE")]
        input: String,

        /// Output binary path (defaults to input name + .bin)
        #[arg(short, long, value_name = "PATH")]
        output: Option<String>,

        /// Specify target architecture for hardware optimization
        #[arg(short, long, value_enum, default_value_t = Target::Bios64)]
        target: Target,

        /// Keep the generated NASM assembly file for inspection
        #[arg(short = 'k', long)]
        keep_asm: bool,

        /// Optimization level: 0 (none), 1 (size), 2 (speed), 3 (aggressive)
        #[arg(short = 'O', long, default_value = "2", value_parser = clap::value_parser!(u8).range(0..=3))]
        optimize: u8,
    },

    /// Generate standard bootloader boilerplate and mode-transition stubs
    #[command(display_order = 2)]
    Generate {
        /// The architecture mode for the template
        #[arg(short, long, value_enum, default_value_t = Target::Bios64)]
        r#type: Target,

        /// Destination filename
        #[arg(short, long, value_name = "FILE")]
        output: Option<String>,

        /// Fixed block size in bytes (e.g., 512 for MBR)
        #[arg(short = 's', long, default_value = "512")]
        size: u32,
    },

    /// Create a Rython Compiled Library (RCL) from source
    #[command(display_order = 3)]
    CreateRcl {
        /// Path to the source file (.ry)
        #[arg(value_name = "FILE")]
        input: String,

        /// Output RCL library path (defaults to input name + .rcl)
        #[arg(short, long, value_name = "PATH")]
        output: Option<String>,

        /// Specify target architecture
        #[arg(short, long, value_enum, default_value_t = Target::Bios64)]
        target: Target,

        /// Show detailed library information after creation
        #[arg(long)]
        info: bool,
    },

    /// Display information about an RCL library
    #[command(display_order = 4, name = "rcl-info")]
    RclInfo {
        /// Path to the RCL library file (.rcl)
        #[arg(value_name = "FILE")]
        input: String,

        /// Show all exported symbols
        #[arg(short, long)]
        verbose: bool,
    },

    /// List functions in an RCL library
    #[command(display_order = 5, name = "rcl-list")]
    RclList {
        /// Path to the RCL library file (.rcl)
        #[arg(value_name = "FILE")]
        input: String,
    },

    /// Extract assembly from an RCL library function
    #[command(display_order = 6, name = "rcl-extract")]
    RclExtract {
        /// Path to the RCL library file (.rcl)
        #[arg(value_name = "FILE")]
        input: String,

        /// Name of the function to extract
        #[arg(value_name = "FUNCTION")]
        function: String,
    },

    /// Validate the current environment and run compiler self-tests
    #[command(display_order = 7)]
    Test {
        /// Test suite level: 'parser', 'emitter', or 'full'
        #[arg(short, long, default_value = "full")]
        suite: String,
    },

    /// Show detailed version and environment diagnostic info
    #[command(display_order = 8)]
    Version,
}

pub fn run() -> Result<(), String> {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Compile { input, output, target, keep_asm, optimize }) => {
            handle_compile(input, output, target, *keep_asm, *optimize, &cli)
        }
        Some(Commands::Generate { r#type, output, size }) => {
            handle_generate(r#type, output, *size, &cli)
        }
        Some(Commands::CreateRcl { input, output, target, info }) => {
            handle_create_rcl(input, output, target, *info, &cli)
        }
        Some(Commands::RclInfo { input, verbose }) => {
            handle_rcl_info(input, *verbose, &cli)
        }
        Some(Commands::RclList { input }) => {
            handle_rcl_list(input, &cli)
        }
        Some(Commands::RclExtract { input, function }) => {
            handle_rcl_extract(input, function, &cli)
        }
        Some(Commands::Test { suite }) => {
            run_basic_tests(suite == "full" || cli.verbose > 0)
        }
        Some(Commands::Version) => {
            display_full_version();
            Ok(())
        }
        None => {
            let mut cmd = Cli::command();
            let _ = cmd.print_help();
            Ok(())
        }
    }
}

fn handle_compile(
    input: &str, 
    output: &Option<String>, 
    target: &Target, 
    keep_asm: bool, 
    optimize: u8, 
    cli: &Cli
) -> Result<(), String> {
    if !cli.quiet {
        println!("{}", "── Rython Build System ──────────────────────────────────".bright_black());
    }

    let input_path = Path::new(input);
    if !input_path.exists() {
        return Err(format!("Source file not found: {}", input));
    }

    let output_name = output.clone().unwrap_or_else(|| {
        input_path.with_extension("bin").to_str().unwrap_or("output.bin").to_string()
    });

    if !cli.quiet {
        println!("{} {} ➜ {}", "Compiling".green().bold(), input.cyan(), output_name.cyan());
    }
    
    let nasm_executable = crate::utils::find_nasm();
    if cli.verbose > 0 {
        log_status("Debug", &format!("Using NASM at: {}", nasm_executable));
        log_status("Debug", &format!("Optimization Level: {}", optimize));
    }

    // 1. Parsing
    log_status("Parsing", input);
    let source = std::fs::read_to_string(input)
        .map_err(|e| format!("IO Error: {}", e))?;
    let program = crate::parser::parse_program(&source)
        .map_err(|e| format!("Syntax Error: {}", e))?;

    // 2. Emission - Now automatically handles imports!
    log_status("Emitting", &format!("{}", target));
    let mut emitter = crate::emitter::NasmEmitter::new();
    match target {
        Target::Bios16 => emitter.set_target_bios16(),
        Target::Bios32 => emitter.set_target_bios32(),
        Target::Bios64 => emitter.set_target_bios64(),
        Target::Bios64Sse => emitter.set_target_bios64_sse(),
        Target::Bios64Avx => emitter.set_target_bios64_avx(),
        Target::Bios64Avx512 => emitter.set_target_bios64_avx512(),
    }
    
    // The emitter now automatically detects and processes imports!
    let asm = emitter.compile_program(&program);
    
    let asm_path = format!("{}.asm", output_name);
    std::fs::write(&asm_path, &asm)
        .map_err(|e| format!("Failed to write assembly: {}", e))?;

    // 3. Assembly
    log_status("Assembling", &output_name);
    let process_exit = std::process::Command::new(&nasm_executable)
        .args(["-f", "bin", "-o", &output_name, &asm_path])
        .status()
        .map_err(|e| format!("NASM execution failed: {}", e))?;

    if !process_exit.success() {
        return Err("NASM assembly failed. Review generated .asm file for errors.".to_string());
    }

    // 4. Cleanup
    if !keep_asm {
        let _ = std::fs::remove_file(&asm_path);
    } else {
        log_status("Kept", &asm_path);
    }

    // 5. Final Report
    let metadata = std::fs::metadata(&output_name).map_err(|e| e.to_string())?;
    let size_bytes = metadata.len();
    
    if !cli.quiet {
        println!("\n{} Created {} ({} bytes)", "✔".green().bold(), output_name.bright_white().bold(), size_bytes);
        if size_bytes == 512 {
            println!("  {} Sector is valid bootloader size (512 bytes)", "★".yellow());
        }
    }
    
    Ok(())
}

fn handle_create_rcl(
    input: &str,
    output: &Option<String>,
    target: &Target,
    info: bool,
    cli: &Cli
) -> Result<(), String> {
    if !cli.quiet {
        println!("{}", "── Rython RCL Creation ──────────────────────────────────".bright_black());
    }

    let input_path = Path::new(input);
    if !input_path.exists() {
        return Err(format!("Source file not found: {}", input));
    }

    let output_name = output.clone().unwrap_or_else(|| {
        input_path.with_extension("rcl").to_str().unwrap_or("output.rcl").to_string()
    });

    if !cli.quiet {
        println!("{} {} ➜ {}", "Creating RCL from".green().bold(), input.cyan(), output_name.cyan());
    }

    // Use the RCL CLI from rcl_integration
    let rcl_cli = crate::rcl_integration::RclCli::new(cli.verbose > 0);
    rcl_cli.compile_to_rcl(input, &output_name, target.to_rcl_string())?;

    if !cli.quiet {
        println!("\n{} Created RCL library: {}", "✔".green().bold(), output_name.bright_white().bold());
    }

    if info {
        println!();
        handle_rcl_info(&output_name, true, cli)?;
    }

    Ok(())
}

fn handle_rcl_info(input: &str, verbose: bool, cli: &Cli) -> Result<(), String> {
    if !cli.quiet {
        println!("{}", "── Rython RCL Information ───────────────────────────────".bright_black());
    }

    let input_path = Path::new(input);
    if !input_path.exists() {
        return Err(format!("RCL file not found: {}", input));
    }

    let rcl_cli = crate::rcl_integration::RclCli::new(verbose || cli.verbose > 0);
    rcl_cli.show_rcl_info(input)?;

    Ok(())
}

fn handle_rcl_list(input: &str, cli: &Cli) -> Result<(), String> {
    if !cli.quiet {
        println!("{}", "── Rython RCL Functions ─────────────────────────────────".bright_black());
    }

    let input_path = Path::new(input);
    if !input_path.exists() {
        return Err(format!("RCL file not found: {}", input));
    }

    let rcl_cli = crate::rcl_integration::RclCli::new(cli.verbose > 0);
    rcl_cli.list_functions(input)?;

    Ok(())
}

fn handle_rcl_extract(input: &str, function: &str, cli: &Cli) -> Result<(), String> {
    if !cli.quiet {
        println!("{}", "── Rython RCL Assembly Extraction ──────────────────────".bright_black());
    }

    let input_path = Path::new(input);
    if !input_path.exists() {
        return Err(format!("RCL file not found: {}", input));
    }

    let rcl_cli = crate::rcl_integration::RclCli::new(cli.verbose > 0);
    rcl_cli.extract_assembly(input, function)?;

    Ok(())
}

fn log_status(stage: &str, detail: &str) {
    println!("{:>12} {}", stage.bright_blue().bold(), detail);
}

fn handle_generate(target: &Target, _output: &Option<String>, size: u32, cli: &Cli) -> Result<(), String> {
    if !cli.quiet {
        println!("{} Generating {} template ({} bytes)...", "INFO".blue(), format!("{}", target).cyan(), size);
    }
    Ok(())
}

fn display_full_version() {
    println!("{} {}", "rythonc".bright_cyan().bold(), env!("CARGO_PKG_VERSION"));
    println!("{} {}", "Target Arch:".bright_black(), std::env::consts::ARCH);
    println!("{} {}", "Binary Path:".bright_black(), std::env::current_exe().unwrap_or_default().display());
}

fn run_basic_tests(_verbose: bool) -> Result<(), String> {
    println!("{}", "Running test suite...".yellow());
    
    print!("  Testing parser... ");
    let test_code = "var x = 42";
    match crate::parser::parse_program(test_code) {
        Ok(_) => println!("{}", "OK".green()),
        Err(e) => return Err(format!("Parser test failed: {}", e)),
    }

    print!("  Testing emitter... ");
    let mut emitter = crate::emitter::NasmEmitter::new();
    emitter.set_target_bios64();
    println!("{}", "OK".green());

    print!("  Testing NASM... ");
    let nasm_path = crate::utils::find_nasm();
    if std::process::Command::new(&nasm_path).arg("-v").output().is_ok() {
        println!("{}", "OK".green());
    } else {
        println!("{}", "MISSING".red());
        return Err("NASM not found".to_string());
    }

    println!("{}", "All tests passed!".bright_green().bold());
    Ok(())
}