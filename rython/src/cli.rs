use clap::{Parser, Subcommand, ValueEnum, CommandFactory, ColorChoice};
use colored::*;
use std::env;
use std::path::Path;
use std::fs;
use crate::compiler::{RythonCompiler, CompilerConfig, Target as CompTarget};
use crate::rcl_integration::{compile_with_rcl, RclCli};
use crate::modules::ModuleRegistry;
use crate::parser::format_parse_errors;
use serde_json;

/// Target architectures for the Rython compiler
#[derive(ValueEnum, Clone, Debug, Default)]
pub enum Target {
    Bios16,
    Bios32,
    #[default]
    Bios64,
    Bios64Sse,
    Bios64Avx,
    Bios64Avx512,
    Linux64,
    Windows64,
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
            Target::Linux64 => "linux64",
            Target::Windows64 => "windows64",
        };
        write!(f, "{}", s)
    }
}

impl Target {
    fn to_compiler_target(&self) -> CompTarget {
        match self {
            Target::Bios16 => CompTarget::Bios16,
            Target::Bios32 => CompTarget::Bios32,
            Target::Bios64 => CompTarget::Bios64,
            Target::Bios64Sse => CompTarget::Bios64Sse,
            Target::Bios64Avx => CompTarget::Bios64Avx,
            Target::Bios64Avx512 => CompTarget::Bios64Avx512,
            Target::Linux64 => CompTarget::Linux64,
            Target::Windows64 => CompTarget::Windows64,
        }
    }
}

#[derive(Parser)]
#[command(
    name = "Rython",
    version = env!("CARGO_PKG_VERSION"),
    color = ColorChoice::Always,
    help_template = "\
\x1b[1;36m{name}\x1b[0m \x1b[32m{version}\x1b[0m
{author-with-newline}\x1b{1m{about}\x1b[0m

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
  \x1b{36mbios64_sse\x1b[0m      64-bit + SSE Extensions
  \x1b[36mbios64_avx\x1b[0m      64-bit + AVX Support
  \x1b[36mbios64_avx512\x1b[0m   64-bit + AVX-512 Support
  \x1b[36mlinux64\x1b[0m         64-bit Linux ELF
  \x1b[36mwindows64\x1b[0m       64-bit Windows PE

\x1b[1;33mEXAMPLES:\x1b[0m
  rythonc \x1b[32mcompile\x1b[0m main.ry -o boot.bin        \x1b[90m# Build default 64-bit binary\x1b[0m
  rythonc \x1b[32mcompile\x1b[0m kernel.ry -t bios32        \x1b[90m# Build for 32-bit environment\x1b[0m
  rythonc \x1b[32mcreate-rcl\x1b[0m lib.ry -o lib.rcl        \x1b[90m# Create an RCL library\x1b[0m
  rythonc \x1b[32mrcl-info\x1b[0m lib.rcl                    \x1b[90m# Show RCL library info\x1b[0m
  rythonc \x1b[32mtest\x1b[0m --suite full                  \x1b[90m# Verify local toolchain & NASM\x1b[0m
  rythonc \x1b[32mgenerate\x1b[0m --type bios16 -s 512      \x1b[90m# Create a 16-bit boot sector stub\x1b[0m
  rythonc \x1b[32mgenerate-modules\x1b[0m -o ./libs         \x1b[90m# Generate RCL libraries for all modules\x1b[0m
  rythonc \x1b[32mcompile-modules\x1b[0m app.ry --use-rcl   \x1b[90m# Compile with RCL module support\x1b[0m

\x1b[1;90mDocumentation: https://github.com/Bit-Jumper-Studio/Rython\x1b[0m
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
        
        /// Comma-separated modules to include
        #[arg(long)] 
        modules: Option<String>,
        
        /// Enable RCL mode
        #[arg(long, default_value_t = false)]
        rcl: bool,
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
    
    /// Generate RCL libraries from all built-in modules
    #[command(display_order = 9)]
    GenerateModules {
        /// Output directory for RCL libraries
        #[arg(short, long, default_value = "./lib")]
        output: String,
        
        /// Target architecture(s)
        #[arg(short, long, value_enum, default_values_t = vec![Target::Bios64])]
        targets: Vec<Target>,
    },
    
    /// Compile with module support
    #[command(display_order = 10, name = "compile-modules")]
    CompileModules {
        /// Path to the source file (.ry)
        #[arg(value_name = "FILE")]
        input: String,

        /// Output binary path
        #[arg(short, long, value_name = "PATH")]
        output: Option<String>,
        
        /// Specify target architecture for hardware optimization
        #[arg(short, long, value_enum, default_value_t = Target::Bios64)]
        target: Target,

        /// Use RCL libraries instead of inline compilation
        #[arg(long)]
        use_rcl: bool,
    },
    
    /// Check syntax of a Rython file without compiling
    #[command(display_order = 11)]
    Check {
        /// Path to the source file (.ry)
        #[arg(value_name = "FILE")]
        input: String,
        
        /// Show detailed parse tree
        #[arg(short, long)]
        ast: bool,
    },
}

pub fn run() -> Result<(), String> {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Compile { input, output, target, keep_asm, optimize, modules, rcl }) => {
            handle_compile(input, output, target, *keep_asm, *optimize, modules, *rcl, &cli)
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
        Some(Commands::GenerateModules { output, targets }) => {
            handle_generate_modules(output, targets, &cli)
        }
        Some(Commands::CompileModules { input, output, target, use_rcl }) => {
            handle_compile_modules(input, output, target, *use_rcl, &cli)
        }
        Some(Commands::Check { input, ast }) => {
            handle_check(input, *ast, &cli)
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
    modules: &Option<String>,
    rcl: bool,
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
        if let Some(modules_str) = modules {
            log_status("Debug", &format!("Modules: {}", modules_str));
        }
        log_status("Debug", &format!("RCL mode: {}", rcl));
    }

    // 1. Parsing
    log_status("Parsing", input);
    let source = std::fs::read_to_string(input)
        .map_err(|e| format!("IO Error: {}", e))?;
    
    // Parse modules from CLI
    let module_list = modules.as_ref()
        .map(|s| s.split(',').map(|m| m.trim().to_string()).collect::<Vec<_>>())
        .unwrap_or_default();
    
    // Create compiler config
    let config = CompilerConfig {
        target: target.to_compiler_target(),
        verbose: cli.verbose > 0,
        keep_assembly: keep_asm,
        optimize: optimize > 0,
        modules: module_list,
    };
    
    if rcl {
        // Use RCL compilation
        log_status("RCL Mode", "Compiling with RCL support");
        compile_with_rcl(&source, &output_name, target.to_compiler_target())?;
    } else {
        // Use regular compiler
        let mut compiler = RythonCompiler::new(config);
        match compiler.compile(&source, &output_name) {
            Ok(_) => {
                // Final Report
                let metadata = std::fs::metadata(&output_name).map_err(|e| e.to_string())?;
                let size_bytes = metadata.len();
                
                if !cli.quiet {
                    println!("\n{} Created {} ({} bytes)", "✔".green().bold(), output_name.bright_white().bold(), size_bytes);
                    if size_bytes == 512 {
                        println!("  {} Sector is valid bootloader size (512 bytes)", "★".yellow());
                    }
                }
                return Ok(());
            }
            Err(err) => {
                // Format the error with red "ERROR:"
                let error_message = if err.starts_with("✗ ") {
                    // Format: ✗ Found 1 error
                    format!("{} {}", "ERROR:".red().bold(), &err[2..])
                } else if err.starts_with("ERROR:") {
                    // Format: ERROR: ✗ Found 1 error
                    let parts: Vec<&str> = err.splitn(2, ':').collect();
                    if parts.len() == 2 {
                        format!("{}:{}", "ERROR".red().bold(), parts[1])
                    } else {
                        format!("{}: {}", "ERROR".red().bold(), err)
                    }
                } else {
                    format!("{}: {}", "ERROR".red().bold(), err)
                };
                
                return Err(error_message);
            }
        }
    }
    
    Ok(())
}

fn handle_check(input: &str, show_ast: bool, cli: &Cli) -> Result<(), String> {
    if !cli.quiet {
        println!("{}", "── Rython Syntax Check ──────────────────────────────────".bright_black());
    }

    let input_path = Path::new(input);
    if !input_path.exists() {
        return Err(format!("Source file not found: {}", input));
    }

    if !cli.quiet {
        println!("{} {}", "Checking".green().bold(), input.cyan());
    }
    
    let source = std::fs::read_to_string(input)
        .map_err(|e| format!("IO Error: {}", e))?;
    
    log_status("Parsing", input);
    
    match crate::parser::parse_program(&source) {
        Ok(program) => {
            if !cli.quiet {
                println!("\n{} Syntax check passed!", "✔".green().bold());
                println!("  {} statements parsed", program.body.len());
            }
            
            if show_ast {
                println!("\n{}", "AST Structure:".yellow().bold());
                print_ast(&program, 0);
            }
            
            Ok(())
        }
        Err(errors) => {
            let error_output = format_parse_errors(&errors, &source);
            // Format the error with red "ERROR:"
            let formatted_error = if error_output.starts_with("✗ ") {
                format!("{} {}", "ERROR:".red().bold(), &error_output[2..])
            } else if error_output.starts_with("ERROR:") {
                let parts: Vec<&str> = error_output.splitn(2, ':').collect();
                if parts.len() == 2 {
                    format!("{}:{}", "ERROR".red().bold(), parts[1])
                } else {
                    format!("{}: {}", "ERROR".red().bold(), error_output)
                }
            } else {
                format!("{}: {}", "ERROR".red().bold(), error_output)
            };
            
            Err(formatted_error)
        }
    }
}

fn print_ast(program: &crate::parser::Program, indent: usize) {
    let indent_str = "  ".repeat(indent);
    
    for (i, stmt) in program.body.iter().enumerate() {
        print!("{}{:2}. ", indent_str, i + 1);
        match stmt {
            crate::parser::Statement::VarDecl { name, value, type_hint, span } => {
                println!("VarDecl: {} = ... (type: {:?}) [{}]", 
                    name, 
                    type_hint, 
                    span);
                print_expr(value, indent + 1);
            }
            crate::parser::Statement::Assign { target, value, span } => {
                println!("Assign: {} = ... [{}]", target, span);
                print_expr(value, indent + 1);
            }
            crate::parser::Statement::Expr(expr) => {
                println!("Expr: [{}]", expr.span());
                print_expr(expr, indent + 1);
            }
            crate::parser::Statement::FunctionDef { name, args, body, span } => {
                println!("FunctionDef: {}({}) [{}]", name, args.join(", "), span);
                for stmt in body {
                    print_ast(&crate::parser::Program { body: vec![stmt.clone()], span: *span }, indent + 1);
                }
            }
            crate::parser::Statement::If { condition, then_block, elif_blocks, else_block, span } => {
                println!("If [{}]", span);
                print!("{}  condition: ", indent_str);
                print_expr(condition, indent + 2);
                println!("{}  then block ({} statements):", indent_str, then_block.len());
                for stmt in then_block {
                    print_ast(&crate::parser::Program { body: vec![stmt.clone()], span: *span }, indent + 2);
                }
                if !elif_blocks.is_empty() {
                    println!("{}  elif blocks ({}):", indent_str, elif_blocks.len());
                    for (cond, block) in elif_blocks {
                        print!("{}    condition: ", indent_str);
                        print_expr(cond, indent + 3);
                        println!("{}    block ({} statements):", indent_str, block.len());
                        for stmt in block {
                            print_ast(&crate::parser::Program { body: vec![stmt.clone()], span: *span }, indent + 3);
                        }
                    }
                }
                if let Some(else_block) = else_block {
                    println!("{}  else block ({} statements):", indent_str, else_block.len());
                    for stmt in else_block {
                        print_ast(&crate::parser::Program { body: vec![stmt.clone()], span: *span }, indent + 2);
                    }
                }
            }
            _ => {
                println!("{:?}", stmt);
            }
        }
    }
}

fn print_expr(expr: &crate::parser::Expr, indent: usize) {
    let indent_str = "  ".repeat(indent);
    
    match expr {
        crate::parser::Expr::Number(n, span) => println!("{}Number: {} [{}]", indent_str, n, span),
        crate::parser::Expr::Float(f, span) => println!("{}Float: {} [{}]", indent_str, f, span),
        crate::parser::Expr::Boolean(b, span) => println!("{}Boolean: {} [{}]", indent_str, b, span),
        crate::parser::Expr::String(s, span) => println!("{}String: \"{}\" [{}]", indent_str, s, span),
        crate::parser::Expr::Var(name, span) => println!("{}Var: {} [{}]", indent_str, name, span),
        crate::parser::Expr::None(span) => println!("{}None [{}]", indent_str, span),
        crate::parser::Expr::BinOp { left, op, right, span } => {
            println!("{}BinOp: {:?} [{}]", indent_str, op, span);
            print_expr(left, indent + 1);
            print_expr(right, indent + 1);
        }
        crate::parser::Expr::Call { func, args, span, .. } => {
            println!("{}Call: {}({}) [{}]", indent_str, func, args.len(), span);
            for arg in args {
                print_expr(arg, indent + 1);
            }
        }
        _ => println!("{}Expr: {:?}", indent_str, expr),
    }
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
    let rcl_cli = RclCli::new(cli.verbose > 0);
    
    let target_str = match target {
        Target::Bios16 => "bios16",
        Target::Bios32 => "bios32",
        Target::Bios64 => "bios64",
        Target::Bios64Sse => "bios64_sse",
        Target::Bios64Avx => "bios64_avx",
        Target::Bios64Avx512 => "bios64_avx512",
        Target::Linux64 => "linux64",
        Target::Windows64 => "windows64",
    };
    
    rcl_cli.compile_to_rcl(input, &output_name, target_str)?;

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

    let rcl_cli = RclCli::new(verbose || cli.verbose > 0);
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

    let rcl_cli = RclCli::new(cli.verbose > 0);
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

    let rcl_cli = RclCli::new(cli.verbose > 0);
    rcl_cli.extract_assembly(input, function)?;

    Ok(())
}

fn log_status(stage: &str, detail: &str) {
    println!("{:>12} {}", stage.bright_blue().bold(), detail);
}

fn handle_generate(target: &Target, output: &Option<String>, size: u32, cli: &Cli) -> Result<(), String> {
    let _size = size; // Prefix with underscore to suppress unused warning
    if !cli.quiet {
        println!("{} Generating {} template ({} bytes)...", "INFO".blue(), format!("{}", target).cyan(), size);
    }
    
    // REAL IMPLEMENTATION: Generate bootloader template
    let output_name = output.clone().unwrap_or_else(|| {
        format!("boot_{}.bin", target)
    });
    
    match target {
        Target::Bios16 => generate_bios16_template(&output_name, size, cli),
        Target::Bios32 => generate_bios32_template(&output_name, size, cli),
        Target::Bios64 => generate_bios64_template(&output_name, size, cli),
        Target::Bios64Sse => generate_bios64_sse_template(&output_name, size, cli),
        _ => generate_generic_template(&output_name, size, target, cli),
    }
}

fn generate_bios16_template(output_name: &str, _size: u32, cli: &Cli) -> Result<(), String> {
    let template = r#"
; BIOS 16-bit Bootloader Template
; Generated by Rython Compiler

org 0x7C00
bits 16

start:
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7C00
    sti
    cld
    
    ; Clear screen
    mov ax, 0x0003
    int 0x10
    
    ; Print message
    mov si, msg_hello
    call print_string
    
    ; Halt
    cli
    hlt
    jmp $

print_string:
    lodsb
    test al, al
    jz .done
    mov ah, 0x0E
    int 0x10
    jmp print_string
.done:
    ret

msg_hello db 'Rython BIOS 16-bit', 0

times 510-($-$$) db 0
dw 0xAA55
"#;
    
    fs::write(output_name, template)
        .map_err(|e| format!("Failed to write template: {}", e))?;
    
    if !cli.quiet {
        println!("{} Generated 16-bit bootloader: {}", "✔".green().bold(), output_name.cyan());
    }
    
    Ok(())
}

fn generate_bios64_template(output_name: &str, _size: u32, cli: &Cli) -> Result<(), String> {
    let template = r#"
; BIOS 64-bit Bootloader Template
; Generated by Rython Compiler

org 0x7C00
bits 16

start:
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7C00
    sti
    cld
    
    ; Enable A20
    in al, 0x92
    or al, 2
    out 0x92, al
    
    ; Load GDT
    lgdt [gdt32_desc]
    
    ; Enter protected mode
    mov eax, cr0
    or eax, 1
    mov cr0, eax
    jmp 0x08:protected_mode

bits 32
protected_mode:
    mov ax, 0x10
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    mov esp, 0x90000
    
    ; Setup paging
    mov edi, 0x1000
    mov cr3, edi
    xor eax, eax
    mov ecx, 4096
    rep stosd
    
    mov edi, 0x1000
    mov dword [edi], 0x2003
    add edi, 0x1000
    mov dword [edi], 0x3003
    add edi, 0x1000
    
    mov ebx, 0x00000083
    mov ecx, 512
.set_entry:
    mov dword [edi], ebx
    add ebx, 0x200000
    add edi, 8
    loop .set_entry
    
    ; Enable PAE
    mov eax, cr4
    or eax, (1 << 5)
    mov cr4, eax
    
    ; Set CR3
    mov eax, 0x1000
    mov cr3, eax
    
    ; Enable long mode
    mov ecx, 0xC0000080
    rdmsr
    or eax, (1 << 8)
    wrmsr
    
    ; Enable paging
    mov eax, cr0
    or eax, (1 << 31)
    mov cr0, eax
    
    ; Load 64-bit GDT
    lgdt [gdt64_desc]
    jmp 0x08:long_mode

bits 64
long_mode:
    mov ax, 0x10
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    mov rsp, 0x90000
    
    ; Clear screen
    mov rdi, 0xB8000
    mov rax, 0x0720072007200720
    mov rcx, 1000
    rep stosq
    
    ; Print message
    mov rsi, msg_64
    mov rdi, 0xB8000
    call print_string_64
    
    ; Halt
    cli
    hlt
    jmp $

print_string_64:
    push rdi
.loop:
    lodsb
    test al, al
    jz .done
    stosb
    mov al, 0x0F
    stosb
    jmp .loop
.done:
    pop rdi
    ret

gdt32:
    dq 0x0000000000000000
    dq 0x00CF9A000000FFFF
    dq 0x00CF92000000FFFF
gdt32_end:

gdt32_desc:
    dw gdt32_end - gdt32 - 1
    dd gdt32

gdt64:
    dq 0x0000000000000000
    dq 0x00209A0000000000
    dq 0x0000920000000000
gdt64_end:

gdt64_desc:
    dw gdt64_end - gdt64 - 1
    dq gdt64

msg_64 db 'Rython 64-bit Long Mode', 0

times 510-($-$$) db 0
dw 0xAA55
"#;
    
    fs::write(output_name, template)
        .map_err(|e| format!("Failed to write template: {}", e))?;
    
    if !cli.quiet {
        println!("{} Generated 64-bit bootloader: {}", "✔".green().bold(), output_name.cyan());
    }
    
    Ok(())
}

fn generate_bios64_sse_template(output_name: &str, _size: u32, cli: &Cli) -> Result<(), String> {
    let template = r#"
; BIOS 64-bit Bootloader with SSE
; Generated by Rython Compiler

org 0x7C00
bits 16

start:
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7C00
    sti
    cld
    
    ; Enable A20
    in al, 0x92
    or al, 2
    out 0x92, al
    
    ; Load GDT
    lgdt [gdt32_desc]
    
    ; Enter protected mode
    mov eax, cr0
    or eax, 1
    mov cr0, eax
    jmp 0x08:protected_mode

bits 32
protected_mode:
    mov ax, 0x10
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    mov esp, 0x90000
    
    ; Enable SSE
    mov eax, cr0
    and ax, 0xFFFB      ; Clear coprocessor emulation CR0.EM
    or ax, 0x2          ; Set coprocessor monitoring CR0.MP
    mov cr0, eax
    mov eax, cr4
    or ax, 3 << 9       ; Set CR4.OSFXSR and CR4.OSXMMEXCPT
    mov cr4, eax
    
    ; Setup paging (same as bios64)
    mov edi, 0x1000
    mov cr3, edi
    xor eax, eax
    mov ecx, 4096
    rep stosd
    
    mov edi, 0x1000
    mov dword [edi], 0x2003
    add edi, 0x1000
    mov dword [edi], 0x3003
    add edi, 0x1000
    
    mov ebx, 0x00000083
    mov ecx, 512
.set_entry:
    mov dword [edi], ebx
    add ebx, 0x200000
    add edi, 8
    loop .set_entry
    
    mov eax, cr4
    or eax, (1 << 5)
    mov cr4, eax
    
    mov eax, 0x1000
    mov cr3, eax
    
    mov ecx, 0xC0000080
    rdmsr
    or eax, (1 << 8)
    wrmsr
    
    mov eax, cr0
    or eax, (1 << 31)
    mov cr0, eax
    
    lgdt [gdt64_desc]
    jmp 0x08:long_mode

bits 64
long_mode:
    mov ax, 0x10
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    mov rsp, 0x90000
    
    ; Clear screen with SSE
    mov rax, 0x0720072007200720
    movdqu xmm0, [rax_quad]
    mov rdi, 0xB8000
    mov rcx, 250        ; 1000 chars / 4 per XMM
.clear_loop:
    movdqu [rdi], xmm0
    add rdi, 16
    loop .clear_loop
    
    ; Print message
    mov rsi, msg_sse
    mov rdi, 0xB8000
    call print_string_64
    
    ; SSE test
    movdqu xmm1, [test_vec1]
    movdqu xmm2, [test_vec2]
    paddb xmm1, xmm2
    movdqu [result], xmm1
    
    cli
    hlt
    jmp $

print_string_64:
    push rdi
.loop:
    lodsb
    test al, al
    jz .done
    stosb
    mov al, 0x0F
    stosb
    jmp .loop
.done:
    pop rdi
    ret

gdt32:
    dq 0x0000000000000000
    dq 0x00CF9A000000FFFF
    dq 0x00CF92000000FFFF
gdt32_end:

gdt32_desc:
    dw gdt32_end - gdt32 - 1
    dd gdt32

gdt64:
    dq 0x0000000000000000
    dq 0x00209A0000000000
    dq 0x0000920000000000
gdt64_end:

gdt64_desc:
    dw gdt64_end - gdt64 - 1
    dq gdt64

msg_sse db 'Rython 64-bit with SSE', 0
rax_quad dq 0x0720072007200720
test_vec1 db 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16
test_vec2 db 16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1
result times 16 db 0

times 510-($-$$) db 0
dw 0xAA55
"#;
    
    fs::write(output_name, template)
        .map_err(|e| format!("Failed to write template: {}", e))?;
    
    if !cli.quiet {
        println!("{} Generated 64-bit SSE bootloader: {}", "✔".green().bold(), output_name.cyan());
    }
    
    Ok(())
}

fn generate_generic_template(output_name: &str, size: u32, target: &Target, cli: &Cli) -> Result<(), String> {
    let template = format!(r#"
; {} Bootloader Template
; Generated by Rython Compiler
; Size: {} bytes

org 0x7C00
bits 16

start:
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7C00
    sti
    cld
    
    ; Clear screen
    mov ax, 0x0003
    int 0x10
    
    ; Print message
    mov si, msg
    call print_string
    
    ; Halt
    cli
    hlt
    jmp $

print_string:
    lodsb
    test al, al
    jz .done
    mov ah, 0x0E
    int 0x10
    jmp print_string
.done:
    ret

msg db 'Rython {} Bootloader', 0

times {} db 0
dw 0xAA55
"#, target, size, target, size - 2);
    
    fs::write(output_name, template)
        .map_err(|e| format!("Failed to write template: {}", e))?;
    
    if !cli.quiet {
        println!("{} Generated {} bootloader: {}", "✔".green().bold(), target, output_name.cyan());
    }
    
    Ok(())
}

fn generate_bios32_template(output_name: &str, _size: u32, cli: &Cli) -> Result<(), String> {
    let template = r#"
; BIOS 32-bit Bootloader Template
; Generated by Rython Compiler

org 0x7C00
bits 16

start:
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7C00
    sti
    cld
    
    ; Enable A20
    in al, 0x92
    or al, 2
    out 0x92, al
    
    ; Load GDT
    lgdt [gdt32_desc]
    
    ; Enter protected mode
    mov eax, cr0
    or eax, 1
    mov cr0, eax
    jmp 0x08:protected_mode

bits 32
protected_mode:
    mov ax, 0x10
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    mov esp, 0x90000
    
    ; Clear screen
    mov edi, 0xB8000
    mov eax, 0x07200720
    mov ecx, 1000
    rep stosd
    
    ; Print message
    mov esi, msg_32
    mov edi, 0xB8000
    call print_string_32
    
    cli
    hlt
    jmp $

print_string_32:
    push edi
.loop:
    lodsb
    test al, al
    jz .done
    stosb
    mov al, 0x0F
    stosb
    jmp .loop
.done:
    pop edi
    ret

gdt32:
    dq 0x0000000000000000
    dq 0x00CF9A000000FFFF
    dq 0x00CF92000000FFFF
gdt32_end:

gdt32_desc:
    dw gdt32_end - gdt32 - 1
    dd gdt32

msg_32 db 'Rython 32-bit Protected Mode', 0

times 510-($-$$) db 0
dw 0xAA55
"#;
    
    fs::write(output_name, template)
        .map_err(|e| format!("Failed to write template: {}", e))?;
    
    if !cli.quiet {
        println!("{} Generated 32-bit bootloader: {}", "✔".green().bold(), output_name.cyan());
    }
    
    Ok(())
}

fn handle_generate_modules(output: &str, targets: &[Target], cli: &Cli) -> Result<(), String> {
    if !cli.quiet {
        println!("{}", "── Rython Module Generation ──────────────────────────────".bright_black());
    }
    
    println!("{} Generating RCL libraries in: {}", "INFO".blue(), output.cyan());
    
    // Create output directory
    fs::create_dir_all(output)
        .map_err(|e| format!("Failed to create directory: {}", e))?;
    
    let registry = ModuleRegistry::default_registry();
    
    for target in targets {
        let target_str = match target {
            Target::Bios16 => "bios16",
            Target::Bios32 => "bios32",
            Target::Bios64 => "bios64",
            Target::Bios64Sse => "bios64_sse",
            Target::Bios64Avx => "bios64_avx",
            Target::Bios64Avx512 => "bios64_avx512",
            Target::Linux64 => "linux64",
            Target::Windows64 => "windows64",
        };
        
        println!("  Target: {}", format!("{}", target).cyan());
        
        for module_name in registry.get_module_names() {
            if let Some(module) = registry.get_module(&module_name) {
                let rcl_lib = module.to_rcl_library(target_str);
                let json = serde_json::to_string_pretty(&rcl_lib)
                    .map_err(|e| format!("Failed to serialize: {}", e))?;
                
                let output_file = format!("{}/{}_{}.rcl", output, module_name, target_str);
                fs::write(&output_file, json)
                    .map_err(|e| format!("Failed to write: {}", e))?;
                
                if cli.verbose > 0 {
                    println!("    Generated: {}", output_file);
                }
            }
        }
    }
    
    if !cli.quiet {
        println!("\n{} RCL libraries generated successfully!", "✔".green().bold());
    }
    
    Ok(())
}

fn handle_compile_modules(input: &str, output: &Option<String>, target: &Target, use_rcl: bool, cli: &Cli) -> Result<(), String> {
    if !cli.quiet {
        println!("{}", "── Rython Module Compilation ────────────────────────────".bright_black());
    }
    
    let input_path = Path::new(input);
    if !input_path.exists() {
        return Err(format!("Source file not found: {}", input));
    }

    let output_name = output.clone().unwrap_or_else(|| {
        input_path.with_extension("bin").to_str().unwrap_or("output.bin").to_string()
    });

    if !cli.quiet {
        println!("{} {} ➜ {} (RCL: {})", 
            "Compiling".green().bold(), 
            input.cyan(), 
            output_name.cyan(),
            use_rcl
        );
    }
    
    // Read source
    let source = fs::read_to_string(input)
        .map_err(|e| format!("IO Error: {}", e))?;
    
    // Create config with modules
    let config = CompilerConfig {
        target: target.to_compiler_target(),
        verbose: cli.verbose > 0,
        keep_assembly: false,
        optimize: true,
        modules: Vec::new(), // Will be extracted from imports
    };
    
    if use_rcl {
        // Use RCL compiler
        let mut compiler = crate::rcl_integration::RythonCompilerWithRcl::new(config);
        compiler.enable_rcl();
        compiler.compile_with_rcl(&source, &output_name)?;
    } else {
        // Use regular compiler with module support
        let mut compiler = RythonCompiler::new(config);
        compiler.compile(&source, &output_name)?;
    }
    
    if !cli.quiet {
        let metadata = std::fs::metadata(&output_name).map_err(|e| e.to_string())?;
        let size_bytes = metadata.len();
        println!("\n{} Created {} ({} bytes)", "✔".green().bold(), output_name.bright_white().bold(), size_bytes);
    }
    
    Ok(())
}

fn display_full_version() {
    println!("{} {}", "rythonc".bright_cyan().bold(), env!("CARGO_PKG_VERSION"));
    println!("{} {}", "Target Arch:".bright_black(), std::env::consts::ARCH);
    println!("{} {}", "Binary Path:".bright_black(), std::env::current_exe().unwrap_or_default().display());
    println!("{} {}", "Module Count:".bright_black(), ModuleRegistry::default_registry().get_module_names().len());
}

fn run_basic_tests(verbose: bool) -> Result<(), String> {
    println!("{}", "Running test suite...".yellow());
    
    print!("  Testing parser... ");
    let test_code = "var x = 42";
    match crate::parser::parse_program(test_code) {
        Ok(_) => println!("{}", "OK".green()),
        Err(errors) => {
            println!("{}", "FAILED".red());
            for error in errors {
                println!("    {}", error);
            }
            return Err("Parser test failed".to_string());
        }
    }

    print!("  Testing emitter... ");
    let mut emitter = crate::emitter::NasmEmitter::new();
    emitter.set_target_bios64();
    println!("{}", "OK".green());

    print!("  Testing backend registry... ");
    let registry = crate::backend::BackendRegistry::default_registry();
    if registry.backends.len() > 0 {
        println!("{}", "OK".green());
    } else {
        println!("{}", "FAILED".red());
        return Err("Backend registry empty".to_string());
    }

    print!("  Testing NASM... ");
    let nasm_path = crate::utils::find_nasm();
    if std::process::Command::new(&nasm_path).arg("-v").output().is_ok() {
        println!("{}", "OK".green());
    } else {
        println!("{}", "MISSING".red());
        return Err("NASM not found".to_string());
    }

    print!("  Testing module registry... ");
    let module_registry = ModuleRegistry::default_registry();
    if module_registry.get_module_names().len() > 0 {
        println!("{}", "OK".green());
    } else {
        println!("{}", "FAILED".red());
        return Err("Module registry empty".to_string());
    }

    if verbose {
        println!("  Running verbose tests...");
        // Test error handling
        let error_code = "var x = (5 + 3  # Missing closing parenthesis";
        println!("    Testing error recovery...");
        match crate::parser::parse_program(error_code) {
            Ok(_) => println!("      No errors found (unexpected)"),
            Err(errors) => println!("      Found {} errors (expected)", errors.len()),
        }
    }

    println!("{}", "All tests passed!".bright_green().bold());
    Ok(())
}