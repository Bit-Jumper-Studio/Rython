use crate::parser::{Program, Statement, Expr, Op};
use crate::backend::{Backend, BackendRegistry, Target, BackendModule, BackendFunction};
use crate::ssd_injector::SsdInjector;
use crate::rcl_compiler::RclImportManager;
use std::collections::HashMap;
use std::fs;
use std::process::Command;

/// Compiler configuration
#[derive(Debug, Clone)]
pub struct CompilerConfig {
    pub target: Target,
    pub verbose: bool,
    pub keep_assembly: bool,
    pub optimize: bool,
    pub modules: Vec<String>,
    pub ssd_headers: Vec<String>,
    pub ssd_assembly: Vec<String>,
    pub enable_ssd: bool,
    pub enable_rcl: bool,
    pub rcl_libraries: Vec<String>,
}

/// Main compiler
pub struct RythonCompiler {
    pub config: CompilerConfig,
    pub backend_registry: BackendRegistry,
    pub ssd_injector: Option<SsdInjector>,
    pub rcl_manager: Option<RclImportManager>,
    pub symbol_table: HashMap<String, String>,
    pub string_literals: HashMap<String, String>,
    pub current_scope: Vec<String>,
}

impl RythonCompiler {
    pub fn new(config: CompilerConfig) -> Self {
        Self {
            config,
            backend_registry: BackendRegistry::default_registry(),
            ssd_injector: None,
            rcl_manager: None,
            symbol_table: HashMap::new(),
            string_literals: HashMap::new(),
            current_scope: Vec::new(),
        }
    }
    
    pub fn compile(&mut self, source: &str, output_path: &str) -> Result<(), String> {
        if self.config.verbose {
            println!("[COMPILER] Starting compilation...");
        }
        
        // Initialize SSD if enabled
        if self.config.enable_ssd {
            self.initialize_ssd()?;
        }
        
        // Initialize RCL if enabled
        if self.config.enable_rcl {
            self.initialize_rcl()?;
        }
        
        // Parse the program
        let mut program = crate::parser::parse_program(source)
            .map_err(|e| format!("Parse error: {:?}", e))?;
        
        // Apply SSD syntax extensions if enabled
        if let Some(injector) = &self.ssd_injector {
            let mutated_source = injector.apply_syntax_extensions(source);
            program = crate::parser::parse_program(&mutated_source)
                .map_err(|e| format!("Parse error after SSD mutation: {:?}", e))?;
        }
        
        // Find appropriate backend
        let module = self.create_backend_module(&program)?;
        let _backend = self.backend_registry.find_backend(&module)
            .ok_or_else(|| "No suitable backend found for the target requirements".to_string())?;
        
        // Create a mutable backend (we need to clone and downcast)
        let mut backend_box: Box<dyn Backend> = match self.config.target {
            Target::Bios16 => Box::new(crate::backend::Bios16Backend::new()),
            Target::Bios32 => Box::new(crate::backend::Bios32Backend::new()),
            Target::Bios64 => Box::new(crate::backend::Bios64Backend::new()),
            Target::Bios64Sse => Box::new(crate::backend::Bios64Backend::new().with_sse()),
            Target::Bios64Avx => Box::new(crate::backend::Bios64Backend::new().with_avx()),
            Target::Bios64Avx512 => Box::new(crate::backend::Bios64Backend::new().with_avx512()),
            Target::Linux64 => Box::new(crate::backend::Linux64Backend::new()),
            Target::Windows64 => Box::new(crate::backend::Windows64Backend::new()),
        };
        
        // Compile to assembly
        let assembly = backend_box.compile_program(&program)?;
        
        // Write assembly file
        let asm_path = format!("{}.asm", output_path);
        fs::write(&asm_path, &assembly)
            .map_err(|e| format!("Failed to write assembly file: {}", e))?;
        
        if self.config.verbose {
            println!("[COMPILER] Assembly written to: {}", asm_path);
        }
        
        // Assemble to binary
        self.assemble_to_binary(&asm_path, output_path)?;
        
        // Clean up if not keeping assembly
        if !self.config.keep_assembly {
            fs::remove_file(&asm_path)
                .map_err(|e| format!("Failed to remove assembly file: {}", e))?;
        }
        
        if self.config.verbose {
            println!("[COMPILER] Compilation successful!");
            println!("[COMPILER] Output: {}", output_path);
        }
        
        Ok(())
    }
    
    fn initialize_ssd(&mut self) -> Result<(), String> {
        let mut injector = SsdInjector::new();
        
        // Load SSD headers
        for header_path in &self.config.ssd_headers {
            injector.load_header(header_path)?;
        }
        
        // Load SSD assembly blocks
        for asm_path in &self.config.ssd_assembly {
            injector.load_assembly_block(asm_path)?;
        }
        
        self.ssd_injector = Some(injector);
        Ok(())
    }
    
    fn initialize_rcl(&mut self) -> Result<(), String> {
        let mut manager = RclImportManager::new();
        
        // Load RCL libraries
        for lib_name in &self.config.rcl_libraries {
            manager.import_library(lib_name)?;
        }
        
        self.rcl_manager = Some(manager);
        Ok(())
    }
    
    fn create_backend_module(&self, program: &Program) -> Result<BackendModule, String> {
        let mut functions = Vec::new();
        let globals = Vec::new();
        let mut required_capabilities = Vec::new();
        
        // Extract functions from program
        for stmt in &program.body {
            if let Statement::FunctionDef { name, args, .. } = stmt {
                let parameters: Vec<(String, String)> = args.iter()
                    .map(|arg| (arg.clone(), "auto".to_string()))
                    .collect();
                
                functions.push(BackendFunction {
                    name: name.clone(),
                    parameters,
                    body: Vec::new(),
                });
            }
        }
        
        // Add capabilities based on target
        match self.config.target {
            Target::Bios16 => {
                required_capabilities.push(crate::backend::Capability::BIOS);
                required_capabilities.push(crate::backend::Capability::RealMode16);
                required_capabilities.push(crate::backend::Capability::PureMetal);
            }
            Target::Bios32 => {
                required_capabilities.push(crate::backend::Capability::BIOS);
                required_capabilities.push(crate::backend::Capability::ProtectedMode32);
                required_capabilities.push(crate::backend::Capability::PureMetal);
            }
            Target::Bios64 => {
                required_capabilities.push(crate::backend::Capability::BIOS);
                required_capabilities.push(crate::backend::Capability::LongMode64);
                required_capabilities.push(crate::backend::Capability::PureMetal);
            }
            Target::Bios64Sse => {
                required_capabilities.push(crate::backend::Capability::BIOS);
                required_capabilities.push(crate::backend::Capability::LongMode64);
                required_capabilities.push(crate::backend::Capability::PureMetal);
                required_capabilities.push(crate::backend::Capability::SSE);
            }
            Target::Bios64Avx => {
                required_capabilities.push(crate::backend::Capability::BIOS);
                required_capabilities.push(crate::backend::Capability::LongMode64);
                required_capabilities.push(crate::backend::Capability::PureMetal);
                required_capabilities.push(crate::backend::Capability::AVX);
            }
            Target::Bios64Avx512 => {
                required_capabilities.push(crate::backend::Capability::BIOS);
                required_capabilities.push(crate::backend::Capability::LongMode64);
                required_capabilities.push(crate::backend::Capability::PureMetal);
                required_capabilities.push(crate::backend::Capability::AVX512);
            }
            Target::Linux64 => {
                required_capabilities.push(crate::backend::Capability::Linux);
                required_capabilities.push(crate::backend::Capability::LongMode64);
            }
            Target::Windows64 => {
                required_capabilities.push(crate::backend::Capability::Windows);
                required_capabilities.push(crate::backend::Capability::LongMode64);
            }
        }
        
        Ok(BackendModule {
            functions,
            globals,
            required_capabilities,
            external_asm: Vec::new(),
            syntax_extensions: Vec::new(),
        })
    }
    
    fn assemble_to_binary(&self, asm_path: &str, output_path: &str) -> Result<(), String> {
    // Determine output format based on target
    let format = match self.config.target {
        Target::Bios16 | Target::Bios32 | Target::Bios64 | 
        Target::Bios64Sse | Target::Bios64Avx | Target::Bios64Avx512 => "bin",
        Target::Linux64 => "elf64",
        Target::Windows64 => "win64",
    };
    
    // Create command based on target
    let mut command = Command::new("nasm");
    command.arg("-f").arg(format);
    
    // Add additional flags for ELF format
    if format == "elf64" {
        command.arg("-g"); // Add debug info
        command.arg("-F").arg("dwarf"); // Use DWARF format
    }
    
    command.arg("-o").arg(output_path);
    command.arg(asm_path);
    
    if self.config.verbose {
        println!("[COMPILER] Running NASM command: {:?}", command);
    }
    
    let output = command
        .output()
        .map_err(|e| format!("Failed to run NASM: {}. Make sure NASM is installed and in PATH.", e))?;
    
    if !output.status.success() {
        let error_msg = String::from_utf8_lossy(&output.stderr);
        let output_msg = String::from_utf8_lossy(&output.stdout);
        
        let mut full_error = format!("NASM assembly failed:\nSTDERR: {}\n", error_msg);
        if !output_msg.is_empty() {
            full_error.push_str(&format!("STDOUT: {}\n", output_msg));
        }
        
        // Check for common errors
        if error_msg.contains("file not found") {
            full_error.push_str("\nPossible solution: Make sure the assembly file exists and the path is correct.");
        } else if error_msg.contains("unable to open input file") {
            full_error.push_str("\nPossible solution: Check file permissions and path.");
        } else if error_msg.contains("parser: instruction expected") {
            full_error.push_str("\nPossible solution: Check for syntax errors in generated assembly.");
        }
        
        return Err(full_error);
    }
    
    if self.config.verbose {
        println!("[COMPILER] NASM assembly successful");
        
        // Check if output file was created
        if std::path::Path::new(output_path).exists() {
            let metadata = std::fs::metadata(output_path)
                .map_err(|e| format!("Failed to get file metadata: {}", e))?;
            println!("[COMPILER] Output file created: {} ({} bytes)", output_path, metadata.len());
        } else {
            return Err(format!("Output file was not created: {}", output_path));
        }
    }
    
    Ok(())
}
    
    pub fn compile_statement(&mut self, stmt: &Statement) -> Result<String, String> {
        match stmt {
            Statement::Expr(expr) => self.compile_expression(expr),
            Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                let value_code = self.compile_expression(value)?;
                self.symbol_table.insert(name.clone(), "variable".to_string());
                Ok(format!("    ; Variable declaration: {}\n{}", name, value_code))
            }
            Statement::Assign { target, value, span: _ } => {
                let value_code = self.compile_expression(value)?;
                if self.symbol_table.contains_key(target) {
                    Ok(format!("    ; Assignment to existing variable: {}\n{}", target, value_code))
                } else {
                    self.symbol_table.insert(target.clone(), "variable".to_string());
                    Ok(format!("    ; Assignment to new variable: {}\n{}", target, value_code))
                }
            }
            Statement::FunctionDef { name, args: _, body, span: _ } => {
                self.current_scope.push(name.clone());
                
                let mut func_code = format!("{}:\n", name);
                func_code.push_str("    push rbp\n");
                func_code.push_str("    mov rbp, rsp\n");
                
                // Compile function body
                for body_stmt in body {
                    func_code.push_str(&self.compile_statement(body_stmt)?);
                }
                
                func_code.push_str("    mov rsp, rbp\n");
                func_code.push_str("    pop rbp\n");
                func_code.push_str("    ret\n");
                
                self.current_scope.pop();
                Ok(func_code)
            }
            Statement::If { 
                condition, 
                then_block, 
                elif_blocks: _, 
                else_block, 
                span: _ 
            } => {
                let condition_code = self.compile_expression(condition)?;
                let mut if_code = String::new();
                if_code.push_str(&condition_code);
                if_code.push_str("    test rax, rax\n");
                if_code.push_str("    jz .else\n");
                
                // Compile then block
                for stmt in then_block {
                    if_code.push_str(&self.compile_statement(stmt)?);
                }
                
                if_code.push_str("    jmp .endif\n");
                if_code.push_str(".else:\n");
                
                // Compile else block if exists
                if let Some(else_block_stmts) = else_block {
                    for stmt in else_block_stmts {
                        if_code.push_str(&self.compile_statement(stmt)?);
                    }
                }
                
                if_code.push_str(".endif:\n");
                Ok(if_code)
            }
            Statement::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    let value_code = self.compile_expression(expr)?;
                    Ok(format!("{}    ret\n", value_code))
                } else {
                    Ok("    ret\n".to_string())
                }
            }
            _ => Ok(format!("    ; Unsupported statement: {:?}\n", stmt)),
        }
    }
    
    fn compile_expression(&mut self, expr: &Expr) -> Result<String, String> {
        match expr {
            Expr::Number(n, _) => Ok(format!("    mov rax, {}\n", n)),
            Expr::String(s, _) => {
                let label = format!("str_{}", s.len());
                self.string_literals.insert(label.clone(), s.clone());
                Ok(format!("    lea rax, [{}]\n", label))
            }
            Expr::Var(name, _) => {
                if self.symbol_table.contains_key(name) {
                    Ok(format!("    ; Variable reference: {}\n", name))
                } else {
                    Err(format!("Undefined variable: {}", name))
                }
            }
            Expr::Call { func, args, kwargs: _, span: _ } => {
                let mut call_code = String::new();
                
                // Compile arguments
                for arg in args {
                    call_code.push_str(&self.compile_expression(arg)?);
                    call_code.push_str("    push rax\n");
                }
                
                // Call function
                call_code.push_str(&format!("    call {}\n", func));
                
                // Clean up arguments from stack
                if !args.is_empty() {
                    call_code.push_str(&format!("    add rsp, {}\n", args.len() * 8));
                }
                
                Ok(call_code)
            }
            Expr::BinOp { left, op, right, span: _ } => {
                let left_code = self.compile_expression(left)?;
                let right_code = self.compile_expression(right)?;
                
                let mut binop_code = String::new();
                binop_code.push_str(&left_code);
                binop_code.push_str("    push rax\n");
                binop_code.push_str(&right_code);
                binop_code.push_str("    mov rbx, rax\n");
                binop_code.push_str("    pop rax\n");
                
                match op {
                    Op::Add => binop_code.push_str("    add rax, rbx\n"),
                    Op::Sub => binop_code.push_str("    sub rax, rbx\n"),
                    Op::Mul => binop_code.push_str("    imul rax, rbx\n"),
                    Op::Div => {
                        binop_code.push_str("    xor rdx, rdx\n");
                        binop_code.push_str("    idiv rbx\n");
                    }
                    _ => {
                        // Try to handle comparisons generically
                        if let Some(cmp_code) = self.handle_comparison(op) {
                            binop_code.push_str(&cmp_code);
                        } else {
                            return Err(format!("Unsupported operator: {:?}", op));
                        }
                    }
                }
                
                Ok(binop_code)
            }
            Expr::UnaryOp { op, operand, span: _ } => {
                let inner_code = self.compile_expression(operand)?;
                let mut unary_code = String::new();
                unary_code.push_str(&inner_code);
                
                // Handle unary operators based on actual variant names
                match op {
                    crate::parser::UnaryOp::Minus => {
                        unary_code.push_str("    neg rax\n");
                    }
                    crate::parser::UnaryOp::Not => {
                        unary_code.push_str("    not rax\n");
                    }
                    crate::parser::UnaryOp::Plus => {
                        // Unary plus - do nothing
                    }
                    crate::parser::UnaryOp::Invert => {
                        unary_code.push_str("    not rax\n");
                    }
                }
                
                Ok(unary_code)
            }
            Expr::Boolean(b, _) => {
                let value = if *b { 1 } else { 0 };
                Ok(format!("    mov rax, {}\n", value))
            }
            _ => Err(format!("Unsupported expression: {:?}", expr)),
        }
    }
    
    fn handle_comparison(&self, op: &Op) -> Option<String> {
        let mut code = String::new();
        
        // Try to match the operator and generate appropriate assembly
        // This is a fallback for when we don't know the exact variant names
        match format!("{:?}", op).as_str() {
            "Eq" | "Equal" => {
                code.push_str("    cmp rax, rbx\n");
                code.push_str("    sete al\n");
                code.push_str("    movzx rax, al\n");
                Some(code)
            }
            "Ne" | "NotEqual" => {
                code.push_str("    cmp rax, rbx\n");
                code.push_str("    setne al\n");
                code.push_str("    movzx rax, al\n");
                Some(code)
            }
            "Lt" | "LessThan" => {
                code.push_str("    cmp rax, rbx\n");
                code.push_str("    setl al\n");
                code.push_str("    movzx rax, al\n");
                Some(code)
            }
            "Gt" | "GreaterThan" => {
                code.push_str("    cmp rax, rbx\n");
                code.push_str("    setg al\n");
                code.push_str("    movzx rax, al\n");
                Some(code)
            }
            "Le" | "LessThanOrEqual" => {
                code.push_str("    cmp rax, rbx\n");
                code.push_str("    setle al\n");
                code.push_str("    movzx rax, al\n");
                Some(code)
            }
            "Ge" | "GreaterThanOrEqual" => {
                code.push_str("    cmp rax, rbx\n");
                code.push_str("    setge al\n");
                code.push_str("    movzx rax, al\n");
                Some(code)
            }
            "And" | "LogicalAnd" => {
                code.push_str("    and rax, rbx\n");
                Some(code)
            }
            "Or" | "LogicalOr" => {
                code.push_str("    or rax, rbx\n");
                Some(code)
            }
            _ => None,
        }
    }
}