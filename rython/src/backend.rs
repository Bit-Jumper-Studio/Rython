// ==================== RYTHON BACKEND SYSTEM ====================
// Converts Rython AST to target-specific assembly

use crate::parser::{Program, Statement, Expr};

// Capabilities for backend selection
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Capability {
    // Architecture
    RealMode16,
    ProtectedMode32,
    LongMode64,
    
    // Extensions
    SSE,
    SSE2,
    SSE3,
    SSE4,
    AVX,
    AVX2,
    AVX512,
    
    // Features
    Paging,
    VirtualMemory,
    MultiCore,
    Graphics,
    
    // Environment
    BIOS,
    UEFI,
    PureMetal,
    Linux,
    Windows,
    
    // Constraints
    NoFloat,
    NoHeap,
    NoFilesystem,
    ReadOnly,
}

// Simple module representation without IR
#[derive(Debug, Clone)]
pub struct BackendModule {
    pub functions: Vec<BackendFunction>,
    pub globals: Vec<BackendGlobal>,
    pub required_capabilities: Vec<Capability>,
}

#[derive(Debug, Clone)]
pub struct BackendFunction {
    pub name: String,
    pub parameters: Vec<(String, String)>, // (name, type)
    pub body: Vec<String>, // Assembly instructions
}

#[derive(Debug, Clone)]
pub struct BackendGlobal {
    pub name: String,
    pub value: String,
    pub type_name: String,
}

pub trait Backend {
    /// Backend name for debugging
    fn name(&self) -> &str;
    
    /// Supported capabilities
    fn supported_capabilities(&self) -> Vec<Capability>;
    
    /// Can this backend handle the module's requirements?
    fn can_compile(&self, module: &BackendModule) -> bool {
        module.required_capabilities.iter()
            .all(|cap| self.supported_capabilities().contains(cap))
    }
    
    /// Generate assembly from program AST
    fn compile_program(&mut self, program: &Program) -> Result<String, String>;
    
    /// Generate function prologue
    fn function_prologue(&self, func: &BackendFunction) -> String;
    
    /// Generate function epilogue
    fn function_epilogue(&self, func: &BackendFunction) -> String;
    
    /// Generate instruction from expression
    fn compile_expression(&self, expr: &Expr) -> Result<String, String>;
}

// ========== BIOS 64-BIT BACKEND ==========

pub struct Bios64Backend {
    use_sse: bool,
    use_avx: bool,
}

impl Bios64Backend {
    pub fn new() -> Self {
        Self {
            use_sse: false,
            use_avx: false,
        }
    }
    
    pub fn with_sse(mut self) -> Self {
        self.use_sse = true;
        self
    }
    
    pub fn with_avx(mut self) -> Self {
        self.use_avx = true;
        self
    }
}

impl Backend for Bios64Backend {
    fn name(&self) -> &str {
        "bios64"
    }
    
    fn supported_capabilities(&self) -> Vec<Capability> {
        let mut caps = vec![
            Capability::BIOS,
            Capability::LongMode64,
            Capability::PureMetal,
        ];
        
        if self.use_sse {
            caps.push(Capability::SSE);
            caps.push(Capability::SSE2);
        }
        
        if self.use_avx {
            caps.push(Capability::AVX);
        }
        
        caps
    }
    
    fn compile_program(&mut self, program: &Program) -> Result<String, String> {
        let mut asm = String::new();
        
        // BIOS bootloader header
        asm.push_str("; Rython BIOS 64-bit Bootloader\n");
        asm.push_str("; Generated from Rython AST\n\n");
        asm.push_str("    org 0x7C00\n");
        asm.push_str("    bits 16\n\n");
        asm.push_str("start:\n");
        asm.push_str("    cli\n");
        asm.push_str("    xor ax, ax\n");
        asm.push_str("    mov ds, ax\n");
        asm.push_str("    mov es, ax\n");
        asm.push_str("    mov ss, ax\n");
        asm.push_str("    mov sp, 0x7C00\n");
        asm.push_str("    sti\n");
        asm.push_str("    cld\n\n");
        
        // Mode transition to 64-bit (simplified)
        asm.push_str("    ; Transition to 64-bit mode\n");
        asm.push_str("    lgdt [gdt64_desc]\n");
        asm.push_str("    mov eax, cr0\n");
        asm.push_str("    or eax, 1\n");
        asm.push_str("    mov cr0, eax\n");
        asm.push_str("    jmp 0x08:long_mode\n\n");
        
        asm.push_str("    bits 64\n");
        asm.push_str("long_mode:\n");
        asm.push_str("    mov ax, 0x10\n");
        asm.push_str("    mov ds, ax\n");
        asm.push_str("    mov es, ax\n");
        asm.push_str("    mov ss, ax\n");
        asm.push_str("    mov rsp, 0x90000\n\n");
        
        // Compile each statement
        for stmt in &program.body {
            match stmt {
                Statement::VarDecl { name, value, type_hint: _ } => {
                    asm.push_str(&format!("; Variable declaration: {}\n", name));
                    asm.push_str(&self.compile_expression(value)?);
                }
                Statement::Expr(expr) => {
                    asm.push_str("; Expression\n");
                    asm.push_str(&self.compile_expression(expr)?);
                }
                Statement::FunctionDef { name, args, body } => {
                    asm.push_str(&format!("; Function: {}\n", name));
                    asm.push_str(&format!("{}:\n", name));
                    asm.push_str(&self.function_prologue(&BackendFunction {
                        name: name.clone(),
                        parameters: args.iter().map(|arg| (arg.clone(), "int".to_string())).collect(),
                        body: Vec::new(),
                    }));
                    
                    // Compile function body
                    for body_stmt in body {
                        if let Statement::Expr(expr) = body_stmt {
                            asm.push_str(&self.compile_expression(expr)?);
                        }
                    }
                    
                    asm.push_str(&self.function_epilogue(&BackendFunction {
                        name: name.clone(),
                        parameters: args.iter().map(|arg| (arg.clone(), "int".to_string())).collect(),
                        body: Vec::new(),
                    }));
                }
                _ => {
                    asm.push_str("; [Other statement]\n");
                }
            }
        }
        
        // Bootloader footer
        asm.push_str("    ; Halt\n");
        asm.push_str("    cli\n");
        asm.push_str("    hlt\n");
        asm.push_str("    jmp $\n\n");
        
        // GDT
        asm.push_str("gdt64:\n");
        asm.push_str("    dq 0x0000000000000000\n");
        asm.push_str("    dq 0x00209A0000000000\n");
        asm.push_str("    dq 0x0000920000000000\n");
        asm.push_str("gdt64_end:\n\n");
        asm.push_str("gdt64_desc:\n");
        asm.push_str("    dw gdt64_end - gdt64 - 1\n");
        asm.push_str("    dq gdt64\n\n");
        
        // Boot signature
        asm.push_str("    times 510-($-$$) db 0\n");
        asm.push_str("    dw 0xAA55\n");
        
        Ok(asm)
    }
    
    fn function_prologue(&self, func: &BackendFunction) -> String {
        let mut prologue = String::new();
        prologue.push_str(&format!("{}:\n", func.name));
        prologue.push_str("    push rbp\n");
        prologue.push_str("    mov rbp, rsp\n");
        
        // Allocate stack space for locals
        let stack_size = func.parameters.len() * 8;
        if stack_size > 0 {
            prologue.push_str(&format!("    sub rsp, {}\n", stack_size));
        }
        
        prologue
    }
    
    fn function_epilogue(&self, _func: &BackendFunction) -> String {
        let mut epilogue = String::new();
        epilogue.push_str("    mov rsp, rbp\n");
        epilogue.push_str("    pop rbp\n");
        epilogue.push_str("    ret\n");
        epilogue
    }
    
    fn compile_expression(&self, expr: &Expr) -> Result<String, String> {
        let mut code = String::new();
        
        match expr {
            Expr::Number(n) => {
                code.push_str(&format!("    ; Number: {}\n", n));
                code.push_str(&format!("    mov rax, {}\n", n));
                code.push_str("    call print_decimal_64\n");
            }
            Expr::Call { func, args, .. } => {
                if func == "print" {
                    for arg in args {
                        let arg_code = self.compile_expression(arg)?;
                        code.push_str(&arg_code);
                    }
                    code.push_str("    ; print intrinsic\n");
                    code.push_str("    mov rsi, msg_print\n");
                    code.push_str("    call print_string_64\n");
                } else {
                    code.push_str(&format!("    call {}\n", func));
                }
            }
            Expr::BinOp { left, op, right } => {
                let left_code = self.compile_expression(left)?;
                let right_code = self.compile_expression(right)?;
                code.push_str(&left_code);
                code.push_str(&right_code);
                
                code.push_str("    ; Binary operation\n");
                code.push_str("    pop rbx\n");
                code.push_str("    pop rax\n");
                
                match op {
                    crate::parser::Op::Add => {
                        code.push_str("    add rax, rbx\n");
                    }
                    crate::parser::Op::Sub => {
                        code.push_str("    sub rax, rbx\n");
                    }
                    crate::parser::Op::Mul => {
                        code.push_str("    imul rax, rbx\n");
                    }
                    crate::parser::Op::Div => {
                        code.push_str("    xor rdx, rdx\n");
                        code.push_str("    idiv rbx\n");
                    }
                    _ => {
                        code.push_str("    add rax, rbx\n");
                    }
                }
                
                code.push_str("    push rax\n");
            }
            _ => {
                code.push_str("    ; [Expression]\n");
            }
        }
        
        Ok(code)
    }
}

// ========== LINUX 64-BIT BACKEND ==========

pub struct Linux64Backend;

impl Linux64Backend {
    pub fn new() -> Self {
        Self
    }
}

impl Backend for Linux64Backend {
    fn name(&self) -> &str {
        "linux64"
    }
    
    fn supported_capabilities(&self) -> Vec<Capability> {
        vec![
            Capability::Linux,
            Capability::LongMode64,
        ]
    }
    
    fn compile_program(&mut self, program: &Program) -> Result<String, String> {
        let mut asm = String::new();
        
        asm.push_str("; Rython Linux 64-bit ELF\n");
        asm.push_str("; Generated from Rython AST\n\n");
        
        asm.push_str("    bits 64\n");
        asm.push_str("    default rel\n\n");
        
        asm.push_str("    section .text\n");
        asm.push_str("    global _start\n\n");
        
        asm.push_str("_start:\n");
        
        // Compile statements
        for stmt in &program.body {
            if let Statement::Expr(expr) = stmt {
                asm.push_str(&self.compile_expression(expr)?);
            }
        }
        
        // System exit
        asm.push_str("    ; Exit\n");
        asm.push_str("    mov rax, 60    ; sys_exit\n");
        asm.push_str("    xor rdi, rdi   ; exit code 0\n");
        asm.push_str("    syscall\n");
        
        Ok(asm)
    }
    
    fn function_prologue(&self, func: &BackendFunction) -> String {
        format!("{}:\n", func.name)
    }
    
    fn function_epilogue(&self, _func: &BackendFunction) -> String {
        String::new()
    }
    
    fn compile_expression(&self, expr: &Expr) -> Result<String, String> {
        Ok(format!("    ; {:?}\n", expr))
    }
}

// ========== BACKEND REGISTRY ==========

pub struct BackendRegistry {
    pub backends: Vec<Box<dyn Backend>>,
}

impl BackendRegistry {
    pub fn new() -> Self {
        Self {
            backends: Vec::new(),
        }
    }
    
    pub fn register(&mut self, backend: Box<dyn Backend>) {
        self.backends.push(backend);
    }
    
    pub fn find_backend(&self, module: &BackendModule) -> Option<&dyn Backend> {
        self.backends.iter()
            .find(|b| b.can_compile(module))
            .map(|b| b.as_ref())
    }
    
    pub fn default_registry() -> Self {
        let mut registry = Self::new();
        
        // Register all available backends
        registry.register(Box::new(Bios64Backend::new()));
        registry.register(Box::new(Bios64Backend::new().with_sse()));
        registry.register(Box::new(Bios64Backend::new().with_avx()));
        registry.register(Box::new(Linux64Backend::new()));
        
        registry
    }
}