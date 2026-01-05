use crate::parser::{Program, Statement, Expr, Position, Span};
use std::collections::HashMap;
use std::cell::RefCell;

// ========== CORE TYPE DEFINITIONS ==========

// Target platforms
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Target {
    Bios16,
    Bios32,
    Bios64,
    Bios64Sse,
    Bios64Avx,
    Bios64Avx512,
    Linux64,
    Windows64,
}

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
    _SSE3,
    _SSE4,
    AVX,
    _AVX2,
    AVX512,
    
    // Features
    Paging,
    VirtualMemory,
    _MultiCore,
    Graphics,
    
    // Environment
    BIOS,
    _UEFI,
    PureMetal,
    Linux,
    Windows,
    
    // Constraints
    _NoFloat,
    NoHeap,
    NoFilesystem,
    _ReadOnly,
}

// Simple module representation without IR
#[derive(Debug, Clone)]
pub struct BackendModule {
    #[allow(dead_code)]
    pub functions: Vec<BackendFunction>,
    #[allow(dead_code)]
    pub globals: Vec<BackendGlobal>,
    pub required_capabilities: Vec<Capability>,
    #[allow(dead_code)]
    pub external_asm: Vec<String>, // SSD: External assembly to inject
    #[allow(dead_code)]
    pub syntax_extensions: Vec<SyntaxExtension>, // SSD: Syntax extensions from RCL
}

#[derive(Debug, Clone)]
pub struct BackendFunction {
    pub name: String,
    pub parameters: Vec<(String, String)>, // (name, type)
    pub body: Vec<String>, // Assembly instructions
}

#[derive(Debug, Clone)]
pub struct BackendGlobal {
    #[allow(dead_code)]
    pub name: String,
    #[allow(dead_code)]
    pub value: String,
    #[allow(dead_code)]
    pub type_name: String,
}

// SSD: Syntax extension from RCL header
#[derive(Debug, Clone)]
pub struct SyntaxExtension {
    #[allow(dead_code)]
    pub pattern: String,       // e.g., "> expr" 
    #[allow(dead_code)]
    pub replacement: String,   // e.g., "print(expr)"
    pub assembly_label: Option<String>, // Associated assembly function
    #[allow(dead_code)]
    pub register_args: Vec<String>, // Which registers to use for arguments
}

pub trait Backend {
    /// Backend name for debugging
    fn name(&self) -> &str;
    
    /// Generate assembly header/setup
    #[allow(dead_code)]
    fn generate_header(&self) -> String;
    
    /// Supported capabilities
    fn supported_capabilities(&self) -> Vec<Capability>;
    
    /// Required capabilities for this backend
    #[allow(dead_code)]
    fn required_capabilities(&self) -> Vec<Capability> {
        Vec::new()
    }
    
    /// Format for NASM
    #[allow(dead_code)]
    fn format(&self) -> &'static str;
    
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
    
    /// SSD: Inject external assembly into generated code
    fn inject_external_asm(&self, base_asm: &str, external_asm: &[String]) -> String {
        let mut final_asm = base_asm.to_string();
        
        for asm_block in external_asm {
            final_asm.push_str("\n; === SSD Injected Assembly ===\n");
            final_asm.push_str(asm_block);
            final_asm.push_str("\n; === End SSD Injected Assembly ===\n");
        }
        
        final_asm
    }
    
    /// SSD: Apply syntax extensions to program
    #[allow(dead_code)]
    fn apply_syntax_extensions(&self, _program: &mut Program, _extensions: &[SyntaxExtension]) -> Result<(), String> {
        for ext in _extensions {
            // This would be implemented in parser.rs, but we declare the interface here
            println!("Applying syntax extension: {} -> {}", ext.pattern, ext.replacement);
        }
        Ok(())
    }
    
    /// SSD: Prepare register arguments for FFI
    #[allow(dead_code)]
    fn prepare_register_args(&self, args: &[Expr], registers: &[String]) -> String {
        let mut asm = String::new();
        
        for (i, (arg, reg)) in args.iter().zip(registers.iter()).enumerate() {
            match arg {
                Expr::Number(n, _) => {
                    asm.push_str(&format!("    mov {}, {}\n", reg, n));
                }
                Expr::Var(name, _) => {
                    asm.push_str(&format!("    ; Loading variable {} into {}\n", name, reg));
                    // Variable loading would be implemented per-backend
                }
                _ => {}
            }
            
            // Stop if we run out of registers
            if i >= 3 { break; } // RCX, RDX, R8, R9 on Windows64
        }
        
        asm
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
            .find(|b| {
                b.can_compile(module) && self.capabilities_match(b.as_ref(), &module.required_capabilities)
            })
            .map(|b| b.as_ref())
    }

    fn capabilities_match(&self, backend: &dyn Backend, module_caps: &[Capability]) -> bool {
        let backend_caps = backend.supported_capabilities();
        module_caps.iter().all(|cap| backend_caps.contains(cap))
    }
    
    pub fn default_registry() -> Self {
        let mut registry = Self::new();
        
        // Register all available backends
        registry.register(Box::new(Bios16Backend::new()));
        registry.register(Box::new(Bios32Backend::new()));
        registry.register(Box::new(Bios64Backend::new()));
        registry.register(Box::new(Bios64Backend::new().with_sse()));
        registry.register(Box::new(Bios64Backend::new().with_avx()));
        registry.register(Box::new(Bios64Backend::new().with_avx512()));
        registry.register(Box::new(Linux64Backend::new()));
        registry.register(Box::new(Windows64Backend::new()));
        
        registry
    }
}

// ========== BIOS 16-BIT BACKEND ==========

pub struct Bios16Backend {
    string_literals: HashMap<String, String>,
    external_asm: Vec<String>, // SSD: Store external assembly
}

impl Bios16Backend {
    pub fn new() -> Self {
        Self {
            string_literals: HashMap::new(),
            external_asm: Vec::new(),
        }
    }
    
    #[allow(dead_code)]
    pub fn add_external_asm(&mut self, asm: &str) {
        self.external_asm.push(asm.to_string());
    }
    
    fn generate_string_data(&self) -> String {
        let mut data = String::new();
        for (content, label) in &self.string_literals {
            data.push_str(&format!("{}:\n", label));
            data.push_str(&format!("    db '{}', 0\n", content.replace("'", "''")));
        }
        data
    }
}

impl Backend for Bios16Backend {
    fn name(&self) -> &str {
        "bios16"
    }
    
    fn generate_header(&self) -> String {
        String::from("; BIOS 16-bit Backend\n    org 0x7C00\n    bits 16\n\n")
    }
    
    fn format(&self) -> &'static str {
        "bin"
    }
    
    fn supported_capabilities(&self) -> Vec<Capability> {
        vec![
            Capability::BIOS,
            Capability::RealMode16,
            Capability::PureMetal,
            Capability::NoHeap,
            Capability::NoFilesystem,
        ]
    }
    
    fn compile_program(&mut self, program: &Program) -> Result<String, String> {
        let mut asm = String::new();
        
        asm.push_str("; Rython BIOS 16-bit Backend\n");
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
        
        asm.push_str("    mov ax, 0x0003\n");
        asm.push_str("    int 0x10\n\n");
        
        // Compile each statement
        for stmt in &program.body {
            match stmt {
                Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                    asm.push_str(&format!("; Variable: {}\n", name));
                    asm.push_str(&self.compile_expression(value)?);
                }
                Statement::Expr(expr) => {
                    asm.push_str("; Expression\n");
                    asm.push_str(&self.compile_expression(expr)?);
                }
                _ => {
                    asm.push_str("; [Statement]\n");
                }
            }
        }
        
        // Boot signature
        asm.push_str("\n    cli\n");
        asm.push_str("    hlt\n");
        asm.push_str("    jmp $\n\n");
        
        // 16-bit subroutines
        asm.push_str("print_string:\n");
        asm.push_str("    pusha\n");
        asm.push_str(".loop:\n");
        asm.push_str("    lodsb\n");
        asm.push_str("    test al, al\n");
        asm.push_str("    jz .done\n");
        asm.push_str("    int 0x10\n");
        asm.push_str("    jmp .loop\n");
        asm.push_str(".done:\n");
        asm.push_str("    popa\n");
        asm.push_str("    ret\n\n");
        
        asm.push_str("print_decimal:\n");
        asm.push_str("    ; Print decimal number in AX\n");
        asm.push_str("    pusha\n");
        asm.push_str("    mov cx, 0\n");
        asm.push_str("    mov bx, 10\n");
        asm.push_str(".div_loop:\n");
        asm.push_str("    xor dx, dx\n");
        asm.push_str("    div bx\n");
        asm.push_str("    push dx\n");
        asm.push_str("    inc cx\n");
        asm.push_str("    test ax, ax\n");
        asm.push_str("    jnz .div_loop\n");
        asm.push_str(".print_loop:\n");
        asm.push_str("    pop ax\n");
        asm.push_str("    add al, '0'\n");
        asm.push_str("    mov ah, 0x0E\n");
        asm.push_str("    int 0x10\n");
        asm.push_str("    loop .print_loop\n");
        asm.push_str("    popa\n");
        asm.push_str("    ret\n\n");
        
        // String data
        asm.push_str("; String literals\n");
        asm.push_str(&self.generate_string_data());
        
        // SSD: Inject external assembly
        asm = self.inject_external_asm(&asm, &self.external_asm);
        
        asm.push_str("\n    times 510-($-$$) db 0\n");
        asm.push_str("    dw 0xAA55\n");
        
        Ok(asm)
    }
    
    fn function_prologue(&self, func: &BackendFunction) -> String {
        format!("{}:\n    push bp\n    mov bp, sp\n", func.name)
    }
    
    fn function_epilogue(&self, _func: &BackendFunction) -> String {
        "    mov sp, bp\n    pop bp\n    ret\n".to_string()
    }
    
    fn compile_expression(&self, expr: &Expr) -> Result<String, String> {
        match expr {
            Expr::Number(n, _) => Ok(format!("    ; Number: {}\n    mov ax, {}\n    call print_decimal\n", n, n)),
            Expr::String(s, _) => Ok(format!("    ; String: '{}'\n    mov si, str_const\n    call print_string\n", s)),
            Expr::Call { func, args, kwargs: _, span: _ } => {
                if func == "print" {
                    let mut code = String::new();
                    for arg in args {
                        let arg_code = self.compile_expression(arg)?;
                        code.push_str(&arg_code);
                    }
                    Ok(code)
                } else {
                    Ok(format!("    call {}\n", func))
                }
            }
            _ => Ok("    ; [Expression]\n".to_string()),
        }
    }
}

// ========== BIOS 32-BIT BACKEND ==========

pub struct Bios32Backend {
    string_literals: HashMap<String, String>,
    external_asm: Vec<String>, // SSD: Store external assembly
}

impl Bios32Backend {
    pub fn new() -> Self {
        Self {
            string_literals: HashMap::new(),
            external_asm: Vec::new(),
        }
    }
    
    #[allow(dead_code)]
    pub fn add_external_asm(&mut self, asm: &str) {
        self.external_asm.push(asm.to_string());
    }
    
    fn generate_string_data(&self) -> String {
        let mut data = String::new();
        for (content, label) in &self.string_literals {
            data.push_str(&format!("{}:\n", label));
            data.push_str(&format!("    db '{}', 0\n", content.replace("'", "''")));
        }
        data
    }
}

impl Backend for Bios32Backend {
    fn name(&self) -> &str {
        "bios32"
    }
    
    fn generate_header(&self) -> String {
        String::from("; BIOS 32-bit Backend\n    org 0x7C00\n    bits 16\n\n")
    }
    
    fn format(&self) -> &'static str {
        "bin"
    }
    
    fn supported_capabilities(&self) -> Vec<Capability> {
        vec![
            Capability::BIOS,
            Capability::ProtectedMode32,
            Capability::PureMetal,
            Capability::Paging,
        ]
    }
    
    fn compile_program(&mut self, program: &Program) -> Result<String, String> {
        let mut asm = String::new();
        
        asm.push_str("; Rython BIOS 32-bit Backend\n");
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
        
        asm.push_str("    mov ax, 0x0003\n");
        asm.push_str("    int 0x10\n\n");
        
        // Setup protected mode
        asm.push_str("    in al, 0x92\n");
        asm.push_str("    or al, 2\n");
        asm.push_str("    out 0x92, al\n\n");
        
        asm.push_str("    lgdt [gdt32_desc]\n\n");
        
        asm.push_str("    mov eax, cr0\n");
        asm.push_str("    or eax, 1\n");
        asm.push_str("    mov cr0, eax\n\n");
        
        asm.push_str("    jmp 0x08:protected_mode\n\n");
        
        asm.push_str("    bits 32\n");
        asm.push_str("protected_mode:\n");
        asm.push_str("    mov ax, 0x10\n");
        asm.push_str("    mov ds, ax\n");
        asm.push_str("    mov es, ax\n");
        asm.push_str("    mov fs, ax\n");
        asm.push_str("    mov gs, ax\n");
        asm.push_str("    mov ss, ax\n");
        asm.push_str("    mov esp, 0x7C00\n\n");
        
        // Compile each statement
        for stmt in &program.body {
            match stmt {
                Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                    asm.push_str(&format!("; Variable: {}\n", name));
                    asm.push_str(&self.compile_expression(value)?);
                }
                Statement::Expr(expr) => {
                    asm.push_str("; Expression\n");
                    asm.push_str(&self.compile_expression(expr)?);
                }
                _ => {
                    asm.push_str("; [Statement]\n");
                }
            }
        }
        
        asm.push_str("\n    cli\n");
        asm.push_str("    hlt\n");
        asm.push_str("    jmp $\n\n");
        
        // 32-bit subroutines
        asm.push_str("print_string_32:\n");
        asm.push_str("    pusha\n");
        asm.push_str(".loop:\n");
        asm.push_str("    lodsb\n");
        asm.push_str("    test al, al\n");
        asm.push_str("    jz .done\n");
        asm.push_str("    mov [edi], al\n");
        asm.push_str("    inc edi\n");
        asm.push_str("    mov byte [edi], 0x0F\n");
        asm.push_str("    inc edi\n");
        asm.push_str("    jmp .loop\n");
        asm.push_str(".done:\n");
        asm.push_str("    popa\n");
        asm.push_str("    ret\n\n");
        
        asm.push_str("print_decimal_32:\n");
        asm.push_str("    ; Print decimal number in EAX\n");
        asm.push_str("    pusha\n");
        asm.push_str("    mov ecx, 0\n");
        asm.push_str("    mov ebx, 10\n");
        asm.push_str("    mov edi, 0xB8000 + 160  ; Second line\n");
        asm.push_str(".div_loop:\n");
        asm.push_str("    xor edx, edx\n");
        asm.push_str("    div ebx\n");
        asm.push_str("    push dx\n");
        asm.push_str("    inc ecx\n");
        asm.push_str("    test eax, eax\n");
        asm.push_str("    jnz .div_loop\n");
        asm.push_str(".print_loop:\n");
        asm.push_str("    pop ax\n");
        asm.push_str("    add al, '0'\n");
        asm.push_str("    mov [edi], al\n");
        asm.push_str("    inc edi\n");
        asm.push_str("    mov byte [edi], 0x0F\n");
        asm.push_str("    inc edi\n");
        asm.push_str("    loop .print_loop\n");
        asm.push_str("    popa\n");
        asm.push_str("    ret\n\n");
        
        // String data
        asm.push_str("; String literals\n");
        asm.push_str(&self.generate_string_data());
        
        // SSD: Inject external assembly
        asm = self.inject_external_asm(&asm, &self.external_asm);
        
        // GDT
        asm.push_str("gdt32:\n");
        asm.push_str("    dq 0x0000000000000000\n");
        asm.push_str("    dq 0x00CF9A000000FFFF\n");
        asm.push_str("    dq 0x00CF92000000FFFF\n");
        asm.push_str("gdt32_end:\n\n");
        
        asm.push_str("gdt32_desc:\n");
        asm.push_str("    dw gdt32_end - gdt32 - 1\n");
        asm.push_str("    dd gdt32\n\n");
        
        asm.push_str("    times 510-($-$$) db 0\n");
        asm.push_str("    dw 0xAA55\n");
        
        Ok(asm)
    }
    
    fn function_prologue(&self, func: &BackendFunction) -> String {
        format!("{}:\n    push ebp\n    mov ebp, esp\n", func.name)
    }
    
    fn function_epilogue(&self, _func: &BackendFunction) -> String {
        "    mov esp, ebp\n    pop ebp\n    ret\n".to_string()
    }
    
    fn compile_expression(&self, expr: &Expr) -> Result<String, String> {
        match expr {
            Expr::Number(n, _) => Ok(format!("    mov eax, {}\n    call print_decimal_32\n", n)),
            Expr::String(s, _) => {
                let label = format!("str_{}", s.hash_code());
                Ok(format!("    ; String: '{}'\n    mov esi, {}\n    mov edi, 0xB8000\n    call print_string_32\n", s, label))
            }
            Expr::Call { func, args, kwargs: _, span: _ } => {
                if func == "print" {
                    let mut code = String::new();
                    for arg in args {
                        code.push_str(&self.compile_expression(arg)?);
                    }
                    Ok(code)
                } else {
                    Ok(format!("    call {}\n", func))
                }
            }
            _ => Ok("    ; [Expression]\n".to_string()),
        }
    }
}

// ========== BIOS 64-BIT BACKEND ==========

pub struct Bios64Backend {
    use_sse: bool,
    use_avx: bool,
    use_avx512: bool,
    string_counter: RefCell<u32>,
    string_literals: RefCell<HashMap<String, String>>,
    #[allow(dead_code)]
    code_size_limit: usize,
    external_asm: Vec<String>, // SSD: Store external assembly
    syntax_extensions: Vec<SyntaxExtension>, // SSD: Syntax extensions
}

impl Bios64Backend {
    pub fn new() -> Self {
        Self {
            use_sse: false,
            use_avx: false,
            use_avx512: false,
            string_counter: RefCell::new(0),
            string_literals: RefCell::new(HashMap::new()),
            code_size_limit: 4096,  // 4KB max for kernel
            external_asm: Vec::new(),
            syntax_extensions: Vec::new(),
        }
    }
    
    // Add this method to check size
    #[allow(dead_code)]
    fn check_code_size(&self, code: &str, strings: &HashMap<String, String>) -> Result<(), String> {
        let code_bytes = code.len();
        let string_bytes: usize = strings.values()
            .map(|s| s.len() + 1)  // +1 for null terminator
            .sum();
        let total = code_bytes + string_bytes;
        
        if total > self.code_size_limit {
            Err(format!(
                "Code size limit exceeded: {} bytes (limit: {}). Too many print statements or string data.",
                total, self.code_size_limit
            ))
        } else {
            Ok(())
        }
    }
    
    // SSD: Add external assembly
    #[allow(dead_code)]
    pub fn add_external_asm(&mut self, asm: &str) {
        self.external_asm.push(asm.to_string());
    }
    
    // SSD: Add syntax extension
    #[allow(dead_code)]
    pub fn add_syntax_extension(&mut self, pattern: &str, replacement: &str, assembly_label: Option<&str>, register_args: Vec<String>) {
        self.syntax_extensions.push(SyntaxExtension {
            pattern: pattern.to_string(),
            replacement: replacement.to_string(),
            assembly_label: assembly_label.map(|s| s.to_string()),
            register_args,
        });
    }
    
    pub fn with_sse(mut self) -> Self {
        self.use_sse = true;
        self
    }
    
    pub fn with_avx(mut self) -> Self {
        self.use_avx = true;
        self
    }
    
    pub fn with_avx512(mut self) -> Self {
        self.use_avx512 = true;
        self
    }
    
    fn get_string_label(&self, content: &str) -> String {
        let mut literals = self.string_literals.borrow_mut();
        if let Some(label) = literals.get(content) {
            return label.clone();
        }
        
        let mut counter = self.string_counter.borrow_mut();
        let label = format!("str_{}", *counter);
        *counter += 1;
        literals.insert(content.to_string(), label.clone());
        label
    }
    
    fn generate_string_data(&self) -> String {
        let literals = self.string_literals.borrow();
        let mut data = String::new();
        for (content, label) in &*literals {
            data.push_str(&format!("{}:\n", label));
            data.push_str(&format!("    db '{}', 0\n", content.replace("'", "''")));
        }
        data
    }
}

impl Backend for Bios64Backend {
    fn name(&self) -> &str {
        "bios64"
    }
    
    fn generate_header(&self) -> String {
        let mut header = String::from("; BIOS 64-bit Backend\n    org 0x7C00\n    bits 16\n\n");
        
        if self.use_sse {
            header.push_str("    ; SSE enabled\n");
        }
        if self.use_avx {
            header.push_str("    ; AVX enabled\n");
        }
        if self.use_avx512 {
            header.push_str("    ; AVX-512 enabled\n");
        }
        
        header
    }
    
    fn format(&self) -> &'static str {
        "bin"
    }
    
    fn supported_capabilities(&self) -> Vec<Capability> {
        let mut caps = vec![
            Capability::BIOS,
            Capability::LongMode64,
            Capability::PureMetal,
            Capability::Paging,
        ];
        
        if self.use_sse {
            caps.push(Capability::SSE);
            caps.push(Capability::SSE2);
        }
        
        if self.use_avx {
            caps.push(Capability::AVX);
        }
        
        if self.use_avx512 {
            caps.push(Capability::AVX512);
        }
        
        caps
    }
    
    fn compile_program(&mut self, program: &Program) -> Result<String, String> {
        let mut asm = String::new();
        
        asm.push_str("; Rython BIOS 64-bit Backend\n");
        asm.push_str("; Generated from Rython AST\n\n");
        asm.push_str("    org 0x7C00\n");
        asm.push_str("    bits 16\n\n");
        
        // Add feature flags
        if self.use_sse {
            asm.push_str("    ; SSE enabled\n");
        }
        if self.use_avx {
            asm.push_str("    ; AVX enabled\n");
        }
        if self.use_avx512 {
            asm.push_str("    ; AVX-512 enabled\n");
        }
        asm.push_str("\n");
        
        asm.push_str("start:\n");
        asm.push_str("    cli\n");
        asm.push_str("    xor ax, ax\n");
        asm.push_str("    mov ds, ax\n");
        asm.push_str("    mov es, ax\n");
        asm.push_str("    mov ss, ax\n");
        asm.push_str("    mov sp, 0x7C00\n");
        asm.push_str("    sti\n");
        asm.push_str("    cld\n\n");
        
        asm.push_str("    mov ax, 0x0003\n");
        asm.push_str("    int 0x10\n\n");
        
        // Simple A20 enable
        asm.push_str("    in al, 0x92\n");
        asm.push_str("    or al, 2\n");
        asm.push_str("    out 0x92, al\n\n");
        
        // Load 32-bit GDT
        asm.push_str("    lgdt [gdt32_desc]\n\n");
        
        // Enter protected mode
        asm.push_str("    mov eax, cr0\n");
        asm.push_str("    or eax, 1\n");
        asm.push_str("    mov cr0, eax\n\n");
        asm.push_str("    jmp 0x08:protected_mode\n\n");
        
        // ========== 32-bit code ==========
        asm.push_str("    bits 32\n");
        asm.push_str("protected_mode:\n");
        asm.push_str("    mov ax, 0x10\n");
        asm.push_str("    mov ds, ax\n");
        asm.push_str("    mov es, ax\n");
        asm.push_str("    mov fs, ax\n");
        asm.push_str("    mov gs, ax\n");
        asm.push_str("    mov ss, ax\n");
        asm.push_str("    mov esp, 0x90000\n\n");
        
        // Setup paging (32-bit code)
        asm.push_str("    ; Setup paging\n");
        asm.push_str("    mov edi, 0x1000\n");
        asm.push_str("    mov cr3, edi\n");
        asm.push_str("    xor eax, eax\n");
        asm.push_str("    mov ecx, 4096\n");
        asm.push_str("    rep stosd\n");
        
        asm.push_str("    mov edi, 0x1000\n");
        asm.push_str("    mov dword [edi], 0x2003\n");
        asm.push_str("    add edi, 0x1000\n");
        asm.push_str("    mov dword [edi], 0x3003\n");
        asm.push_str("    add edi, 0x1000\n");
        
        asm.push_str("    mov ebx, 0x00000083\n");
        asm.push_str("    mov ecx, 512\n");
        asm.push_str(".set_entry:\n");
        asm.push_str("    mov dword [edi], ebx\n");
        asm.push_str("    add ebx, 0x200000\n");
        asm.push_str("    add edi, 8\n");
        asm.push_str("    loop .set_entry\n\n");
        
        // Enable PAE
        asm.push_str("    mov eax, cr4\n");
        asm.push_str("    or eax, (1 << 5)\n");
        asm.push_str("    mov cr4, eax\n\n");
        
        // Set CR3
        asm.push_str("    mov eax, 0x1000\n");
        asm.push_str("    mov cr3, eax\n\n");
        
        // Enable long mode
        asm.push_str("    mov ecx, 0xC0000080\n");
        asm.push_str("    rdmsr\n");
        asm.push_str("    or eax, (1 << 8)\n");
        asm.push_str("    wrmsr\n\n");
        
        // Enable paging
        asm.push_str("    mov eax, cr0\n");
        asm.push_str("    or eax, (1 << 31)\n");
        asm.push_str("    mov cr0, eax\n\n");
        
        // Load 64-bit GDT
        asm.push_str("    lgdt [gdt64_desc]\n\n");
        
        // Jump to 64-bit mode
        asm.push_str("    jmp 0x08:long_mode\n\n");
        
        // ========== 64-bit code ==========
        asm.push_str("    bits 64\n");
        asm.push_str("long_mode:\n");
        
        asm.push_str("    mov ax, 0x10\n");
        asm.push_str("    mov ds, ax\n");
        asm.push_str("    mov es, ax\n");
        asm.push_str("    mov fs, ax\n");
        asm.push_str("    mov gs, ax\n");
        asm.push_str("    mov ss, ax\n");
        asm.push_str("    mov rsp, 0x90000\n\n");
        
        // Clear screen (64-bit)
        asm.push_str("    mov rdi, 0xB8000\n");
        asm.push_str("    mov rax, 0x0720072007200720\n");
        asm.push_str("    mov rcx, 1000\n");
        asm.push_str("    rep stosq\n\n");
        
        // Print message (64-bit)
        asm.push_str("    mov rsi, msg_64\n");
        asm.push_str("    mov rdi, 0xB8000\n");
        asm.push_str("    call print_string_64\n\n");
        
        // Compile program statements
        for stmt in &program.body {
            match stmt {
                Statement::Expr(expr) => {
                    asm.push_str("; Expression\n");
                    asm.push_str(&self.compile_expression(expr)?);
                }
                Statement::FunctionDef { name, args, body, span: _ } => {
                    asm.push_str(&format!("; Function: {}\n", name));
                    asm.push_str(&format!("{}:\n", name));
                    
                    // Create a Program for the function body with a default span
                    let _func_program = Program {
                        body: body.to_vec(),
                        span: Span::single(Position::start()),
                    };
                    
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
                Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                    asm.push_str(&format!("; Variable: {}\n", name));
                    asm.push_str(&self.compile_expression(value)?);
                }
                _ => {
                    asm.push_str("; [Other statement]\n");
                }
            }
        }
        
        // SSD: Handle syntax extensions that need assembly calls
        for ext in &self.syntax_extensions {
            if let Some(label) = &ext.assembly_label {
                // This would be handled during parsing, but we show the concept here
                asm.push_str(&format!("    ; Syntax extension assembly call: {}\n", label));
                asm.push_str(&format!("    call {}\n", label));
            }
        }
        
        asm.push_str("\n    cli\n");
        asm.push_str("    hlt\n");
        asm.push_str("    jmp $\n\n");
        
        // ========== 64-bit subroutines ==========
        asm.push_str("print_string_64:\n");
        asm.push_str("    push rdi\n");
        asm.push_str(".loop:\n");
        asm.push_str("    lodsb\n");
        asm.push_str("    test al, al\n");
        asm.push_str("    jz .done\n");
        asm.push_str("    stosb\n");
        asm.push_str("    mov al, 0x0F\n");
        asm.push_str("    stosb\n");
        asm.push_str("    jmp .loop\n");
        asm.push_str(".done:\n");
        asm.push_str("    pop rdi\n");
        asm.push_str("    ret\n\n");
        
        asm.push_str("print_decimal_64:\n");
        asm.push_str("    ; Print decimal number in RAX\n");
        asm.push_str("    push rdi\n");
        asm.push_str("    push rcx\n");
        asm.push_str("    push rdx\n");
        asm.push_str("    push rbx\n");
        asm.push_str("    \n");
        asm.push_str("    mov rdi, 0xB8000 + 160  ; Second line\n");
        asm.push_str("    mov rcx, 0\n");
        asm.push_str("    mov rbx, 10\n");
        asm.push_str(".div_loop:\n");
        asm.push_str("    xor rdx, rdx\n");
        asm.push_str("    div rbx\n");
        asm.push_str("    push dx\n");
        asm.push_str("    inc rcx\n");
        asm.push_str("    test rax, rax\n");
        asm.push_str("    jnz .div_loop\n");
        asm.push_str(".print_loop:\n");
        asm.push_str("    pop ax\n");
        asm.push_str("    add al, '0'\n");
        asm.push_str("    stosb\n");
        asm.push_str("    mov al, 0x0F\n");
        asm.push_str("    stosb\n");
        asm.push_str("    loop .print_loop\n");
        asm.push_str("    \n");
        asm.push_str("    pop rbx\n");
        asm.push_str("    pop rdx\n");
        asm.push_str("    pop rcx\n");
        asm.push_str("    pop rdi\n");
        asm.push_str("    ret\n\n");
        
        // ========== GDTs ==========
        asm.push_str("gdt32:\n");
        asm.push_str("    dq 0x0000000000000000\n");
        asm.push_str("    dq 0x00CF9A000000FFFF\n");
        asm.push_str("    dq 0x00CF92000000FFFF\n");
        asm.push_str("gdt32_end:\n\n");
        
        asm.push_str("gdt32_desc:\n");
        asm.push_str("    dw gdt32_end - gdt32 - 1\n");
        asm.push_str("    dd gdt32\n\n");
        
        asm.push_str("gdt64:\n");
        asm.push_str("    dq 0x0000000000000000\n");
        asm.push_str("    dq 0x00209A0000000000\n");
        asm.push_str("    dq 0x0000920000000000\n");
        asm.push_str("gdt64_end:\n\n");
        
        asm.push_str("gdt64_desc:\n");
        asm.push_str("    dw gdt64_end - gdt64 - 1\n");
        asm.push_str("    dq gdt64\n\n");
        
        // ========== Data ==========
        asm.push_str("msg_64:\n");
        asm.push_str("    db 'Rython 64-bit', 0\n\n");
        
        // String literals
        asm.push_str("; String literals\n");
        asm.push_str(&self.generate_string_data());
        
        // SSD: Inject external assembly
        asm = self.inject_external_asm(&asm, &self.external_asm);
        
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
            Expr::Number(n, _) => {
                code.push_str(&format!("    ; Number: {}\n", n));
                code.push_str(&format!("    mov rax, {}\n", n));
                code.push_str("    call print_decimal_64\n");
            }
            Expr::String(s, _) => {
                let label = self.get_string_label(s);
                code.push_str(&format!("    ; String: '{}'\n", s));
                code.push_str(&format!("    mov rsi, {}\n", label));
                code.push_str("    mov rdi, 0xB8000 + 320  ; Third line\n");
                code.push_str("    call print_string_64\n");
            }
            Expr::Call { func, args, kwargs: _, span: _ } => {
                if func == "print" {
                    for arg in args {
                        let arg_code = self.compile_expression(arg)?;
                        code.push_str(&arg_code);
                    }
                } else {
                    code.push_str(&format!("    call {}\n", func));
                }
            }
            Expr::BinOp { left, op, right, span: _ } => {
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

pub struct Linux64Backend {
    string_counter: RefCell<u32>,
    string_literals: RefCell<HashMap<String, String>>,
    external_asm: Vec<String>, // SSD: Store external assembly
}

impl Linux64Backend {
    pub fn new() -> Self {
        Self {
            string_counter: RefCell::new(0),
            string_literals: RefCell::new(HashMap::new()),
            external_asm: Vec::new(),
        }
    }
    
    #[allow(dead_code)]
    pub fn add_external_asm(&mut self, asm: &str) {
        self.external_asm.push(asm.to_string());
    }
    
    fn get_string_label(&self, content: &str) -> String {
        let mut literals = self.string_literals.borrow_mut();
        if let Some(label) = literals.get(content) {
            return label.clone();
        }
        
        let mut counter = self.string_counter.borrow_mut();
        let label = format!("str_{}", *counter);
        *counter += 1;
        literals.insert(content.to_string(), label.clone());
        label
    }
    
    fn generate_string_data(&self) -> String {
        let literals = self.string_literals.borrow();
        let mut data = String::new();
        for (content, label) in &*literals {
            data.push_str(&format!("{}:\n", label));
            data.push_str(&format!("    db '{}', 0\n", content.replace("'", "''")));
        }
        data
    }
}

impl Backend for Linux64Backend {
    fn name(&self) -> &str {
        "linux64"
    }
    
    fn generate_header(&self) -> String {
        String::from("; Linux 64-bit Backend\n    bits 64\n    default rel\n\n    section .text\n    global _start\n\n")
    }
    
    fn format(&self) -> &'static str {
        "elf64"
    }
    
    fn supported_capabilities(&self) -> Vec<Capability> {
        vec![
            Capability::Linux,
            Capability::LongMode64,
            Capability::VirtualMemory,
        ]
    }
    
    fn compile_program(&mut self, program: &Program) -> Result<String, String> {
        let mut asm = String::new();
        
        asm.push_str("; Rython Linux 64-bit Backend\n");
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
        
        // String print function
        asm.push_str("\n; Print string function\n");
        asm.push_str("print_string:\n");
        asm.push_str("    ; rsi = string address\n");
        asm.push_str("    push rcx\n");
        asm.push_str("    push rdx\n");
        asm.push_str("    push rdi\n");
        asm.push_str("    push rsi\n");
        asm.push_str("    \n");
        asm.push_str("    ; Calculate string length\n");
        asm.push_str("    mov rdi, rsi\n");
        asm.push_str("    xor rcx, rcx\n");
        asm.push_str("    dec rcx\n");
        asm.push_str(".count_loop:\n");
        asm.push_str("    inc rcx\n");
        asm.push_str("    cmp byte [rdi + rcx], 0\n");
        asm.push_str("    jne .count_loop\n");
        asm.push_str("    \n");
        asm.push_str("    ; Write to stdout\n");
        asm.push_str("    mov rax, 1        ; sys_write\n");
        asm.push_str("    mov rdi, 1        ; stdout\n");
        asm.push_str("    mov rdx, rcx      ; length\n");
        asm.push_str("    syscall\n");
        asm.push_str("    \n");
        asm.push_str("    pop rsi\n");
        asm.push_str("    pop rdi\n");
        asm.push_str("    pop rdx\n");
        asm.push_str("    pop rcx\n");
        asm.push_str("    ret\n\n");
        
        // Data section
        asm.push_str("    section .data\n");
        asm.push_str(&self.generate_string_data());
        
        // SSD: Inject external assembly
        asm = self.inject_external_asm(&asm, &self.external_asm);
        
        Ok(asm)
    }
    
    fn function_prologue(&self, func: &BackendFunction) -> String {
        format!("{}:\n    push rbp\n    mov rbp, rsp\n", func.name)
    }
    
    fn function_epilogue(&self, _func: &BackendFunction) -> String {
        "    mov rsp, rbp\n    pop rbp\n    ret\n".to_string()
    }
    
    fn compile_expression(&self, expr: &Expr) -> Result<String, String> {
        match expr {
            Expr::Number(n, _) => Ok(format!("    ; Number: {}\n    mov rax, {}\n", n, n)),
            Expr::String(s, _) => {
                let label = self.get_string_label(s);
                Ok(format!(
                    "    ; String: '{}'\n    mov rsi, {}\n    call print_string\n",
                    s, label
                ))
            }
            Expr::Call { func, args, kwargs: _, span: _ } if func == "print" => {
                if let Some(arg) = args.get(0) {
                    self.compile_expression(arg)
                } else {
                    Ok("    ; Empty print\n".to_string())
                }
            }
            _ => Ok(format!("    ; {:?}\n", expr)),
        }
    }
}

// Add this struct to track variables
#[derive(Debug, Clone)]
pub struct VariableInfo {
    #[allow(dead_code)]
    pub name: String,
    pub offset: i32,  // Negative offset from rbp
    #[allow(dead_code)]
    pub type_hint: Option<String>,
}

// ========== WINDOWS 64-BIT BACKEND ==========

pub struct Windows64Backend {
    string_counter: RefCell<u32>,
    string_literals: RefCell<HashMap<String, String>>,
    symbol_table: RefCell<HashMap<String, VariableInfo>>,
    current_stack_offset: RefCell<i32>,
    external_asm: Vec<String>, // SSD: Store external assembly
    #[allow(dead_code)]
    syntax_extensions: Vec<SyntaxExtension>, // SSD: Syntax extensions
}

impl Windows64Backend {
    pub fn new() -> Self {
        Self {
            string_counter: RefCell::new(0),
            string_literals: RefCell::new(HashMap::new()),
            symbol_table: RefCell::new(HashMap::new()),
            current_stack_offset: RefCell::new(8), // Start at rbp-8
            external_asm: Vec::new(),
            syntax_extensions: Vec::new(),
        }
    }
    
    #[allow(dead_code)]
    pub fn add_external_asm(&mut self, asm: &str) {
        self.external_asm.push(asm.to_string());
    }
    
    fn allocate_variable(&self, _name: &str) -> i32 {
        let mut offset = self.current_stack_offset.borrow_mut();
        let current = *offset;
        *offset += 8; // Each variable takes 8 bytes (64-bit pointer)
        current
    }
    
    fn get_variable_offset(&self, name: &str) -> Option<i32> {
        self.symbol_table.borrow().get(name).map(|v| v.offset)
    }
    
    fn ensure_variable_exists(&self, name: &str) -> i32 {
        if let Some(offset) = self.get_variable_offset(name) {
            offset
        } else {
            self.allocate_variable(name)
        }
    }
    
    fn generate_string_data(&self) -> String {
        let literals = self.string_literals.borrow();
        let mut data = String::new();
        for (content, label) in &*literals {
            data.push_str(&format!("{} db '{}', 0\n", label, content.replace("'", "''")));
        }
        data
    }
    
    fn get_string_label(&self, content: &str) -> String {
        let mut literals = self.string_literals.borrow_mut();
        if let Some(label) = literals.get(content) {
            return label.clone();
        }
        
        let mut counter = self.string_counter.borrow_mut();
        let label = format!("str_{}", *counter);
        *counter += 1;
        literals.insert(content.to_string(), label.clone());
        label
    }
    
    fn count_variables(&self, program: &Program) -> usize {
        let mut count = 0;
        for stmt in &program.body {
            match stmt {           
                Statement::Assign { target: _, value: _, span: _ } => {
                    count += 1;
                }
                _ => {}
            }
        }
        count
    }
    
    // SSD: Handle FFI register passing for external assembly
    #[allow(dead_code)]
    fn handle_ffi_call(&self, func_label: &str, args: &[Expr], registers: &[String]) -> Result<String, String> {
        let mut asm = String::new();
        
        asm.push_str(&format!("    ; FFI call to {}\n", func_label));
        
        // Prepare arguments in registers
        for (i, (arg, reg)) in args.iter().zip(registers.iter()).enumerate() {
            match arg {
                Expr::Number(n, _) => {
                    asm.push_str(&format!("    mov {}, {}\n", reg, n));
                }
                Expr::Var(name, _) => {
                    if let Some(offset) = self.get_variable_offset(name) {
                        asm.push_str(&format!("    mov {}, [rbp - {}]\n", reg, offset));
                    } else {
                        return Err(format!("Variable {} not found", name));
                    }
                }
                _ => {
                    return Err("Unsupported argument type for FFI".to_string());
                }
            }
            
            // Windows 64-bit uses RCX, RDX, R8, R9 for first 4 arguments
            if i >= 3 {
                break;
            }
        }
        
        // Align stack for Windows calling convention
        asm.push_str("    sub rsp, 32          ; Shadow space\n");
        asm.push_str(&format!("    call {}\n", func_label));
        asm.push_str("    add rsp, 32          ; Clean up shadow space\n");
        
        Ok(asm)
    }
}

impl Backend for Windows64Backend {
    fn name(&self) -> &str {
        "windows64"
    }
    
    fn generate_header(&self) -> String {
        String::from("; Windows 64-bit Backend\n    bits 64\n    default rel\n\n    section .text\n")
    }
    
    fn format(&self) -> &'static str {
        "win64"
    }
    
    fn supported_capabilities(&self) -> Vec<Capability> {
        vec![
            Capability::Windows,
            Capability::LongMode64,
        ]
    }
    
    fn compile_program(&mut self, program: &Program) -> Result<String, String> {
        let mut asm = String::new();
        
        asm.push_str("; Rython Windows 64-bit Backend\n");
        asm.push_str("; Generated from Rython AST\n\n");
        asm.push_str("    bits 64\n");
        asm.push_str("    default rel\n\n");
        
        asm.push_str("    section .text\n");
        asm.push_str("    extern ExitProcess\n");
        asm.push_str("    extern printf\n");
        asm.push_str("    extern putchar\n");
        asm.push_str("    global main\n\n");
        
        asm.push_str("main:\n");
        asm.push_str("    push rbp\n");
        asm.push_str("    mov rbp, rsp\n");
        
        // Allocate space for variables (calculate based on number of variables)
        let var_count = self.count_variables(program);
        let stack_size = 32 + (var_count * 8); // Shadow space + variables
        asm.push_str(&format!("    sub rsp, {}          ; Allocate shadow space and variable space\n", stack_size));
        asm.push_str("    and rsp, -16         ; Align stack to 16 bytes\n\n");
        
        // Compile statements
        for stmt in &program.body {
            match stmt {
                Statement::Expr(expr) => {
                    asm.push_str(&self.compile_expression(expr)?);
                }
                Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                    // Allocate space for variable
                    let offset = self.allocate_variable(name);
                    self.symbol_table.borrow_mut().insert(name.clone(), VariableInfo {
                        name: name.clone(),
                        offset,
                        type_hint: None,
                    });
                    
                    // Compile the value
                    asm.push_str(&format!("    ; Variable declaration: {}\n", name));
                    asm.push_str(&self.compile_expression(value)?);
                    
                    // Store the value at [rbp - offset]
                    asm.push_str(&format!("    mov [rbp - {}], rax\n", offset));
                }
                Statement::Assign { target, value, span: _ } => {
                    // Ensure variable exists (allocate if not)
                    let offset = self.ensure_variable_exists(target);
                    
                    // If the variable wasn't already in the symbol table, add it
                    if !self.symbol_table.borrow().contains_key(target) {
                        self.symbol_table.borrow_mut().insert(target.clone(), VariableInfo {
                            name: target.clone(),
                            offset,
                            type_hint: None,
                        });
                    }
                    
                    // Compile the value
                    asm.push_str(&format!("    ; Assignment: {} = ...\n", target));
                    asm.push_str(&self.compile_expression(value)?);
                    
                    // Store the value at [rbp - offset]
                    asm.push_str(&format!("    mov [rbp - {}], rax\n", offset));
                }
                _ => {
                    asm.push_str("    ; [Unsupported statement]\n");
                }
            }
        }
        
        // Windows exit
        asm.push_str("\n    ; Exit\n");
        asm.push_str("    xor ecx, ecx          ; exit code 0\n");
        asm.push_str("    call ExitProcess\n");
        
        // Helper function to convert integer to string
        asm.push_str("\n; Helper function to convert integer to string\n");
        asm.push_str("int_to_string:\n");
        asm.push_str("    ; Input: rax = integer\n");
        asm.push_str("    ; Output: rdi = pointer to null-terminated string\n");
        asm.push_str("    push rbp\n");
        asm.push_str("    mov rbp, rsp\n");
        asm.push_str("    sub rsp, 32          ; Allocate space for local buffer\n");
        asm.push_str("    \n");
        asm.push_str("    ; Point rdi to end of buffer\n");
        asm.push_str("    lea rdi, [rbp - 32 + 30]\n");
        asm.push_str("    mov byte [rdi], 0    ; Null terminator\n");
        asm.push_str("    \n");
        asm.push_str("    ; Handle negative numbers\n");
        asm.push_str("    test rax, rax\n");
        asm.push_str("    jns .positive\n");
        asm.push_str("    neg rax\n");
        asm.push_str("    mov byte [rbp - 32 + 29], '-'\n");
        asm.push_str("    dec rdi\n");
        asm.push_str("    mov byte [rdi], '-'\n");
        asm.push_str("    \n");
        asm.push_str(".positive:\n");
        asm.push_str("    mov rbx, 10          ; Base 10\n");
        asm.push_str("    \n");
        asm.push_str(".convert_loop:\n");
        asm.push_str("    xor rdx, rdx\n");
        asm.push_str("    div rbx\n");
        asm.push_str("    add dl, '0'\n");
        asm.push_str("    dec rdi\n");
        asm.push_str("    mov [rdi], dl\n");
        asm.push_str("    test rax, rax\n");
        asm.push_str("    jnz .convert_loop\n");
        asm.push_str("    \n");
        asm.push_str("    ; Return pointer to string\n");
        asm.push_str("    mov rax, rdi\n");
        asm.push_str("    leave\n");
        asm.push_str("    ret\n\n");
        
        // Print newline function
        asm.push_str("; Print newline function\n");
        asm.push_str("print_newline:\n");
        asm.push_str("    push rcx\n");
        asm.push_str("    push rdx\n");
        asm.push_str("    push r8\n");
        asm.push_str("    push r9\n");
        asm.push_str("    sub rsp, 32          ; Shadow space\n");
        asm.push_str("    \n");
        asm.push_str("    mov rcx, 10          ; ASCII code for newline\n");
        asm.push_str("    call putchar\n");
        asm.push_str("    \n");
        asm.push_str("    add rsp, 32\n");
        asm.push_str("    pop r9\n");
        asm.push_str("    pop r8\n");
        asm.push_str("    pop rdx\n");
        asm.push_str("    pop rcx\n");
        asm.push_str("    ret\n\n");
        
        asm.push_str("    section .data\n");
        asm.push_str("printf_format_string:\n");
        asm.push_str("    db '%s', 0           ; printf format string for strings\n");
        asm.push_str("printf_format_int:\n");
        asm.push_str("    db '%d', 0           ; printf format string for integers\n\n");
        asm.push_str(&self.generate_string_data());
        
        // SSD: Inject external assembly
        asm = self.inject_external_asm(&asm, &self.external_asm);
        
        Ok(asm)
    }
    
    fn function_prologue(&self, func: &BackendFunction) -> String {
        format!("{}:\n    push rbp\n    mov rbp, rsp\n", func.name)
    }
    
    fn function_epilogue(&self, _func: &BackendFunction) -> String {
        "    mov rsp, rbp\n    pop rbp\n    ret\n".to_string()
    }
    
    fn compile_expression(&self, expr: &Expr) -> Result<String, String> {
        let mut code = String::new();
        
        match expr {
            Expr::Number(n, _) => {
                code.push_str(&format!("    ; Number: {}\n", n));
                code.push_str(&format!("    mov rax, {}\n", n));
                
                // For Windows, we need to pass the number correctly to printf
                // We'll use the %d format specifier for integers
                code.push_str("    mov rdx, rax          ; Second arg: integer value\n");
                code.push_str("    lea rcx, [printf_format_int] ; First arg: format string for integer\n");
                code.push_str("    sub rsp, 32          ; Allocate shadow space\n");
                code.push_str("    xor rax, rax          ; No floating point args\n");
                code.push_str("    call printf\n");
                code.push_str("    add rsp, 32          ; Clean up shadow space\n");
                
                // Automatically add newline after print
                code.push_str("    call print_newline\n");
            }
            Expr::String(s, _) => {
                let label = self.get_string_label(s);
                code.push_str(&format!("    ; String: '{}'\n", s));
                code.push_str(&format!("    lea rax, [{}]\n", label));
                
                // For strings, use %s format specifier
                code.push_str("    mov rdx, rax          ; Second arg: string address\n");
                code.push_str("    lea rcx, [printf_format_string] ; First arg: format string for string\n");
                code.push_str("    sub rsp, 32          ; Allocate shadow space\n");
                code.push_str("    xor rax, rax          ; No floating point args\n");
                code.push_str("    call printf\n");
                code.push_str("    add rsp, 32          ; Clean up shadow space\n");
                
                // Automatically add newline after print
                code.push_str("    call print_newline\n");
            }
            Expr::Var(name, _) => {
                // Look up variable offset
                let offset = self.get_variable_offset(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?;
                
                code.push_str(&format!("    ; Variable: {}\n", name));
                code.push_str(&format!("    mov rax, [rbp - {}]\n", offset));
                
                // For variables, we need to determine if they're strings or numbers
                // For now, assume they're numbers and use the int_to_string helper
                code.push_str("    call int_to_string    ; Convert integer to string\n");
                code.push_str("    mov rdx, rax          ; Second arg: string address\n");
                code.push_str("    lea rcx, [printf_format_string] ; First arg: format string\n");
                code.push_str("    sub rsp, 32          ; Allocate shadow space\n");
                code.push_str("    xor rax, rax          ; No floating point args\n");
                code.push_str("    call printf\n");
                code.push_str("    add rsp, 32          ; Clean up shadow space\n");
                
                // Automatically add newline after print
                code.push_str("    call print_newline\n");
            }
            Expr::Call { func, args, kwargs: _, span: _ } if func == "print" => {
                if let Some(arg) = args.get(0) {
                    // Handle each type of argument differently
                    match arg {
                        Expr::Number(_, _) | Expr::BinOp { .. } => {
                            // Compile the argument as a number
                            code.push_str(&self.compile_expression(arg)?);
                            
                            // The number is already in rax, so we can just print it
                            code.push_str("    mov rdx, rax          ; Second arg: integer value\n");
                            code.push_str("    lea rcx, [printf_format_int] ; First arg: format string for integer\n");
                            code.push_str("    sub rsp, 32          ; Allocate shadow space\n");
                            code.push_str("    xor rax, rax          ; No floating point args\n");
                            code.push_str("    call printf\n");
                            code.push_str("    add rsp, 32          ; Clean up shadow space\n");
                            
                            // Automatically add newline after print
                            code.push_str("    call print_newline\n");
                        }
                        Expr::String(_, _) => {
                            // Compile the string (which already includes newline)
                            code.push_str(&self.compile_expression(arg)?);
                        }
                        Expr::Var(_, _) => {
                            // For variables, we need to check their type
                            // For now, compile and let the variable handling code deal with it
                            code.push_str(&self.compile_expression(arg)?);
                        }
                        _ => {
                            return Err(format!("Unsupported print argument type: {:?}", arg));
                        }
                    }
                } else {
                    // Empty print() - just print a newline
                    code.push_str("    ; Empty print - just newline\n");
                    code.push_str("    call print_newline\n");
                }
            }
            Expr::BinOp { left, op, right, span: _ } => {
                // Compile left side
                code.push_str(&self.compile_expression(left)?);
                code.push_str("    push rax\n");
                
                // Compile right side
                code.push_str(&self.compile_expression(right)?);
                code.push_str("    mov rbx, rax\n");
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
                        return Err(format!("Unsupported operator: {:?}", op));
                    }
                }
                
            }
            _ => {
                return Err(format!("Unsupported expression: {:?}", expr));
            }
        }
        
        Ok(code)
    }
}

// Helper trait for string hashing
trait HashCode {
    fn hash_code(&self) -> u64;
}

impl HashCode for str {
    fn hash_code(&self) -> u64 {
        let mut hash: u64 = 5381;
        for &byte in self.as_bytes() {
            hash = ((hash << 5).wrapping_add(hash)).wrapping_add(byte as u64);
        }
        hash
    }
}