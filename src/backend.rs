
use crate::parser::{Program, Statement, Expr, Op};
use crate::dsl::{HardwareDSL};
use std::collections::HashMap;
use std::cell::RefCell;
use std::any::Any;

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

// Struct to track variables
#[derive(Debug, Clone)]
pub struct VariableInfo {
    #[allow(dead_code)]
    pub name: String,
    pub offset: i32,  // Negative offset from rbp
    #[allow(dead_code)]
    pub type_hint: Option<String>,
}

// Struct for function context
#[derive(Debug, Clone)]
struct FunctionContext {
    name: String,
    args: Vec<String>,
    locals: HashMap<String, VariableInfo>,
    return_label: String,
    has_returned: bool,
    stack_size: i32,
}

impl FunctionContext {
    fn new(name: String, args: Vec<String>) -> Self {
        let return_label = format!("{}_return", name.clone()); // FIXED: Clone name
        Self {
            name,
            args,
            locals: HashMap::new(),
            return_label,
            has_returned: false,
            stack_size: 0,
        }
    }
    
    fn add_local(&mut self, name: String, offset: i32, type_hint: Option<String>) {
        let var_name = name.clone(); // FIXED: Clone name before moving
        self.locals.insert(name, VariableInfo {
            name: var_name,
            offset,
            type_hint,
        });
    }
    
    fn get_local(&self, name: &str) -> Option<&VariableInfo> {
        self.locals.get(name)
    }
    
    fn allocate_local(&mut self) -> i32 {
        self.stack_size += 8;
        self.stack_size
    }
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
    fn compile_expression(&mut self, expr: &Expr) -> Result<String, String>;
    
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
    
    /// For downcasting
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

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
    
    fn compile_expression(&mut self, expr: &Expr) -> Result<String, String> {
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
    
    fn as_any(&self) -> &dyn Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}


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
    
    fn compile_expression(&mut self, expr: &Expr) -> Result<String, String> {
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
    
    fn as_any(&self) -> &dyn Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}


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
    hardware_dsl: HardwareDSL, // NEW: Hardware DSL for device code generation
    label_counter: RefCell<u32>, // For generating unique labels
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
            hardware_dsl: HardwareDSL::new(), // NEW: Initialize hardware DSL
            label_counter: RefCell::new(0),
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
    
    // NEW: Method to compile hardware function definitions
    fn compile_hardware_function_def(&self, device: &str, name: &str, args: &[String], body: &[Statement]) -> Result<String, String> {
        let mut asm = String::new();
        
        // Generate hardware-specific prologue
        match self.hardware_dsl.generate_device_function_prologue(device, name) {
            Ok(prologue) => {
                asm.push_str(&prologue);
            }
            Err(_) => {
                // Fall back to generic hardware function prologue
                asm.push_str(&format!("; Hardware function: {} for device {}\n", name, device));
                asm.push_str(&format!("{}_hw_{}:\n", name, device));
                asm.push_str("    push rbp\n");
                asm.push_str("    mov rbp, rsp\n");
                asm.push_str("    push rbx\n");
                asm.push_str("    push rcx\n");
                asm.push_str("    push rdx\n");
                asm.push_str("    push rsi\n");
                asm.push_str("    push rdi\n");
                asm.push_str("    ; Hardware-specific setup\n");
                asm.push_str(&format!("    ; Device: {}, Function: {}\n", device, name));
            }
        }
        
        // Handle arguments
        if !args.is_empty() {
            asm.push_str(&format!("    ; {} argument(s): {}\n", args.len(), args.join(", ")));
            for (i, arg) in args.iter().enumerate() {
                // Standard System V AMD64 calling convention for hardware functions
                match i {
                    0 => asm.push_str(&format!("    ; {} -> rdi\n", arg)),
                    1 => asm.push_str(&format!("    ; {} -> rsi\n", arg)),
                    2 => asm.push_str(&format!("    ; {} -> rdx\n", arg)),
                    3 => asm.push_str(&format!("    ; {} -> rcx\n", arg)),
                    4 => asm.push_str(&format!("    ; {} -> r8\n", arg)),
                    5 => asm.push_str(&format!("    ; {} -> r9\n", arg)),
                    _ => asm.push_str(&format!("    ; {} -> stack\n", arg)),
                }
            }
        }
        
        // Compile the body of the hardware function
        asm.push_str("    ; Hardware function body\n");
        for stmt in body {
            // For now, just add a comment - we'll implement proper compilation later
            asm.push_str(&format!("    ; Statement: {:?}\n", stmt));
        }
        
        // Generate hardware-specific epilogue
        match self.hardware_dsl.generate_device_function_epilogue(device) {
            Ok(epilogue) => {
                asm.push_str(&epilogue);
            }
            Err(_) => {
                // Fall back to generic hardware function epilogue
                asm.push_str("    ; Hardware function cleanup\n");
                asm.push_str("    pop rdi\n");
                asm.push_str("    pop rsi\n");
                asm.push_str("    pop rdx\n");
                asm.push_str("    pop rcx\n");
                asm.push_str("    pop rbx\n");
                asm.push_str("    mov rsp, rbp\n");
                asm.push_str("    pop rbp\n");
                asm.push_str("    ret\n");
            }
        }
        
        Ok(asm)
    }
    
    // Helper method to compile expressions without borrowing issues
    fn compile_expression_helper(&mut self, expr: &Expr, code: &mut String) -> Result<(), String> {
        match expr {
            Expr::Number(n, _) => {
                code.push_str(&format!("    ; Number: {}\n", n));
                code.push_str(&format!("    mov rax, {}\n", n));
                code.push_str("    call print_decimal_64\n");
                Ok(())
            }
            Expr::String(s, _) => {
                let label = self.get_string_label(s);
                code.push_str(&format!("    ; String: '{}'\n", s));
                code.push_str(&format!("    mov rsi, {}\n", label));
                code.push_str("    mov rdi, 0xB8000\n");
                code.push_str("    call print_string_64\n");
                Ok(())
            }
            Expr::Var(name, _) => {
                // BIOS64 doesn't have proper variable tracking yet
                code.push_str(&format!("    ; Variable: {} (not implemented in BIOS64)\n", name));
                // Return a dummy value
                code.push_str("    mov rax, 0\n");
                Ok(())
            }
            Expr::Call { func, args, kwargs: _, span: _ } => {
                // Check if it's a hardware intrinsic
                if func == "write_register" || func == "read_register" || 
                   func == "dma_transfer" || func == "port_in" || func == "port_out" ||
                   func.starts_with("hw_") {
                    
                    // Convert arguments to string representation for the DSL
                    let arg_strings: Vec<String> = args.iter()
                        .map(|arg| {
                            match arg {
                                Expr::Number(n, _) => n.to_string(),
                                Expr::String(s, _) => format!("\"{}\"", s),
                                Expr::Var(name, _) => name.clone(),
                                _ => "0".to_string(),
                            }
                        })
                        .collect();
                    
                    // Build the hardware statement string
                    let stmt_str = if !arg_strings.is_empty() {
                        format!("{}({})", func, arg_strings.join(", "))
                    } else {
                        func.clone()
                    };
                    
                    // Use the hardware DSL to parse and generate assembly
                    match self.hardware_dsl.parse_hardware_statement(&stmt_str) {
                        Ok(asm_lines) => {
                            code.push_str(&format!("    ; Hardware intrinsic: {}\n", stmt_str));
                            for line in asm_lines {
                                code.push_str(&format!("    {}\n", line));
                            }
                        }
                        Err(_) => {
                            // Fall back to generic hardware call
                            code.push_str(&format!("    ; Hardware call: {}\n", func));
                            code.push_str(&format!("    ; Generic hardware call for {}\n", func));
                            
                            // Handle specific hardware intrinsics
                            if func == "write_register" && args.len() == 2 {
                                code.push_str("    ; write_register(reg, value)\n");
                                // Compile register address
                                self.compile_expression_helper(&args[0], code)?;
                                code.push_str("    push rax\n");
                                
                                // Compile value
                                self.compile_expression_helper(&args[1], code)?;
                                code.push_str("    mov rdx, rax\n");
                                code.push_str("    pop rax\n");
                                code.push_str("    mov rdi, rax\n");
                                code.push_str("    ; Actually write to register (placeholder)\n");
                                code.push_str("    ; out dx, di\n");
                            }
                            else if func == "read_register" && args.len() == 1 {
                                code.push_str("    ; read_register(reg)\n");
                                // Compile register address
                                self.compile_expression_helper(&args[0], code)?;
                                code.push_str("    mov rdi, rax\n");
                                code.push_str("    ; Actually read from register (placeholder)\n");
                                code.push_str("    ; in ax, dx\n");
                                code.push_str("    mov rax, 0x12345678 ; Placeholder value\n");
                            }
                            else if func == "port_out" && args.len() == 2 {
                                code.push_str("    ; port_out(port, value)\n");
                                // Compile port
                                self.compile_expression_helper(&args[0], code)?;
                                code.push_str("    mov dx, ax\n");
                                
                                // Compile value
                                self.compile_expression_helper(&args[1], code)?;
                                code.push_str("    ; out dx, ax\n");
                            }
                            else if func == "port_in" && args.len() == 1 {
                                code.push_str("    ; port_in(port)\n");
                                // Compile port
                                self.compile_expression_helper(&args[0], code)?;
                                code.push_str("    mov dx, ax\n");
                                code.push_str("    ; in ax, dx\n");
                                code.push_str("    mov rax, 0xDEADBEEF ; Placeholder value\n");
                            }
                            else if func == "dma_transfer" && args.len() == 2 {
                                code.push_str("    ; dma_transfer(address, data)\n");
                                // Compile address
                                self.compile_expression_helper(&args[0], code)?;
                                code.push_str("    push rax\n");
                                
                                // Compile data
                                self.compile_expression_helper(&args[1], code)?;
                                code.push_str("    mov rsi, rax\n");
                                code.push_str("    pop rdi\n");
                                code.push_str("    ; Setup DMA transfer (placeholder)\n");
                                code.push_str("    mov rcx, 512 ; Default transfer size\n");
                                code.push_str("    rep movsb\n");
                            }
                            else {
                                return Err(format!("Unsupported hardware intrinsic: {} with {} args", func, args.len()));
                            }
                        }
                    }
                }
                else if func == "print" {
                    if let Some(arg) = args.get(0) {
                        // Handle each type of argument differently
                        match arg {
                            Expr::Number(_, _) => {
                                // Compile the argument as a number
                                self.compile_expression_helper(arg, code)?;
                                
                                // After compilation, rax contains the result
                                code.push_str("    call print_decimal_64\n");
                            }
                            Expr::String(_, _) => {
                                // Compile the string
                                self.compile_expression_helper(arg, code)?;
                            }
                            Expr::Var(_, _) => {
                                // For variables, we need to check their type
                                // For now, compile and let the variable handling code deal with it
                                self.compile_expression_helper(arg, code)?;
                            }
                            // Handle binary operations - the issue was here
                            Expr::BinOp { .. } => {
                                // Handle binary operations by compiling the whole expression
                                // This will recursively compile the operation
                                self.compile_expression_helper(arg, code)?;
                                
                                // After compilation, rax contains the result
                                code.push_str("    call print_decimal_64\n");
                            }
                            _ => {
                                return Err(format!("Unsupported print argument type: {:?}", arg));
                            }
                        }
                    } else {
                        // Empty print() - just print a newline
                        code.push_str("    ; Empty print - just newline\n");
                        // BIOS64 doesn't have a newline function, so we print a space
                        code.push_str("    mov rsi, newline_str\n");
                        code.push_str("    mov rdi, 0xB8000\n");
                        code.push_str("    call print_string_64\n");
                    }
                }
                else {
                    // Regular function call
                    code.push_str(&format!("    call {}\n", func));
                }
                Ok(())
            }
            Expr::BinOp { left, op, right, span: _ } => {
                // Compile left side
                self.compile_expression_helper(left, code)?;
                code.push_str("    push rax\n");
                
                // Compile right side
                self.compile_expression_helper(right, code)?;
                code.push_str("    mov rbx, rax\n");
                code.push_str("    pop rax\n");
                
                match op {
                    Op::Add => {
                        code.push_str("    add rax, rbx\n");
                    }
                    Op::Sub => {
                        code.push_str("    sub rax, rbx\n");
                    }
                    Op::Mul => {
                        code.push_str("    imul rax, rbx\n");
                    }
                    Op::Div => {
                        code.push_str("    xor rdx, rdx\n");
                        code.push_str("    idiv rbx\n");
                    }
                    Op::Mod => {
                        code.push_str("    xor rdx, rdx\n");
                        code.push_str("    div rbx\n");
                        code.push_str("    mov rax, rdx\n"); // Remainder
                    }
                    _ => {
                        return Err(format!("Unsupported operator: {:?}", op));
                    }
                }
                Ok(())
            }
            _ => {
                Err(format!("Unsupported expression: {:?}", expr))
            }
        }
    }
    
    fn get_next_label_id(&self) -> u32 {
        let mut counter = self.label_counter.borrow_mut();
        let id = *counter;
        *counter += 1;
        id
    }
    
    fn compile_function(&mut self, name: &str, args: &[String], body: &[Statement]) -> Result<String, String> {
        let mut asm = String::new();
        let mut func_ctx = FunctionContext::new(name.to_string(), args.to_vec());
        
        // Function prologue
        asm.push_str(&format!("{}:\n", name));
        asm.push_str("    push rbp\n");
        asm.push_str("    mov rbp, rsp\n");
        
        // Allocate space for locals
        let local_count = body.iter()
            .filter(|stmt| matches!(stmt, Statement::VarDecl { .. }))
            .count();
        
        if local_count > 0 {
            let stack_size = ((local_count * 8) + 15) & !15; // Align to 16 bytes
            asm.push_str(&format!("    sub rsp, {}   ; Allocate space for locals\n", stack_size));
            func_ctx.stack_size = stack_size as i32;
        }
        
        // Setup arguments (System V AMD64 calling convention for BIOS functions)
        for (i, arg) in args.iter().enumerate() {
            match i {
                0 => {
                    asm.push_str(&format!("    ; {} -> rdi\n", arg));
                    asm.push_str("    mov [rbp - 8], rdi\n"); // Store first arg
                    func_ctx.add_local(arg.clone(), 8, None);
                }
                1 => {
                    asm.push_str(&format!("    ; {} -> rsi\n", arg));
                    asm.push_str("    mov [rbp - 16], rsi\n"); // Store second arg
                    func_ctx.add_local(arg.clone(), 16, None);
                }
                2 => {
                    asm.push_str(&format!("    ; {} -> rdx\n", arg));
                    asm.push_str("    mov [rbp - 24], rdx\n"); // Store third arg
                    func_ctx.add_local(arg.clone(), 24, None);
                }
                3 => {
                    asm.push_str(&format!("    ; {} -> rcx\n", arg));
                    asm.push_str("    mov [rbp - 32], rcx\n"); // Store fourth arg
                    func_ctx.add_local(arg.clone(), 32, None);
                }
                _ => {
                    // Additional args on stack
                    let offset = 16 + (i - 4) * 8;
                    asm.push_str(&format!("    ; {} -> [rbp + {}]\n", arg, offset));
                    let offset_i32 = offset as i32; // FIXED: Convert to i32 first
                    func_ctx.add_local(arg.clone(), -offset_i32, None); // Now negate
                }
            }
        }
        
        // Compile function body
        for stmt in body {
            match stmt {
                Statement::Expr(expr) => {
                    let expr_code = self.compile_expression(expr)?;
                    asm.push_str(&expr_code);
                }
                Statement::Return(expr) => {
                    if let Some(expr) = expr {
                        let expr_code = self.compile_expression(expr)?;
                        asm.push_str(&expr_code);
                        // Result is already in rax
                    } else {
                        asm.push_str("    xor rax, rax\n"); // Return 0 for void
                    }
                    
                    // Function epilogue
                    if func_ctx.stack_size > 0 {
                        asm.push_str(&format!("    add rsp, {}\n", func_ctx.stack_size));
                    }
                    asm.push_str("    mov rsp, rbp\n");
                    asm.push_str("    pop rbp\n");
                    asm.push_str("    ret\n");
                    func_ctx.has_returned = true;
                }
                Statement::VarDecl { name: var_name, value, type_hint: _, span: _ } => {
                    let offset = func_ctx.allocate_local();
                    func_ctx.add_local(var_name.clone(), offset, None);
                    
                    asm.push_str(&format!("    ; Local variable: {}\n", var_name));
                    let expr_code = self.compile_expression(value)?;
                    asm.push_str(&expr_code);
                    asm.push_str(&format!("    mov [rbp - {}], rax\n", offset));
                }
                Statement::Assign { target, value, span: _ } => {
                    if let Some(var_info) = func_ctx.get_local(target) {
                        asm.push_str(&format!("    ; Assignment to local: {}\n", target));
                        let expr_code = self.compile_expression(value)?;
                        asm.push_str(&expr_code);
                        asm.push_str(&format!("    mov [rbp - {}], rax\n", var_info.offset));
                    } else if args.contains(&target) {
                        asm.push_str(&format!("    ; Assignment to argument: {}\n", target));
                        let expr_code = self.compile_expression(value)?;
                        asm.push_str(&expr_code);
                        let arg_index = args.iter().position(|a| a == target).unwrap();
                        if arg_index < 4 {
                            let offset = (arg_index + 1) * 8;
                            asm.push_str(&format!("    mov [rbp - {}], rax\n", offset));
                        } else {
                            let offset = 16 + (arg_index - 4) * 8;
                            asm.push_str(&format!("    mov [rbp + {}], rax\n", offset));
                        }
                    } else {
                        return Err(format!("Undefined variable in function: {}", target));
                    }
                }
                Statement::If { condition, then_block, elif_blocks, else_block, span: _ } => {
                    let label_id = self.get_next_label_id();
                    let else_label = format!("{}_{}_else", name, label_id);
                    let end_label = format!("{}_{}_end", name, label_id);
                    
                    // Compile condition
                    asm.push_str(&format!("    ; If condition in {}\n", name));
                    let cond_code = self.compile_expression(&condition)?;
                    asm.push_str(&cond_code);
                    
                    // Test condition
                    asm.push_str("    test rax, rax\n");
                    asm.push_str(&format!("    jz {}\n", else_label));
                    
                    // Then block
                    asm.push_str(&format!("    ; Then block in {}\n", name));
                    for stmt in then_block {
                        let stmt_code = self.compile_statement(stmt)?;
                        asm.push_str(&stmt_code);
                    }
                    asm.push_str(&format!("    jmp {}\n", end_label));
                    
                    // Else if blocks
                    for (elif_cond, elif_body) in elif_blocks {
                        asm.push_str(&format!("{}:\n", else_label));
                        asm.push_str(&format!("    ; Else if condition in {}\n", name));
                        let elif_cond_code = self.compile_expression(&elif_cond)?;
                        asm.push_str(&elif_cond_code);
                        asm.push_str("    test rax, rax\n");
                        asm.push_str(&format!("    jz {}_elif\n", else_label));
                        
                        // Else if body
                        asm.push_str(&format!("    ; Else if body in {}\n", name));
                        for stmt in elif_body {
                            let stmt_code = self.compile_statement(stmt)?;
                            asm.push_str(&stmt_code);
                        }
                        asm.push_str(&format!("    jmp {}\n", end_label));
                        asm.push_str(&format!("{}_elif:\n", else_label));
                    }
                    
                    // Else block
                    if let Some(else_body) = else_block {
                        if elif_blocks.is_empty() {
                            asm.push_str(&format!("{}:\n", else_label));
                        }
                        asm.push_str(&format!("    ; Else block in {}\n", name));
                        for stmt in else_body {
                            let stmt_code = self.compile_statement(&stmt)?;
                            asm.push_str(&stmt_code);
                        }
                    } else {
                        if elif_blocks.is_empty() {
                            asm.push_str(&format!("{}:\n", else_label));
                        }
                    }
                    
                    asm.push_str(&format!("{}:\n", end_label));
                }
                Statement::While { condition, body, orelse: _, span: _ } => {
                    let label_id = self.get_next_label_id();
                    let while_start = format!("{}_{}_while_start", name, label_id);
                    let while_end = format!("{}_{}_while_end", name, label_id);
                    
                    asm.push_str(&format!("{}:\n", while_start));
                    
                    // Compile condition
                    asm.push_str(&format!("    ; While condition in {}\n", name));
                    let cond_code = self.compile_expression(&condition)?;
                    asm.push_str(&cond_code);
                    
                    // Test condition
                    asm.push_str("    test rax, rax\n");
                    asm.push_str(&format!("    jz {}\n", while_end));
                    
                    // Body
                    asm.push_str(&format!("    ; While body in {}\n", name));
                    for stmt in body {
                        let stmt_code = self.compile_statement(stmt)?;
                        asm.push_str(&stmt_code);
                    }
                    
                    asm.push_str(&format!("    jmp {}\n", while_start));
                    asm.push_str(&format!("{}:\n", while_end));
                }
                Statement::Pass => {
                    asm.push_str("    ; pass\n");
                }
                Statement::Break => {
                    asm.push_str("    ; break (would need loop context)\n");
                }
                Statement::Continue => {
                    asm.push_str("    ; continue (would need loop context)\n");
                }
                _ => {
                    asm.push_str(&format!("    ; [Statement type in function {}: {:?}]\n", name, stmt));
                }
            }
        }
        
        // If function doesn't end with return, add default epilogue
        if !func_ctx.has_returned {
            asm.push_str("    ; Default return\n");
            asm.push_str("    xor rax, rax\n"); // Return 0
            
            if func_ctx.stack_size > 0 {
                asm.push_str(&format!("    add rsp, {}\n", func_ctx.stack_size));
            }
            asm.push_str("    mov rsp, rbp\n");
            asm.push_str("    pop rbp\n");
            asm.push_str("    ret\n");
        }
        
        Ok(asm)
    }
    
    fn compile_statement(&mut self, stmt: &Statement) -> Result<String, String> {
        match stmt {
            Statement::Expr(expr) => self.compile_expression(expr),
            Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                let mut code = String::new();
                code.push_str(&format!("; Variable: {}\n", name));
                code.push_str(&self.compile_expression(value)?);
                Ok(code)
            }
            Statement::Assign { target, value, span: _ } => {
                let mut code = String::new();
                code.push_str(&format!("; Assignment: {} = \n", target));
                code.push_str(&self.compile_expression(value)?);
                Ok(code)
            }
            _ => Ok(format!("    ; [Statement: {:?}]\n", stmt)),
        }
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
        
        // Separate main code from function definitions
        let mut functions = Vec::new();
        let mut main_statements = Vec::new();
        
        for stmt in &program.body {
            match stmt {
                Statement::FunctionDef { .. } => {
                    functions.push(stmt.clone());
                }
                Statement::HardwareFunctionDef { .. } => {
                    functions.push(stmt.clone());
                }
                _ => {
                    main_statements.push(stmt.clone());
                }
            }
        }
        
        // Compile main statements
        for stmt in main_statements {
            match stmt {
                Statement::Expr(expr) => {
                    asm.push_str("; Expression\n");
                    let expr_code = self.compile_expression(&expr)?;
                    asm.push_str(&expr_code);
                }
                Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                    asm.push_str(&format!("; Variable: {}\n", name));
                    let var_code = self.compile_expression(&value)?;
                    asm.push_str(&var_code);
                }
                Statement::Assign { target, value, span: _ } => {
                    asm.push_str(&format!("; Assignment: {} = \n", target));
                    let expr_code = self.compile_expression(&value)?;
                    asm.push_str(&expr_code);
                }
                Statement::If { condition, then_block, elif_blocks, else_block, span: _ } => {
                    let label_id = self.get_next_label_id();
                    let else_label = format!("if_else_{}", label_id);
                    let end_label = format!("if_end_{}", label_id);
                    
                    // Compile condition
                    asm.push_str("; If condition\n");
                    let cond_code = self.compile_expression(&condition)?;
                    asm.push_str(&cond_code);
                    
                    // Test condition (result in rax)
                    asm.push_str("    test rax, rax\n");
                    asm.push_str(&format!("    jz {}\n", else_label));
                    
                    // Then block
                    asm.push_str("    ; Then block\n");
                    for stmt in &then_block {
                        let stmt_code = self.compile_statement(stmt)?;
                        asm.push_str(&stmt_code);
                    }
                    asm.push_str(&format!("    jmp {}\n", end_label));
                    
                    // Check if we have elif blocks BEFORE the loop
                    let has_elif_blocks = !elif_blocks.is_empty();
                    
                    // Else if blocks - iterate by reference
                    for (i, (elif_cond, elif_body)) in elif_blocks.iter().enumerate() {
                        if i == 0 {
                            asm.push_str(&format!("{}:\n", else_label));
                        } else {
                            asm.push_str(&format!("{}_elif_{}:\n", else_label, i));
                        }
                        
                        asm.push_str("    ; Else if condition\n");
                        let elif_cond_code = self.compile_expression(elif_cond)?;
                        asm.push_str(&elif_cond_code);
                        asm.push_str("    test rax, rax\n");
                        
                        if i < elif_blocks.len() - 1 {
                            asm.push_str(&format!("    jz {}_elif_{}\n", else_label, i + 1));
                        } else {
                            // Last elif block - jump to else or end
                            if else_block.is_some() {
                                asm.push_str(&format!("    jz {}_else\n", else_label));
                            } else {
                                asm.push_str(&format!("    jz {}\n", end_label));
                            }
                        }
                        
                        // Else if body
                        asm.push_str("    ; Else if body\n");
                        for stmt in elif_body {
                            let stmt_code = self.compile_statement(stmt)?;
                            asm.push_str(&stmt_code);
                        }
                        asm.push_str(&format!("    jmp {}\n", end_label));
                    }
                    
                    // Else block
                    if let Some(else_body) = &else_block {
                        if !has_elif_blocks {
                            asm.push_str(&format!("{}:\n", else_label));
                        } else {
                            asm.push_str(&format!("{}_else:\n", else_label));
                        }
                        
                        asm.push_str("    ; Else block\n");
                        for stmt in else_body {
                            let stmt_code = self.compile_statement(&stmt)?;
                            asm.push_str(&stmt_code);
                        }
                    } else if !has_elif_blocks {
                        // No else block and no elif blocks - we still need the else label
                        asm.push_str(&format!("{}:\n", else_label));
                    }
                    
                    asm.push_str(&format!("{}:\n", end_label));
                }
                Statement::While { condition, body, orelse: _, span: _ } => {
                    let label_id = self.get_next_label_id();
                    let while_start = format!("while_start_{}", label_id);
                    let while_end = format!("while_end_{}", label_id);
                    
                    asm.push_str(&format!("{}:\n", while_start));
                    
                    // Compile condition
                    asm.push_str("    ; While condition\n");
                    let cond_code = self.compile_expression(&condition)?;
                    asm.push_str(&cond_code);
                    
                    // Test condition
                    asm.push_str("    test rax, rax\n");
                    asm.push_str(&format!("    jz {}\n", while_end));
                    
                    // Body
                    asm.push_str("    ; While body\n");
                    for stmt in &body {
                        let stmt_code = self.compile_statement(&stmt)?;
                        asm.push_str(&stmt_code);
                    }
                    
                    asm.push_str(&format!("    jmp {}\n", while_start));
                    asm.push_str(&format!("{}:\n", while_end));
                }
                Statement::Return(_) => {
                    return Err("Return statement not allowed outside function".to_string());
                }
                Statement::Pass => {
                    asm.push_str("    ; pass\n");
                }
                Statement::Break => {
                    // In BIOS64, we'll just halt
                    asm.push_str("    ; break\n");
                    asm.push_str("    jmp $\n");
                }
                Statement::Continue => {
                    asm.push_str("    ; continue\n");
                    // This would need to jump to loop start
                }
                _ => {
                    asm.push_str("    ; [Statement type not fully implemented in main]\n");
                }
            }
        }
        
        // Compile functions after main code
        for stmt in functions {
            match stmt {
                Statement::FunctionDef { name, args, body, span: _ } => {
                    asm.push_str(&format!("\n; Function definition: {}\n", name));
                    let func_asm = self.compile_function(&name, &args, &body)?;
                    asm.push_str(&func_asm);
                }
                Statement::HardwareFunctionDef { device, name, args, body, span: _ } => {
                    asm.push_str(&format!("\n; Hardware function: {} for device {}\n", name, device));
                    let hw_func_asm = self.compile_hardware_function_def(&device, &name, &args, &body)?;
                    asm.push_str(&hw_func_asm);
                }
                _ => {}
            }
        }
        
        // SSD: Handle syntax extensions that need assembly calls
        for ext in &self.syntax_extensions {
            if let Some(label) = &ext.assembly_label {
                asm.push_str(&format!("    ; Syntax extension assembly call: {}\n", label));
                asm.push_str(&format!("    call {}\n", label));
            }
        }
        
        asm.push_str("\n    cli\n");
        asm.push_str("    hlt\n");
        asm.push_str("    jmp $\n\n");
        
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
        
        // Add hardware library to the generated code
        asm.push_str("\n; ========== HARDWARE LIBRARY ==========\n");
        asm.push_str(&self.hardware_dsl.generate_hardware_library());
        
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
    
    fn compile_expression(&mut self, expr: &Expr) -> Result<String, String> {
        let mut code = String::new();
        self.compile_expression_helper(expr, &mut code)?;
        Ok(code)
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
    
    fn as_any(&self) -> &dyn Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub struct Linux64Backend {
    string_counter: RefCell<u32>,
    string_literals: RefCell<HashMap<String, String>>,
    symbol_table: RefCell<HashMap<String, VariableInfo>>,
    current_stack_offset: RefCell<i32>,
    external_asm: Vec<String>, // SSD: Store external assembly
    label_counter: RefCell<u32>, // For generating unique labels
}

impl Linux64Backend {
    pub fn new() -> Self {
        Self {
            string_counter: RefCell::new(0),
            string_literals: RefCell::new(HashMap::new()),
            symbol_table: RefCell::new(HashMap::new()),
            current_stack_offset: RefCell::new(0),
            external_asm: Vec::new(),
            label_counter: RefCell::new(0),
        }
    }
    
    #[allow(dead_code)]
    pub fn add_external_asm(&mut self, asm: &str) {
        self.external_asm.push(asm.to_string());
    }
    
    fn allocate_variable_sp_relative(&self, name: &str) -> i32 {
        let mut offset = self.current_stack_offset.borrow_mut();
        
        // Ensure 16-byte alignment
        if (*offset % 16) != 0 {
            *offset += 8;
        }
        
        let current = *offset;
        *offset += 8; // Each variable is 8 bytes
        
        self.symbol_table.borrow_mut().insert(name.to_string(), VariableInfo {
            name: name.to_string(),
            offset: current,
            type_hint: Some("int".to_string()),
        });
        
        current
    }
    
    fn get_variable_offset_sp_relative(&self, name: &str) -> Option<i32> {
        self.symbol_table.borrow().get(name).map(|v| v.offset)
    }
    
    fn ensure_variable_exists_sp_relative(&self, name: &str) -> i32 {
        if let Some(offset) = self.get_variable_offset_sp_relative(name) {
            offset
        } else {
            self.allocate_variable_sp_relative(name)
        }
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
            // NO NEWLINE in the string data! Just null terminator
            data.push_str(&format!("    db '{}', 0\n", content.replace("'", "''")));
        }
        data
    }
    
    fn count_variables(&self, program: &Program) -> usize {
        let mut count = 0;
        let mut seen = std::collections::HashSet::new();
        
        for stmt in &program.body {
            if let Statement::FunctionDef { .. } = stmt {
                continue;
            }
            
            match stmt {
                Statement::VarDecl { name, .. } => {
                    if !seen.contains(name) {
                        seen.insert(name.clone());
                        count += 1;
                    }
                }
                Statement::Assign { target, .. } => {
                    if !seen.contains(target) {
                        seen.insert(target.clone());
                        count += 1;
                    }
                }
                _ => {}
            }
        }
        count
    }
    
    fn get_next_label_id(&self) -> u32 {
        let mut counter = self.label_counter.borrow_mut();
        let id = *counter;
        *counter += 1;
        id
    }
    
    fn generate_helper_function(&self) -> String {
        let mut helpers = String::new();
        
        helpers.push_str("; Simple print string function\n");
        helpers.push_str("print_string:\n");
        helpers.push_str("    ; Input: rdi = string address\n");
        helpers.push_str("    push rax\n");
        helpers.push_str("    push rdi\n");
        helpers.push_str("    push rsi\n");
        helpers.push_str("    push rdx\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Calculate length\n");
        helpers.push_str("    mov rsi, rdi          ; String address\n");
        helpers.push_str("    xor rdx, rdx          ; Length counter\n");
        helpers.push_str(".count_loop:\n");
        helpers.push_str("    cmp byte [rsi + rdx], 0\n");
        helpers.push_str("    je .count_done\n");
        helpers.push_str("    inc rdx\n");
        helpers.push_str("    jmp .count_loop\n");
        helpers.push_str(".count_done:\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Write to stdout\n");
        helpers.push_str("    mov rax, 1           ; sys_write\n");
        helpers.push_str("    mov rdi, 1           ; stdout\n");
        helpers.push_str("    ; rsi already has string address\n");
        helpers.push_str("    ; rdx already has length\n");
        helpers.push_str("    syscall\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Restore registers\n");
        helpers.push_str("    pop rdx\n");
        helpers.push_str("    pop rsi\n");
        helpers.push_str("    pop rdi\n");
        helpers.push_str("    pop rax\n");
        helpers.push_str("    ret\n\n");

        helpers.push_str("; Print decimal number\n");
        helpers.push_str("print_decimal:\n");
        helpers.push_str("    ; Input: rax = integer\n");
        helpers.push_str("    push rbp\n");
        helpers.push_str("    mov rbp, rsp\n");
        helpers.push_str("    sub rsp, 32          ; Buffer space\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Save registers\n");
        helpers.push_str("    push rbx\n");
        helpers.push_str("    push rcx\n");
        helpers.push_str("    push rdx\n");
        helpers.push_str("    push rsi\n");
        helpers.push_str("    push rdi\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Save the number\n");
        helpers.push_str("    mov [rbp - 8], rax   ; Save at [rbp-8]\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Point to buffer end\n");
        helpers.push_str("    lea rdi, [rsp + 31]  ; Last byte of buffer\n");
        helpers.push_str("    mov byte [rdi], 0    ; Null terminator\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Handle negative numbers\n");
        helpers.push_str("    mov rax, [rbp - 8]\n");
        helpers.push_str("    test rax, rax\n");
        helpers.push_str("    jns .positive\n");
        helpers.push_str("    neg rax\n");
        helpers.push_str("    \n");
        helpers.push_str(".positive:\n");
        helpers.push_str("    mov rbx, 10\n");
        helpers.push_str("    \n");
        helpers.push_str(".convert_loop:\n");
        helpers.push_str("    xor rdx, rdx\n");
        helpers.push_str("    div rbx              ; rax = quotient, rdx = remainder\n");
        helpers.push_str("    add dl, '0'\n");
        helpers.push_str("    dec rdi\n");
        helpers.push_str("    mov [rdi], dl\n");
        helpers.push_str("    test rax, rax\n");
        helpers.push_str("    jnz .convert_loop\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Add minus sign if needed\n");
        helpers.push_str("    mov rax, [rbp - 8]\n");
        helpers.push_str("    test rax, rax\n");
        helpers.push_str("    jns .print_it\n");
        helpers.push_str("    dec rdi\n");
        helpers.push_str("    mov byte [rdi], '-'\n");
        helpers.push_str("    \n");
        helpers.push_str(".print_it:\n");
        helpers.push_str("    ; Calculate length\n");
        helpers.push_str("    lea rsi, [rsp + 32]  ; End of buffer + 1\n");
        helpers.push_str("    sub rsi, rdi         ; rsi = length\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Print the number\n");
        helpers.push_str("    mov rax, 1           ; sys_write\n");
        helpers.push_str("    mov rdx, rsi         ; length\n");
        helpers.push_str("    mov rsi, rdi         ; string\n");
        helpers.push_str("    mov rdi, 1           ; stdout\n");
        helpers.push_str("    syscall\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Print newline\n");
        helpers.push_str("    mov rax, 1\n");
        helpers.push_str("    mov rdi, 1\n");
        helpers.push_str("    lea rsi, [newline]\n");
        helpers.push_str("    mov rdx, 1\n");
        helpers.push_str("    syscall\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Restore registers\n");
        helpers.push_str("    pop rdi\n");
        helpers.push_str("    pop rsi\n");
        helpers.push_str("    pop rdx\n");
        helpers.push_str("    pop rcx\n");
        helpers.push_str("    pop rbx\n");
        helpers.push_str("    \n");
        helpers.push_str("    mov rsp, rbp\n");
        helpers.push_str("    pop rbp\n");
        helpers.push_str("    ret\n\n");
        
        // print_newline - simple version
        helpers.push_str("; Print newline\n");
        helpers.push_str("print_newline:\n");
        helpers.push_str("    push rax\n");
        helpers.push_str("    push rdi\n");
        helpers.push_str("    push rsi\n");
        helpers.push_str("    push rdx\n");
        helpers.push_str("    \n");
        helpers.push_str("    mov rax, 1\n");
        helpers.push_str("    mov rdi, 1\n");
        helpers.push_str("    lea rsi, [newline]\n");
        helpers.push_str("    mov rdx, 1\n");
        helpers.push_str("    syscall\n");
        helpers.push_str("    \n");
        helpers.push_str("    pop rdx\n");
        helpers.push_str("    pop rsi\n");
        helpers.push_str("    pop rdi\n");
        helpers.push_str("    pop rax\n");
        helpers.push_str("    ret\n");
        
        helpers
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
        
        // Separate functions from main code
        let mut functions = Vec::new();
        let mut main_statements = Vec::new();
        
        for stmt in &program.body {
            match stmt {
                Statement::FunctionDef { .. } => {
                    functions.push(stmt.clone());
                }
                _ => {
                    main_statements.push(stmt.clone());
                }
            }
        }
        
        // Entry point
        asm.push_str("_start:\n");
        asm.push_str("    ; Setup stack frame\n");
        asm.push_str("    mov rbp, rsp\n");
        asm.push_str("    and rsp, -16        ; Align stack to 16 bytes\n");
        asm.push_str("    sub rsp, 64         ; Space for variables and alignment\n");
        asm.push_str("    \n");
        asm.push_str("    ; Call main\n");
        asm.push_str("    call main\n");
        asm.push_str("    \n");
        asm.push_str("    ; Exit with return code\n");
        asm.push_str("    mov rdi, rax        ; Exit code from main\n");
        asm.push_str("    mov rax, 60         ; sys_exit\n");
        asm.push_str("    syscall\n\n");
        
        // Main function
        asm.push_str("main:\n");
        asm.push_str("    push rbp\n");
        asm.push_str("    mov rbp, rsp\n");
        asm.push_str("    and rsp, -16        ; Align stack to 16 bytes\n");
        
        // Allocate space for variables
        let var_count = self.count_variables(program);
        if var_count > 0 {
            let total_space = ((var_count * 8) + 15) & !15; // Round up to 16-byte multiple
            asm.push_str(&format!("    sub rsp, {}          ; Space for {} variables\n", 
                total_space, var_count));
        }
        
        // Reset variable tracking
        {
            let mut offset = self.current_stack_offset.borrow_mut();
            *offset = 16; // Start after saved rbp
            self.symbol_table.borrow_mut().clear();
        }
        
        // Compile main statements
        for stmt in main_statements {
            match stmt {
                Statement::Expr(expr) => {
                    let expr_code = self.compile_expression(&expr)?;
                    asm.push_str(&expr_code);
                }
                Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                    asm.push_str(&format!("    ; Variable declaration: {}\n", name));
                    
                    // Allocate space for variable
                    let offset = self.allocate_variable_sp_relative(&name);
                    
                    // Compile the value (result in rax)
                    let value_code = self.compile_expression(&value)?;
                    asm.push_str(&value_code);
                    
                    // Store value at [rsp + offset]
                    asm.push_str(&format!("    mov [rsp + {}], rax\n", offset));
                }
                Statement::Assign { target, value, span: _ } => {
                    asm.push_str(&format!("    ; Assignment: {} = ...\n", target));
                    
                    // Get or allocate variable
                    let offset = self.ensure_variable_exists_sp_relative(&target);
                    
                    // Compile the value
                    let value_code = self.compile_expression(&value)?;
                    asm.push_str(&value_code);
                    
                    // Store value
                    asm.push_str(&format!("    mov [rsp + {}], rax\n", offset));
                }
                Statement::If { condition, then_block, elif_blocks, else_block, span: _ } => {
                    let label_id = self.get_next_label_id();
                    let else_label = format!("if_else_{}", label_id);
                    let end_label = format!("if_end_{}", label_id);
                    
                    // Compile condition
                    asm.push_str("    ; If condition\n");
                    let cond_code = self.compile_expression(&condition)?;
                    asm.push_str(&cond_code);
                    
                    // Test condition
                    asm.push_str("    test rax, rax\n");
                    asm.push_str(&format!("    jz {}\n", else_label));
                    
                    // Then block
                    asm.push_str("    ; Then block\n");
                    for stmt in &then_block {
                        match stmt {
                            Statement::Expr(expr) => {
                                let expr_code = self.compile_expression(&expr)?;
                                asm.push_str(&expr_code);
                            }
                            Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                                let offset = self.allocate_variable_sp_relative(&name);
                                let value_code = self.compile_expression(&value)?;
                                asm.push_str(&value_code);
                                asm.push_str(&format!("    mov [rsp + {}], rax\n", offset));
                            }
                            _ => {}
                        }
                    }
                    asm.push_str(&format!("    jmp {}\n", end_label));
                    
                    // Else if blocks - iterate by reference
                    for (elif_cond, elif_body) in elif_blocks.iter() {
                        asm.push_str(&format!("{}:\n", else_label));
                        let elif_cond_code = self.compile_expression(elif_cond)?;
                        asm.push_str(&elif_cond_code);
                        asm.push_str("    test rax, rax\n");
                        asm.push_str(&format!("    jz {}_elif\n", else_label));
                        
                        for stmt in elif_body {
                            match stmt {
                                Statement::Expr(expr) => {
                                    let expr_code = self.compile_expression(&expr)?;
                                    asm.push_str(&expr_code);
                                }
                                _ => {}
                            }
                        }
                        asm.push_str(&format!("    jmp {}\n", end_label));
                        asm.push_str(&format!("{}_elif:\n", else_label));
                    }
                    
                    // Else block
                    if let Some(else_body) = &else_block {
                        if elif_blocks.is_empty() {
                            asm.push_str(&format!("{}:\n", else_label));
                        }
                        for stmt in else_body {
                            match stmt {
                                Statement::Expr(expr) => {
                                    let expr_code = self.compile_expression(&expr)?;
                                    asm.push_str(&expr_code);
                                }
                                _ => {}
                            }
                        }
                    } else {
                        if elif_blocks.is_empty() {
                            asm.push_str(&format!("{}:\n", else_label));
                        }
                    }
                    
                    asm.push_str(&format!("{}:\n", end_label));
                }
                _ => {
                    asm.push_str("    ; [Other statement type in main]\n");
                }
            }
        }
        
        // Return 0 from main
        asm.push_str("    ; Return from main\n");
        asm.push_str("    mov rax, 0          ; Return 0\n");
        asm.push_str("    mov rsp, rbp\n");
        asm.push_str("    pop rbp\n");
        asm.push_str("    ret\n\n");
        
        // Compile functions
        for stmt in functions {
            if let Statement::FunctionDef { name, args, body, span: _ } = stmt {
                asm.push_str(&format!("; Function: {}\n", name));
                
                // Create function context
                let mut func_ctx = FunctionContext::new(name.clone(), args.clone());
                
                // Function prologue
                asm.push_str(&format!("{}:\n", name));
                asm.push_str("    push rbp\n");
                asm.push_str("    mov rbp, rsp\n");
                
                // Calculate stack space needed
                let local_count = body.iter()
                    .filter(|stmt| matches!(stmt, Statement::VarDecl { .. }))
                    .count();
                
                if local_count > 0 {
                    let stack_size = ((local_count * 8) + 15) & !15;
                    asm.push_str(&format!("    sub rsp, {}   ; Local variables\n", stack_size));
                    func_ctx.stack_size = stack_size as i32;
                }
                
                // Handle arguments (System V AMD64 calling convention)
                for (i, arg) in args.iter().enumerate() {
                    match i {
                        0 => {
                            asm.push_str(&format!("    ; {} -> rdi\n", arg));
                            asm.push_str("    mov [rbp - 8], rdi\n");
                            func_ctx.add_local(arg.clone(), 8, None);
                        }
                        1 => {
                            asm.push_str(&format!("    ; {} -> rsi\n", arg));
                            asm.push_str("    mov [rbp - 16], rsi\n");
                            func_ctx.add_local(arg.clone(), 16, None);
                        }
                        2 => {
                            asm.push_str(&format!("    ; {} -> rdx\n", arg));
                            asm.push_str("    mov [rbp - 24], rdx\n");
                            func_ctx.add_local(arg.clone(), 24, None);
                        }
                        3 => {
                            asm.push_str(&format!("    ; {} -> rcx\n", arg));
                            asm.push_str("    mov [rbp - 32], rcx\n");
                            func_ctx.add_local(arg.clone(), 32, None);
                        }
                        4 => {
                            asm.push_str(&format!("    ; {} -> r8\n", arg));
                            asm.push_str("    mov [rbp - 40], r8\n");
                            func_ctx.add_local(arg.clone(), 40, None);
                        }
                        5 => {
                            asm.push_str(&format!("    ; {} -> r9\n", arg));
                            asm.push_str("    mov [rbp - 48], r9\n");
                            func_ctx.add_local(arg.clone(), 48, None);
                        }
                        _ => {
                            // Stack arguments
                            let offset = 16 + (i - 6) * 8;
                            let offset_i32 = offset as i32; // FIXED: Convert to i32
                            asm.push_str(&format!("    ; {} -> [rbp + {}]\n", arg, offset));
                            func_ctx.add_local(arg.clone(), -offset_i32, None); // Negative for positive offsets
                        }
                    }
                }
                
                // Compile function body
                for stmt in &body {
                    match stmt {
                        Statement::Expr(expr) => {
                            let expr_code = self.compile_expression(&expr)?;
                            asm.push_str(&expr_code);
                        }
                        Statement::Return(expr) => {
                            if let Some(expr) = expr {
                                let expr_code = self.compile_expression(&expr)?;
                                asm.push_str(&expr_code);
                                // Result is in rax
                            } else {
                                asm.push_str("    xor rax, rax\n");
                            }
                            
                            // Function epilogue
                            if func_ctx.stack_size > 0 {
                                asm.push_str(&format!("    add rsp, {}\n", func_ctx.stack_size));
                            }
                            asm.push_str("    mov rsp, rbp\n");
                            asm.push_str("    pop rbp\n");
                            asm.push_str("    ret\n");
                            func_ctx.has_returned = true;
                        }
                        Statement::VarDecl { name: var_name, value, type_hint: _, span: _ } => {
                            let offset = func_ctx.allocate_local();
                            func_ctx.add_local(var_name.clone(), offset, None);
                            
                            asm.push_str(&format!("    ; Local variable: {}\n", var_name));
                            let expr_code = self.compile_expression(&value)?;
                            asm.push_str(&expr_code);
                            asm.push_str(&format!("    mov [rbp - {}], rax\n", offset));
                        }
                        _ => {
                            asm.push_str(&format!("    ; [Statement in function {}: {:?}]\n", name, stmt));
                        }
                    }
                }
                
                // Default return if no explicit return
                if !func_ctx.has_returned {
                    asm.push_str("    ; Default return\n");
                    asm.push_str("    xor rax, rax\n");
                    if func_ctx.stack_size > 0 {
                        asm.push_str(&format!("    add rsp, {}\n", func_ctx.stack_size));
                    }
                    asm.push_str("    mov rsp, rbp\n");
                    asm.push_str("    pop rbp\n");
                    asm.push_str("    ret\n");
                }
                
                asm.push_str("\n");
            }
        }
        
        // Helper functions
        asm.push_str(&self.generate_helper_function());
        
        // Data section
        asm.push_str("    section .data\n");
        asm.push_str("newline:\n");
        asm.push_str("    db 10, 0\n\n");
        
        // String literals
        asm.push_str("; String literals\n");
        asm.push_str(&self.generate_string_data());
        
        // SSD: Inject external assembly
        asm = self.inject_external_asm(&asm, &self.external_asm);
        
        Ok(asm)
    }
    
    fn compile_expression(&mut self, expr: &Expr) -> Result<String, String> {
        match expr {
            Expr::Number(n, _) => {
                // Load number into rax
                Ok(format!("    ; Number: {}\n    mov rax, {}\n", n, n))
            }
            Expr::String(s, _) => {
                let label = self.get_string_label(s);
                // Load string address into rax
                Ok(format!("    ; String: '{}'\n    lea rax, [{}]\n", s, label))
            }
            Expr::Call { func, args, kwargs: _, span: _ } if func == "print" => {
                let mut code = String::new();
                
                // Process each argument
                for arg in args {
                    match arg {
                        Expr::String(s, _) => {
                            let label = self.get_string_label(s);
                            code.push_str(&format!("    ; String: '{}'\n", s));
                            code.push_str(&format!("    lea rdi, [{}]\n", label));
                            code.push_str("    call print_string\n");
                        }
                        Expr::Number(n, _) => {
                            code.push_str(&format!("    ; Number: {}\n", n));
                            code.push_str(&format!("    mov rax, {}\n", n));
                            code.push_str("    call print_decimal\n");
                        }
                        Expr::BinOp { left, op, right, span: _ } => {
                            // Handle binary operations
                            code.push_str("    ; Binary operation\n");
                            
                            // Helper function to get value
                            fn get_value_code(backend: &Linux64Backend, expr: &Expr) -> Result<String, String> {
                                match expr {
                                    Expr::Number(n, _) => Ok(format!("    mov rax, {}\n", n)),
                                    Expr::Var(name, _) => {
                                        if let Some(offset) = backend.get_variable_offset_sp_relative(name) {
                                            Ok(format!("    mov rax, [rsp + {}]\n", offset))
                                        } else {
                                            Err(format!("Undefined variable: {}", name))
                                        }
                                    }
                                    _ => Err("Unsupported expression type".to_string()),
                                }
                            }
                            
                            // Left operand
                            code.push_str(&get_value_code(self, left)?);
                            code.push_str("    push rax\n");
                            
                            // Right operand  
                            code.push_str(&get_value_code(self, right)?);
                            code.push_str("    mov rbx, rax\n");
                            code.push_str("    pop rax\n");
                            
                            // Perform operation
                            match op {
                                Op::Add => code.push_str("    add rax, rbx\n"),
                                Op::Sub => code.push_str("    sub rax, rbx\n"),
                                Op::Mul => code.push_str("    imul rax, rbx\n"),
                                Op::Div => {
                                    code.push_str("    xor rdx, rdx\n");
                                    code.push_str("    div rbx\n");
                                }
                                Op::Mod => {
                                    code.push_str("    xor rdx, rdx\n");
                                    code.push_str("    div rbx\n");
                                    code.push_str("    mov rax, rdx\n");
                                }
                                _ => return Err(format!("Unsupported operator: {:?}", op)),
                            }
                            
                            // Print the result
                            code.push_str("    call print_decimal\n");
                        }
                        Expr::Var(name, _) => {
                            if let Some(offset) = self.get_variable_offset_sp_relative(name) {
                                code.push_str(&format!("    ; Variable: {}\n", name));
                                code.push_str(&format!("    mov rax, [rsp + {}]\n", offset));
                                code.push_str("    call print_decimal\n");
                            } else {
                                return Err(format!("Undefined variable: {}", name));
                            }
                        }
                        _ => {
                            return Err(format!("Unsupported argument type in print: {:?}", arg));
                        }
                    }
                }
                
                // Add newline at the end of print statement
                code.push_str("    call print_newline\n");
                
                Ok(code)
            }
            Expr::Var(name, _) => {
                if let Some(offset) = self.get_variable_offset_sp_relative(name) {
                    let mut code = format!("    ; Variable: {}\n    mov rax, [rsp + {}]\n", name, offset);
                    // For standalone variable expression, print it
                    code.push_str("    call print_decimal\n");
                    Ok(code)
                } else {
                    Err(format!("Undefined variable: {}", name))
                }
            }
            Expr::BinOp { left, op, right, span: _ } => {
                // Standalone binary operation - compute and print
                let mut code = String::new();
                code.push_str("    ; Binary operation\n");
                
                // Same helper as above
                fn get_value_code(backend: &Linux64Backend, expr: &Expr) -> Result<String, String> {
                    match expr {
                        Expr::Number(n, _) => Ok(format!("    mov rax, {}\n", n)),
                        Expr::Var(name, _) => {
                            if let Some(offset) = backend.get_variable_offset_sp_relative(name) {
                                Ok(format!("    mov rax, [rsp + {}]\n", offset))
                            } else {
                                Err(format!("Undefined variable: {}", name))
                            }
                        }
                        _ => Err("Unsupported expression type".to_string()),
                    }
                }
                
                // Left operand
                code.push_str(&get_value_code(self, left)?);
                code.push_str("    push rax\n");
                
                // Right operand
                code.push_str(&get_value_code(self, right)?);
                code.push_str("    mov rbx, rax\n");
                code.push_str("    pop rax\n");
                
                // Operation
                match op {
                    Op::Add => code.push_str("    add rax, rbx\n"),
                    Op::Sub => code.push_str("    sub rax, rbx\n"),
                    Op::Mul => code.push_str("    imul rax, rbx\n"),
                    Op::Div => {
                        code.push_str("    xor rdx, rdx\n");
                        code.push_str("    div rbx\n");
                    }
                    Op::Mod => {
                        code.push_str("    xor rdx, rdx\n");
                        code.push_str("    div rbx\n");
                        code.push_str("    mov rax, rdx\n");
                    }
                    _ => return Err(format!("Unsupported operator: {:?}", op)),
                }
                
                // Print the result
                code.push_str("    call print_decimal\n");
                Ok(code)
            }
            _ => Err(format!("Unsupported expression: {:?}", expr)),
        }
    }
    
    fn function_prologue(&self, func: &BackendFunction) -> String {
        let mut prologue = String::new();
        prologue.push_str(&format!("{}:\n", func.name));
        prologue.push_str("    push rbp\n");
        prologue.push_str("    mov rbp, rsp\n");
        
        // Allocate space for locals
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
    
    fn as_any(&self) -> &dyn Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}


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
    
    fn allocate_variable(&self, name: &str) -> i32 {
        let mut offset = self.current_stack_offset.borrow_mut();
        let current = *offset;
        *offset += 8; // Each variable takes 8 bytes (64-bit pointer)
        
        self.symbol_table.borrow_mut().insert(name.to_string(), VariableInfo {
            name: name.to_string(),
            offset: current,
            type_hint: Some("int".to_string()),
        });
        
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
        
        // Separate functions from main code
        let mut functions = Vec::new();
        let mut main_statements = Vec::new();
        
        for stmt in &program.body {
            match stmt {
                Statement::FunctionDef { .. } => {
                    functions.push(stmt.clone());
                }
                _ => {
                    main_statements.push(stmt.clone());
                }
            }
        }
        
        // Main function
        asm.push_str("main:\n");
        asm.push_str("    push rbp\n");
        asm.push_str("    mov rbp, rsp\n");
        
        // Allocate space for variables (calculate based on number of variables)
        let var_count = self.count_variables(program);
        let stack_size = 32 + (var_count * 8); // Shadow space + variables
        asm.push_str(&format!("    sub rsp, {}          ; Allocate shadow space and variable space\n", stack_size));
        asm.push_str("    and rsp, -16         ; Align stack to 16 bytes\n\n");
        
        // Reset variable tracking
        {
            let mut offset = self.current_stack_offset.borrow_mut();
            *offset = 8; // Reset to start
            self.symbol_table.borrow_mut().clear();
        }
        
        // Compile main statements
        for stmt in main_statements {
            match stmt {
                Statement::Expr(expr) => {
                    asm.push_str(&self.compile_expression(&expr)?);
                }
                Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                    // Allocate space for variable
                    let offset = self.allocate_variable(&name);
                    
                    // Compile the value
                    asm.push_str(&format!("    ; Variable declaration: {}\n", name));
                    asm.push_str(&self.compile_expression(&value)?);
                    
                    // Store the value at [rbp - offset]
                    asm.push_str(&format!("    mov [rbp - {}], rax\n", offset));
                }
                Statement::Assign { target, value, span: _ } => {
                    // Ensure variable exists (allocate if not)
                    let offset = self.ensure_variable_exists(&target);
                    
                    // Compile the value
                    asm.push_str(&format!("    ; Assignment: {} = ...\n", target));
                    asm.push_str(&self.compile_expression(&value)?);
                    
                    // Store the value at [rbp - offset]
                    asm.push_str(&format!("    mov [rbp - {}], rax\n", offset));
                }
                _ => {
                    asm.push_str("    ; [Unsupported statement in main]\n");
                }
            }
        }
        
        // Windows exit
        asm.push_str("\n    ; Exit\n");
        asm.push_str("    xor ecx, ecx          ; exit code 0\n");
        asm.push_str("    call ExitProcess\n");
        
        // Compile functions
        for stmt in functions {
            if let Statement::FunctionDef { name, args, body, span: _ } = stmt {
                asm.push_str(&format!("\n; Function: {}\n", name));
                
                // Create function context
                let mut func_ctx = FunctionContext::new(name.clone(), args.clone());
                
                // Function prologue
                asm.push_str(&format!("{}:\n", name));
                asm.push_str("    push rbp\n");
                asm.push_str("    mov rbp, rsp\n");
                
                // Windows x64 requires 32 bytes of shadow space
                asm.push_str("    sub rsp, 32          ; Shadow space\n");
                
                // Calculate local variable space
                let local_count = body.iter()
                    .filter(|stmt| matches!(stmt, Statement::VarDecl { .. }))
                    .count();
                
                if local_count > 0 {
                    let local_space = ((local_count * 8) + 15) & !15;
                    asm.push_str(&format!("    sub rsp, {}   ; Local variables\n", local_space));
                    func_ctx.stack_size = local_space as i32;
                }
                
                // Handle arguments (Windows x64 calling convention)
                for (i, arg) in args.iter().enumerate() {
                    match i {
                        0 => {
                            asm.push_str(&format!("    ; {} -> rcx\n", arg));
                            asm.push_str("    mov [rbp - 8], rcx\n"); // Store in shadow space
                            func_ctx.add_local(arg.clone(), 8, None);
                        }
                        1 => {
                            asm.push_str(&format!("    ; {} -> rdx\n", arg));
                            asm.push_str("    mov [rbp - 16], rdx\n");
                            func_ctx.add_local(arg.clone(), 16, None);
                        }
                        2 => {
                            asm.push_str(&format!("    ; {} -> r8\n", arg));
                            asm.push_str("    mov [rbp - 24], r8\n");
                            func_ctx.add_local(arg.clone(), 24, None);
                        }
                        3 => {
                            asm.push_str(&format!("    ; {} -> r9\n", arg));
                            asm.push_str("    mov [rbp - 32], r9\n");
                            func_ctx.add_local(arg.clone(), 32, None);
                        }
                        _ => {
                            // Stack arguments (after shadow space and return address)
                            let offset = 48 + (i - 4) * 8; // 32 shadow + 8 return + 8 saved rbp
                            let offset_i32 = offset as i32; // FIXED: Convert to i32
                            asm.push_str(&format!("    ; {} -> [rbp + {}]\n", arg, offset));
                            func_ctx.add_local(arg.clone(), -offset_i32, None);
                        }
                    }
                }
                
                // Compile function body
                for stmt in &body {
                    match stmt {
                        Statement::Expr(expr) => {
                            let expr_code = self.compile_expression(&expr)?;
                            asm.push_str(&expr_code);
                        }
                        Statement::Return(expr) => {
                            if let Some(expr) = expr {
                                let expr_code = self.compile_expression(&expr)?;
                                asm.push_str(&expr_code);
                                // Result is in rax
                            } else {
                                asm.push_str("    xor rax, rax\n");
                            }
                            
                            // Function epilogue for Windows
                            if func_ctx.stack_size > 0 {
                                asm.push_str(&format!("    add rsp, {}\n", func_ctx.stack_size));
                            }
                            asm.push_str("    add rsp, 32          ; Clean up shadow space\n");
                            asm.push_str("    mov rsp, rbp\n");
                            asm.push_str("    pop rbp\n");
                            asm.push_str("    ret\n");
                            func_ctx.has_returned = true;
                        }
                        Statement::VarDecl { name: var_name, value, type_hint: _, span: _ } => {
                            let offset = func_ctx.allocate_local() + 32; // Account for shadow space
                            func_ctx.add_local(var_name.clone(), offset, None);
                            
                            asm.push_str(&format!("    ; Local variable: {}\n", var_name));
                            let expr_code = self.compile_expression(&value)?;
                            asm.push_str(&expr_code);
                            asm.push_str(&format!("    mov [rbp - {}], rax\n", offset));
                        }
                        _ => {
                            asm.push_str(&format!("    ; [Statement in function {}: {:?}]\n", name, stmt));
                        }
                    }
                }
                
                // Default return if no explicit return
                if !func_ctx.has_returned {
                    asm.push_str("    ; Default return\n");
                    asm.push_str("    xor rax, rax\n");
                    if func_ctx.stack_size > 0 {
                        asm.push_str(&format!("    add rsp, {}\n", func_ctx.stack_size));
                    }
                    asm.push_str("    add rsp, 32          ; Clean up shadow space\n");
                    asm.push_str("    mov rsp, rbp\n");
                    asm.push_str("    pop rbp\n");
                    asm.push_str("    ret\n");
                }
            }
        }
        
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
        asm.push_str("    .positive:\n");
        asm.push_str("    mov rbx, 10          ; Base 10\n");
        asm.push_str("    \n");
        asm.push_str("    .convert_loop:\n");
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
    
    fn compile_expression(&mut self, expr: &Expr) -> Result<String, String> {
        let mut code = String::new();
        
        match expr {
            Expr::Number(n, _) => {
                code.push_str(&format!("    ; Number: {}\n", n));
                code.push_str(&format!("    mov rax, {}\n", n));
                
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
                        Expr::Number(_, _) => {
                            // Compile the argument as a number
                            let compiled = self.compile_expression(arg)?;
                            code.push_str(&compiled);
                            
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
                            let compiled = self.compile_expression(arg)?;
                            code.push_str(&compiled);
                        }
                        Expr::Var(_, _) => {
                            let compiled = self.compile_expression(arg)?;
                            code.push_str(&compiled);
                        }
                        Expr::BinOp { .. } => {
                            // Handle binary operations by compiling the whole expression
                            let compiled = self.compile_expression(arg)?;
                            code.push_str(&compiled);
                            
                            // After compilation, rax contains the result
                            code.push_str("    mov rdx, rax          ; Second arg: integer value\n");
                            code.push_str("    lea rcx, [printf_format_int] ; First arg: format string for integer\n");
                            code.push_str("    sub rsp, 32          ; Allocate shadow space\n");
                            code.push_str("    xor rax, rax          ; No floating point args\n");
                            code.push_str("    call printf\n");
                            code.push_str("    add rsp, 32          ; Clean up shadow space\n");
                            
                            // Automatically add newline after print
                            code.push_str("    call print_newline\n");
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
                    Op::Add => {
                        code.push_str("    add rax, rbx\n");
                    }
                    Op::Sub => {
                        code.push_str("    sub rax, rbx\n");
                    }
                    Op::Mul => {
                        code.push_str("    imul rax, rbx\n");
                    }
                    Op::Div => {
                        code.push_str("    xor rdx, rdx\n");
                        code.push_str("    idiv rbx\n");
                    }
                    Op::Mod => {
                        code.push_str("    xor rdx, rdx\n");
                        code.push_str("    div rbx\n");
                        code.push_str("    mov rax, rdx\n"); // Remainder
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
    
    fn as_any(&self) -> &dyn Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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

impl HashCode for String {
    fn hash_code(&self) -> u64 {
        self.as_str().hash_code()
    }
}