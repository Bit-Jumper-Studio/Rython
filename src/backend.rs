use crate::parser::{Program, Statement, Expr, Op, CompareOp};
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

// Struct to track variables
#[derive(Debug, Clone)]
pub struct VariableInfo {
    #[allow(dead_code)]
    pub name: String,
    pub offset: i32,  // Negative offset from rbp
    #[allow(dead_code)]
    pub type_hint: Option<String>,
}

// Symbol for bare metal memory management
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub address: u64,
    pub size: usize,
    pub type_: String,
    pub value: Option<String>,
    pub is_initialized: bool,
    pub is_const: bool,
}

// Memory manager for bare metal
#[derive(Debug, Clone)]
pub struct BareMetalMemoryManager {
    pub heap_start: u64,
    pub heap_current: u64,
    pub heap_end: u64,
    pub stack_start: u64,
    pub stack_current: u64,
    pub stack_end: u64,
    pub data_start: u64,
    pub data_current: u64,
    pub data_end: u64,
    pub symbol_table: HashMap<String, Symbol>,
    pub string_literals: HashMap<String, (u64, String)>,
}

impl BareMetalMemoryManager {
    pub fn new() -> Self {
        Self {
            heap_start: 0x100000,
            heap_current: 0x100000,
            heap_end: 0x4000000,
            stack_start: 0x7C00,
            stack_current: 0x7C00,
            stack_end: 0x8000,
            data_start: 0x200000,
            data_current: 0x200000,
            data_end: 0x400000,
            symbol_table: HashMap::new(),
            string_literals: HashMap::new(),
        }
    }
    
    pub fn allocate_data(&mut self, size: usize, align: usize) -> Result<u64, String> {
        if align > 0 && (self.data_current % align as u64 != 0) {
            self.data_current = (self.data_current + align as u64 - 1) & !(align as u64 - 1);
        }
        
        let address = self.data_current;
        self.data_current += size as u64;
        
        if self.data_current > self.data_end {
            return Err(format!("Data section overflow!"));
        }
        
        Ok(address)
    }
    
    pub fn register_symbol(&mut self, name: String, address: u64, size: usize, 
                      type_: &str, value: Option<String>) -> &Symbol {
    let symbol = Symbol {
        name: name.clone(),
        address,
        size,
        type_: type_.to_string(),
        value: value.clone(),
        is_initialized: value.is_some(),
        is_const: false,
    };
    
    self.symbol_table.insert(name.clone(), symbol);
    self.symbol_table.get(&name).unwrap()
}

    pub fn register_string_literal(&mut self, content: &str) -> (u64, String) {
        if let Some((addr, label)) = self.string_literals.get(content) {
            return (*addr, label.clone());
        }
        
        let size = content.len() + 1;
        let address = self.allocate_data(size, 1).unwrap();
        let label = format!("str_{:x}", address);
        
        self.string_literals.insert(content.to_string(), (address, label.clone()));
        (address, label)
    }
    
    pub fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        self.symbol_table.get(name)
    }
    
    pub fn get_string_address(&self, content: &str) -> Option<u64> {
        self.string_literals.get(content).map(|(addr, _)| *addr)
    }
}

// Function context
#[derive(Debug, Clone)]
struct FunctionContext {
    name: String,
    args: Vec<String>,
    locals: HashMap<String, VariableInfo>,
    symbol_table: HashMap<String, Symbol>,
    has_returned: bool,
    stack_size: i32,
    memory_manager: std::rc::Rc<std::cell::RefCell<BareMetalMemoryManager>>,
}

impl FunctionContext {
    fn new(name: String, args: Vec<String>, memory_manager: std::rc::Rc<std::cell::RefCell<BareMetalMemoryManager>>) -> Self {
        Self {
            name,
            args,
            locals: HashMap::new(),
            symbol_table: HashMap::new(),
            has_returned: false,
            stack_size: 0,
            memory_manager,
        }
    }
    
    fn allocate_local_variable(&mut self, name: &str, type_hint: Option<String>) -> Result<i32, String> {
        let offset = self.allocate_local();
        
        // Clone type_hint before using it
        let type_clone = type_hint.clone().unwrap_or("int".to_string());
        
        let symbol = Symbol {
            name: name.to_string(),
            address: offset as u64,
            size: 8,
            type_: type_clone,
            value: None,
            is_initialized: false,
            is_const: false,
        };
        
        self.symbol_table.insert(name.to_string(), symbol);
        self.add_local(name.to_string(), offset, type_hint);
        
        Ok(offset)
    }
    
    fn add_local(&mut self, name: String, offset: i32, type_hint: Option<String>) {
        let var_name = name.clone();
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
    
    fn get_symbol(&self, name: &str) -> Option<Symbol> {
        if let Some(symbol) = self.symbol_table.get(name) {
            Some(symbol.clone())
        } else {
            let mm = self.memory_manager.borrow();
            mm.get_symbol(name).cloned()
        }
    }
}

/// Trait for earthang language extension modules
pub trait EarthngModule {
    /// Name of the module
    fn name(&self) -> &str;
    
    /// Description of the module
    fn description(&self) -> &str;
    
    /// Functions provided by this module
    fn functions(&self) -> Vec<&str>;
    
    /// Compile a function call to assembly
    fn compile_function(
        &self, 
        func: &str, 
        args: &[crate::parser::Expr], 
        target: &Target,
        emitter: &mut dyn AssemblyEmitter
    ) -> Result<String, String>;
    
    /// Initialize the module with required capabilities
    fn init(&mut self, capabilities: &[Capability]);
    
    /// Check if module supports a specific function
    fn supports_function(&self, func: &str) -> bool {
        self.functions().contains(&func)
    }
}

/// Trait for emitting assembly code from modules
pub trait AssemblyEmitter {
    fn emit_call(&mut self, func: &str, args: &[String]) -> String;
    fn emit_reg_move(&mut self, src: &str, dst: &str) -> String;
    fn emit_label(&mut self, label: &str) -> String;
    fn emit_comment(&mut self, comment: &str) -> String;
}

/// Basic assembly emitter implementation
pub struct BasicAssemblyEmitter {
    target: Target,
}

impl BasicAssemblyEmitter {
    pub fn new(target: Target) -> Self {
        Self { target }
    }
}

impl AssemblyEmitter for BasicAssemblyEmitter {
    fn emit_call(&mut self, func: &str, args: &[String]) -> String {
        match self.target {
            Target::Linux64 => {
                let mut asm = String::new();
                for (i, arg) in args.iter().enumerate() {
                    match i {
                        0 => asm.push_str(&format!("    mov rdi, {}\n", arg)),
                        1 => asm.push_str(&format!("    mov rsi, {}\n", arg)),
                        2 => asm.push_str(&format!("    mov rdx, {}\n", arg)),
                        3 => asm.push_str(&format!("    mov rcx, {}\n", arg)),
                        4 => asm.push_str(&format!("    mov r8, {}\n", arg)),
                        5 => asm.push_str(&format!("    mov r9, {}\n", arg)),
                        _ => asm.push_str(&format!("    push {}\n", arg)),
                    }
                }
                if args.len() > 6 {
                    asm.push_str(&format!("    ; {} stack args passed\n", args.len() - 6));
                }
                asm.push_str(&format!("    call {}\n", func));
                asm
            }
            Target::Bios64 => {
                let mut asm = String::new();
                for (i, arg) in args.iter().enumerate() {
                    match i {
                        0 => asm.push_str(&format!("    mov rax, {}\n", arg)),
                        1 => asm.push_str(&format!("    mov rbx, {}\n", arg)),
                        2 => asm.push_str(&format!("    mov rcx, {}\n", arg)),
                        3 => asm.push_str(&format!("    mov rdx, {}\n", arg)),
                        _ => asm.push_str(&format!("    ; arg{}: {}\n", i, arg)),
                    }
                }
                asm.push_str(&format!("    call {}\n", func));
                asm
            }
            _ => format!("    ; call {} - target not implemented\n", func),
        }
    }
    
    fn emit_reg_move(&mut self, src: &str, dst: &str) -> String {
        format!("    mov {}, {}\n", dst, src)
    }
    
    fn emit_label(&mut self, label: &str) -> String {
        format!("{}:\n", label)
    }
    
    fn emit_comment(&mut self, comment: &str) -> String {
        format!("    ; {}\n", comment)
    }
}

/// Extension Registry for dynamic module loading
pub struct ExtensionRegistry {
    modules: Vec<Box<dyn EarthngModule>>,
    loaded_modules: HashMap<String, Box<dyn EarthngModule>>,
}

impl ExtensionRegistry {
    pub fn new() -> Self {
        Self {
            modules: Vec::new(),
            loaded_modules: HashMap::new(),
        }
    }
    
    /// Register a new module
    pub fn register_module(&mut self, module: Box<dyn EarthngModule>) {
        let name = module.name().to_string();
        self.modules.push(module);
        if !self.loaded_modules.contains_key(&name) {}
    }
    
    /// Find a module that supports a function
    pub fn find_module_for_function(&self, func: &str) -> Option<&dyn EarthngModule> {
        for module in &self.modules {
            if module.supports_function(func) {
                return Some(module.as_ref());
            }
        }
        None
    }
    
    /// Get all registered modules
    pub fn modules(&self) -> &[Box<dyn EarthngModule>] {
        &self.modules
    }
    
    /// Load a module by name
    pub fn load_module(&mut self, name: &str) -> Result<(), String> {
        for module in &self.modules {
            if module.name() == name {
                return Err("Module cloning not implemented".to_string());
            }
        }
        Err(format!("Module '{}' not found", name))
    }
    
    /// Check if a function is available in any module
    pub fn has_function(&self, func: &str) -> bool {
        self.find_module_for_function(func).is_some()
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
        
        registry.register(Box::new(Bios16Backend::new()));
        registry.register(Box::new(Bios32Backend::new()));
        registry.register(Box::new(Bios64Backend::new()));
        registry.register(Box::new(Bios64Backend::new().with_sse()));
        registry.register(Box::new(Bios64Backend::new().with_avx()));
        registry.register(Box::new(Bios64Backend::new().with_avx512()));
        registry.register(Box::new(Linux64Backend::new()));
        
        registry
    }
}

pub struct Bios16Backend {
    string_literals: HashMap<String, String>,
}

impl Bios16Backend {
    pub fn new() -> Self {
        Self {
            string_literals: HashMap::new(),
        }
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
        
        asm.push_str("; Earthang BIOS 16-bit Backend\n");
        asm.push_str("; Generated from Earthang AST\n\n");
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
        
        asm.push_str("; String literals\n");
        asm.push_str(&self.generate_string_data());
        
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
}

impl Bios32Backend {
    pub fn new() -> Self {
        Self {
            string_literals: HashMap::new(),
        }
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
        
        asm.push_str("; Earthang BIOS 32-bit Backend\n");
        asm.push_str("; Generated from Earthang AST\n\n");
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
        asm.push_str("    pusha\n");
        asm.push_str("    mov ecx, 0\n");
        asm.push_str("    mov ebx, 10\n");
        asm.push_str("    mov edi, 0xB8000 + 160\n");
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
        
        asm.push_str("; String literals\n");
        asm.push_str(&self.generate_string_data());
        
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
    code_size_limit: usize,
    hardware_dsl: HardwareDSL,
    label_counter: RefCell<u32>,
    memory_manager: std::rc::Rc<std::cell::RefCell<BareMetalMemoryManager>>,
    current_function: RefCell<Option<String>>,
    current_stack_depth: RefCell<i32>,
}

impl Bios64Backend {
    pub fn new() -> Self {
        Self {
            use_sse: false,
            use_avx: false,
            use_avx512: false,
            code_size_limit: 4096,
            hardware_dsl: HardwareDSL::new(),
            label_counter: RefCell::new(0),
            memory_manager: std::rc::Rc::new(std::cell::RefCell::new(BareMetalMemoryManager::new())),
            current_function: RefCell::new(None),
            current_stack_depth: RefCell::new(0),
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
    
    pub fn with_avx512(mut self) -> Self {
        self.use_avx512 = true;
        self
    }
    
    fn get_next_label_id(&self) -> u32 {
        let mut counter = self.label_counter.borrow_mut();
        let id = *counter;
        *counter += 1;
        id
    }
    
    fn allocate_global_variable(&self, name: &str, size: usize, type_: &str, 
                               value: Option<String>) -> Result<u64, String> {
        let mut mm = self.memory_manager.borrow_mut();
        
        let align = match type_ {
            "int" | "float" => 8,
            "double" => 8,
            "string" => 1,
            "array" => 16,
            _ => 8,
        };
        
        let address = mm.allocate_data(size, align)?;
        mm.register_symbol(name.to_string(), address, size, type_, value);
        
        Ok(address)
    }
    
    fn store_string_literal(&self, content: &str) -> (u64, String) {
        let mut mm = self.memory_manager.borrow_mut();
        mm.register_string_literal(content)
    }
    
    fn get_variable_address(&self, name: &str) -> Result<u64, String> {
        let mm = self.memory_manager.borrow();
        
        if let Some(symbol) = mm.get_symbol(name) {
            Ok(symbol.address)
        } else {
            Err(format!("Undefined variable: {}", name))
        }
    }
    
    fn get_string_address(&self, content: &str) -> Option<u64> {
        let mm = self.memory_manager.borrow();
        mm.get_string_address(content)
    }
    
    fn generate_data_section(&self) -> String {
        let mm = self.memory_manager.borrow();
        let mut data = String::new();
        
        data.push_str("\n; ========== DATA SECTION ==========\n");
        data.push_str("section .data\n");
        data.push_str("align 16\n\n");
        
        for (name, symbol) in &mm.symbol_table {
            if symbol.type_ == "string" && symbol.value.is_some() {
                data.push_str(&format!("; Global string: {}\n", name));
                data.push_str(&format!("{}_addr:\n", name));
                data.push_str(&format!("    dq {}\n", symbol.address));
                
                if let Some(content) = &symbol.value {
                    data.push_str(&format!("{}:\n", symbol.address));
                    data.push_str(&format!("    db '{}', 0\n", content.replace("'", "''")));
                }
            } else {
                data.push_str(&format!("; Global variable: {} ({}) at {:#x}\n", 
                    name, symbol.type_, symbol.address));
                data.push_str(&format!("{}_addr:\n", name));
                data.push_str(&format!("    dq {}\n", symbol.address));
                
                data.push_str(&format!("{}:\n", symbol.address));
                if symbol.is_initialized {
                    if let Some(value) = &symbol.value {
                        match symbol.type_.as_str() {
                            "int" => {
                                if let Ok(num) = value.parse::<i64>() {
                                    data.push_str(&format!("    dq {}\n", num));
                                } else {
                                    data.push_str("    dq 0\n");
                                }
                            }
                            "float" => {
                                data.push_str(&format!("    dd {}\n", value));
                            }
                            "double" => {
                                data.push_str(&format!("    dq {}\n", value));
                            }
                            _ => {
                                data.push_str(&format!("    times {} db 0\n", symbol.size));
                            }
                        }
                    }
                } else {
                    data.push_str(&format!("    times {} db 0\n", symbol.size));
                }
            }
            data.push_str("\n");
        }
        
        if !mm.string_literals.is_empty() {
            data.push_str("; String literals\n");
            for (content, (address, label)) in &mm.string_literals {
                data.push_str(&format!("{}_addr:\n", label));
                data.push_str(&format!("    dq {:#x}\n", address));
                data.push_str(&format!("{}:\n", address));
                data.push_str(&format!("    db '{}', 0\n", content.replace("'", "''")));
                data.push_str("\n");
            }
        }
        
        data
    }
    
    fn generate_bss_section(&self) -> String {
        let mm = self.memory_manager.borrow();
        let mut bss = String::new();
        
        let uninitialized: Vec<&Symbol> = mm.symbol_table.values()
            .filter(|s| !s.is_initialized && s.type_ != "string")
            .collect();
        
        if !uninitialized.is_empty() {
            bss.push_str("\n; ========== BSS SECTION ==========\n");
            bss.push_str("section .bss\n");
            bss.push_str("align 16\n\n");
            
            for symbol in uninitialized {
                bss.push_str(&format!("{} resb {}\n\n", symbol.address, symbol.size));
            }
        }
        
        bss
    }
    
    fn compile_expression_impl(&mut self, expr: &Expr, func_ctx: &mut Option<&mut FunctionContext>) -> Result<String, String> {
        let mut code = String::new();
        
        match expr {
            Expr::Number(n, _) => {
                code.push_str(&format!("    ; Number: {}\n", n));
                code.push_str(&format!("    mov rax, {}\n", n));
            }
            Expr::String(s, _) => {
                let (address, _) = self.store_string_literal(s);
                code.push_str(&format!("    ; String: '{}' at {:#x}\n", s.replace("'", "''"), address));
                code.push_str(&format!("    mov rsi, {:#x}\n", address));
            }
            Expr::Var(name, _) => {
                if let Some(ctx) = func_ctx {
                    if let Some(var_info) = ctx.get_local(name) {
                        code.push_str(&format!("    ; Local variable: {} at [rbp - {}]\n", name, var_info.offset));
                        code.push_str(&format!("    mov rax, [rbp - {}]\n", var_info.offset));
                    } else if let Some(symbol) = ctx.get_symbol(name) {
                        code.push_str(&format!("    ; Function symbol: {} at {:#x}\n", name, symbol.address));
                        code.push_str(&format!("    mov rax, [{:#x}]\n", symbol.address));
                    } else {
                        match self.get_variable_address(name) {
                            Ok(address) => {
                                code.push_str(&format!("    ; Global variable: {} at {:#x}\n", name, address));
                                code.push_str(&format!("    mov rax, [{:#x}]\n", address));
                            }
                            Err(e) => return Err(e),
                        }
                    }
                } else {
                    match self.get_variable_address(name) {
                        Ok(address) => {
                            code.push_str(&format!("    ; Global variable: {} at {:#x}\n", name, address));
                            code.push_str(&format!("    mov rax, [{:#x}]\n", address));
                        }
                        Err(e) => return Err(e),
                    }
                }
            }
            Expr::Call { func, args, kwargs: _, span: _ } => {
                if func == "print" {
                    if let Some(arg) = args.get(0) {
                        match arg {
                            Expr::Number(_n, _) => {
                                code.push_str(&self.compile_expression_impl(arg, func_ctx)?);
                                code.push_str("    call print_decimal_64\n");
                            }
                            Expr::String(s, _) => {
                                let (address, _) = self.store_string_literal(s);
                                code.push_str(&format!("    ; Print string: '{}'\n", s.replace("'", "''")));
                                code.push_str(&format!("    mov rsi, {:#x}\n", address));
                                code.push_str("    mov rdi, rsi\n");
                                code.push_str("    call print_string_64\n");
                            }
                            Expr::Var(_name, _) => {
                                code.push_str(&self.compile_expression_impl(arg, func_ctx)?);
                                code.push_str("    call print_decimal_64\n");
                            }
                            Expr::BinOp { left, op, right, span: _ } => {
                                let left_code = self.compile_expression_impl(left, func_ctx)?;
                                let right_code = self.compile_expression_impl(right, func_ctx)?;
                                
                                code.push_str(&left_code);
                                code.push_str("    push rax\n");
                                code.push_str(&right_code);
                                code.push_str("    mov rbx, rax\n");
                                code.push_str("    pop rax\n");
                                
                                match op {
                                    Op::Add => code.push_str("    add rax, rbx\n"),
                                    Op::Sub => code.push_str("    sub rax, rbx\n"),
                                    Op::Mul => code.push_str("    imul rax, rbx\n"),
                                    Op::Div => {
                                        code.push_str("    xor rdx, rdx\n");
                                        code.push_str("    idiv rbx\n");
                                    }
                                    Op::Mod => {
                                        code.push_str("    xor rdx, rdx\n");
                                        code.push_str("    div rbx\n");
                                        code.push_str("    mov rax, rdx\n");
                                    }
                                    _ => return Err(format!("Unsupported operator in print: {:?}", op)),
                                }
                                
                                code.push_str("    call print_decimal_64\n");
                            }
                            _ => return Err(format!("Unsupported print argument type: {:?}", arg)),
                        }
                    } else {
                        let (address, _) = self.store_string_literal("\n");
                        code.push_str("    ; Empty print - newline\n");
                        code.push_str(&format!("    mov rsi, {:#x}\n", address));
                        code.push_str("    mov rdi, rsi\n");
                        code.push_str("    call print_string_64\n");
                    }
                } else {
                    code.push_str(&format!("    ; Function call: {}\n", func));
                    
                    for (i, arg) in args.iter().enumerate() {
                        let arg_code = self.compile_expression_impl(arg, func_ctx)?;
                        code.push_str(&arg_code);
                        
                        match i {
                            0 => code.push_str("    mov rdi, rax\n"),
                            1 => code.push_str("    mov rsi, rax\n"),
                            2 => code.push_str("    mov rdx, rax\n"),
                            3 => code.push_str("    mov rcx, rax\n"),
                            4 => code.push_str("    mov r8, rax\n"),
                            5 => code.push_str("    mov r9, rax\n"),
                            _ => {
                                code.push_str("    push rax\n");
                            }
                        }
                    }
                    
                    if args.len() > 6 {
                        let stack_args = args.len() - 6;
                        if stack_args % 2 != 0 {
                            code.push_str("    sub rsp, 8\n");
                        }
                    }
                    
                    code.push_str(&format!("    call {}\n", func));
                    
                    if args.len() > 6 {
                        let stack_args = args.len() - 6;
                        code.push_str(&format!("    add rsp, {}\n", stack_args * 8));
                    }
                }
            }
            Expr::BinOp { left, op, right, span: _ } => {
                code.push_str(&self.compile_expression_impl(left, func_ctx)?);
                code.push_str("    push rax\n");
                
                code.push_str(&self.compile_expression_impl(right, func_ctx)?);
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
                        code.push_str("    mov rax, rdx\n");
                    }
                    _ => {
                        return Err(format!("Unsupported operator: {:?}", op));
                    }
                }
            }
            _ => {
                return Err(format!("Unsupported expression type: {:?}", expr));
            }
        }
        
        Ok(code)
    }
    
    
    fn compile_statement(&mut self, stmt: &Statement, func_ctx: &mut FunctionContext) -> Result<String, String> {
    let mut code = String::new();
    
    match stmt {
        Statement::VarDecl { name, value, type_hint: _, span: _ } => {
            let offset = func_ctx.allocate_local_variable(name, None)?;
            
            code.push_str(&format!("    ; Variable declaration: {} at [rbp - {}]\n", name, offset));
            
            {
                let value_code = self.compile_expression_impl(value, &mut Some(func_ctx))?;
                code.push_str(&value_code);
            }
            
            code.push_str(&format!("    mov [rbp - {}], rax\n", offset));
        }
        Statement::Assign { target, value, span: _ } => {
            if let Some(var_info) = func_ctx.get_local(target) {
                let offset = var_info.offset;
                code.push_str(&format!("    ; Assignment to local: {} at [rbp - {}]\n", target, offset));
                
                {
                    let value_code = self.compile_expression_impl(value, &mut Some(func_ctx))?;
                    code.push_str(&value_code);
                }
                
                code.push_str(&format!("    mov [rbp - {}], rax\n", offset));
            } else {
                match self.get_variable_address(target) {
                    Ok(address) => {
                        code.push_str(&format!("    ; Assignment to global: {} at {:#x}\n", target, address));
                        
                        {
                            let value_code = self.compile_expression_impl(value, &mut Some(func_ctx))?;
                            code.push_str(&value_code);
                        }
                        
                        code.push_str(&format!("    mov [{:#x}], rax\n", address));
                    }
                    Err(e) => return Err(e),
                }
            }
        }
        Statement::Expr(expr) => {
            code.push_str("    ; Expression statement\n");
            {
                let expr_code = self.compile_expression_impl(expr, &mut Some(func_ctx))?;
                code.push_str(&expr_code);
            }
        }
        Statement::Return(expr) => {
            if let Some(expr) = expr {
                code.push_str("    ; Return statement\n");
                {
                    let expr_code = self.compile_expression_impl(expr, &mut Some(func_ctx))?;
                    code.push_str(&expr_code);
                }
            } else {
                code.push_str("    xor rax, rax\n");
            }
            
            if func_ctx.stack_size > 0 {
                code.push_str(&format!("    add rsp, {}\n", func_ctx.stack_size));
            }
            
            code.push_str("    mov rsp, rbp\n");
            code.push_str("    pop rbp\n");
            code.push_str("    ret\n");
            
            func_ctx.has_returned = true;
        }
        Statement::If { condition, then_block, elif_blocks, else_block, span: _ } => {
            let label_id = self.get_next_label_id();
            let else_label = format!("if_else_{}", label_id);
            let end_label = format!("if_end_{}", label_id);
            
            code.push_str("    ; If condition\n");
            {
                let cond_code = self.compile_expression_impl(condition, &mut Some(func_ctx))?;
                code.push_str(&cond_code);
            }
            
            code.push_str("    test rax, rax\n");
            code.push_str(&format!("    jz {}\n", else_label));
            
            code.push_str("    ; Then block\n");
            for stmt in then_block {
                let stmt_code = self.compile_statement(stmt, func_ctx)?;
                code.push_str(&stmt_code);
            }
            code.push_str(&format!("    jmp {}\n", end_label));
            
            let mut current_else_label = else_label.clone();
            for (i, (elif_cond, elif_body)) in elif_blocks.iter().enumerate() {
                code.push_str(&format!("{}:\n", current_else_label));
                
                {
                    let elif_cond_code = self.compile_expression_impl(elif_cond, &mut Some(func_ctx))?;
                    code.push_str(&elif_cond_code);
                }
                
                let next_label = if i < elif_blocks.len() - 1 || else_block.is_some() {
                    format!("{}_elif_{}", current_else_label, i)
                } else {
                    end_label.clone()
                };
                
                code.push_str("    test rax, rax\n");
                code.push_str(&format!("    jz {}\n", next_label));
                
                for stmt in elif_body {
                    let stmt_code = self.compile_statement(stmt, func_ctx)?;
                    code.push_str(&stmt_code);
                }
                code.push_str(&format!("    jmp {}\n", end_label));
                
                code.push_str(&format!("{}:\n", next_label));
                current_else_label = next_label;
            }
            
            if let Some(else_body) = else_block {
                if elif_blocks.is_empty() {
                    code.push_str(&format!("{}:\n", else_label));
                } else {
                    code.push_str(&format!("{}:\n", current_else_label));
                }
                
                code.push_str("    ; Else block\n");
                for stmt in else_body {
                    let stmt_code = self.compile_statement(stmt, func_ctx)?;
                    code.push_str(&stmt_code);
                }
            } else if elif_blocks.is_empty() {
                code.push_str(&format!("{}:\n", else_label));
            }
            
            code.push_str(&format!("{}:\n", end_label));
        }
        Statement::While { condition, body, orelse: _, span: _ } => {
            let label_id = self.get_next_label_id();
            let while_start = format!("while_start_{}", label_id);
            let while_end = format!("while_end_{}", label_id);
            
            code.push_str(&format!("{}:\n", while_start));
            
            code.push_str("    ; While condition\n");
            {
                let cond_code = self.compile_expression_impl(condition, &mut Some(func_ctx))?;
                code.push_str(&cond_code);
            }
            
            code.push_str("    test rax, rax\n");
            code.push_str(&format!("    jz {}\n", while_end));
            
            code.push_str("    ; While body\n");
            for stmt in body {
                let stmt_code = self.compile_statement(stmt, func_ctx)?;
                code.push_str(&stmt_code);
            }
            
            code.push_str(&format!("    jmp {}\n", while_start));
            code.push_str(&format!("{}:\n", while_end));
        }
        Statement::Pass => {
            code.push_str("    ; pass\n");
        }
        Statement::Break => {
            code.push_str("    ; break\n");
        }
        Statement::Continue => {
            code.push_str("    ; continue\n");
        }
        _ => {
            code.push_str(&format!("    ; [Statement type: {:?}]\n", stmt));
        }
    }
    
    Ok(code)
}

    
    fn compile_function(&mut self, name: &str, args: &[String], body: &[Statement]) -> Result<String, String> {
        let mut code = String::new();
        let mut func_ctx = FunctionContext::new(name.to_string(), args.to_vec(), 
                                               std::rc::Rc::clone(&self.memory_manager));
        
        *self.current_function.borrow_mut() = Some(name.to_string());
        *self.current_stack_depth.borrow_mut() = 0;
        
        code.push_str(&format!("{}:\n", name));
        code.push_str("    push rbp\n");
        code.push_str("    mov rbp, rsp\n");
        
        let local_count = body.iter()
            .filter(|stmt| matches!(stmt, Statement::VarDecl { .. }))
            .count();
        
        if local_count > 0 {
            let stack_size = ((local_count * 8) + 15) & !15;
            code.push_str(&format!("    sub rsp, {}\n", stack_size));
            func_ctx.stack_size = stack_size as i32;
            *self.current_stack_depth.borrow_mut() = stack_size as i32;
        }
        
        for (i, arg) in args.iter().enumerate() {
            match i {
                0 => {
                    code.push_str(&format!("    ; {} -> rdi\n", arg));
                    code.push_str("    mov [rbp - 8], rdi\n");
                    func_ctx.allocate_local_variable(arg, Some("int".to_string()))?;
                }
                1 => {
                    code.push_str(&format!("    ; {} -> rsi\n", arg));
                    code.push_str("    mov [rbp - 16], rsi\n");
                    func_ctx.allocate_local_variable(arg, Some("int".to_string()))?;
                }
                2 => {
                    code.push_str(&format!("    ; {} -> rdx\n", arg));
                    code.push_str("    mov [rbp - 24], rdx\n");
                    func_ctx.allocate_local_variable(arg, Some("int".to_string()))?;
                }
                3 => {
                    code.push_str(&format!("    ; {} -> rcx\n", arg));
                    code.push_str("    mov [rbp - 32], rcx\n");
                    func_ctx.allocate_local_variable(arg, Some("int".to_string()))?;
                }
                4 => {
                    code.push_str(&format!("    ; {} -> r8\n", arg));
                    code.push_str("    mov [rbp - 40], r8\n");
                    func_ctx.allocate_local_variable(arg, Some("int".to_string()))?;
                }
                5 => {
                    code.push_str(&format!("    ; {} -> r9\n", arg));
                    code.push_str("    mov [rbp - 48], r9\n");
                    func_ctx.allocate_local_variable(arg, Some("int".to_string()))?;
                }
                _ => {
                    let stack_offset = 16 + (i - 6) * 8;
                    code.push_str(&format!("    ; {} -> [rbp + {}]\n", arg, stack_offset));
                    let offset = func_ctx.allocate_local_variable(arg, Some("int".to_string()))?;
                    code.push_str(&format!("    mov rax, [rbp + {}]\n", stack_offset));
                    code.push_str(&format!("    mov [rbp - {}], rax\n", offset));
                }
            }
        }
        
        for stmt in body {
            code.push_str(&self.compile_statement(stmt, &mut func_ctx)?);
        }
        
        if !func_ctx.has_returned {
            code.push_str("    ; Default return\n");
            code.push_str("    xor rax, rax\n");
            
            if func_ctx.stack_size > 0 {
                code.push_str(&format!("    add rsp, {}\n", func_ctx.stack_size));
            }
            code.push_str("    mov rsp, rbp\n");
            code.push_str("    pop rbp\n");
            code.push_str("    ret\n");
        }
        
        *self.current_function.borrow_mut() = None;
        *self.current_stack_depth.borrow_mut() = 0;
        
        Ok(code)
    }
    
    fn compile_hardware_function_def(&self, device: &str, name: &str, args: &[String], body: &[Statement]) -> Result<String, String> {
        let mut asm = String::new();
        
        asm.push_str(&format!("; Hardware function: {} for device {}\n", name, device));
        asm.push_str(&format!("{}_hw_{}:\n", name, device));
        asm.push_str("    push rbp\n");
        asm.push_str("    mov rbp, rsp\n");
        asm.push_str("    push rbx\n");
        asm.push_str("    push rcx\n");
        asm.push_str("    push rdx\n");
        asm.push_str("    push rsi\n");
        asm.push_str("    push rdi\n");
        asm.push_str(&format!("    ; Device: {}, Function: {}\n", device, name));
        
        if !args.is_empty() {
            asm.push_str(&format!("    ; {} argument(s): {}\n", args.len(), args.join(", ")));
            for (i, arg) in args.iter().enumerate() {
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
        
        asm.push_str("    ; Hardware function body\n");
        for stmt in body {
            asm.push_str(&format!("    ; Statement: {:?}\n", stmt));
        }
        
        asm.push_str("    ; Hardware function cleanup\n");
        asm.push_str("    pop rdi\n");
        asm.push_str("    pop rsi\n");
        asm.push_str("    pop rdx\n");
        asm.push_str("    pop rcx\n");
        asm.push_str("    pop rbx\n");
        asm.push_str("    mov rsp, rbp\n");
        asm.push_str("    pop rbp\n");
        asm.push_str("    ret\n");
        
        Ok(asm)
    }
    
    fn generate_utility_functions(&self) -> String {
        let mut utils = String::new();
        
        utils.push_str("; ========== UTILITY FUNCTIONS ==========\n\n");
        
        utils.push_str("; Print null-terminated string\n");
        utils.push_str("; Input: RSI = string address\n");
        utils.push_str("print_string_64:\n");
        utils.push_str("    push rax\n");
        utils.push_str("    push rdi\n");
        utils.push_str("    push rsi\n");
        utils.push_str("    \n");
        utils.push_str("    mov rdi, 0xB8000\n");
        utils.push_str("    add rdi, 160\n");
        utils.push_str("    \n");
        utils.push_str(".print_loop:\n");
        utils.push_str("    mov al, [rsi]\n");
        utils.push_str("    test al, al\n");
        utils.push_str("    jz .print_done\n");
        utils.push_str("    \n");
        utils.push_str("    mov [rdi], al\n");
        utils.push_str("    inc rdi\n");
        utils.push_str("    mov byte [rdi], 0x0F\n");
        utils.push_str("    inc rdi\n");
        utils.push_str("    inc rsi\n");
        utils.push_str("    jmp .print_loop\n");
        utils.push_str("    \n");
        utils.push_str(".print_done:\n");
        utils.push_str("    pop rsi\n");
        utils.push_str("    pop rdi\n");
        utils.push_str("    pop rax\n");
        utils.push_str("    ret\n\n");
        
        utils.push_str("; Print decimal number\n");
        utils.push_str("; Input: RAX = number to print\n");
        utils.push_str("print_decimal_64:\n");
        utils.push_str("    push rax\n");
        utils.push_str("    push rbx\n");
        utils.push_str("    push rcx\n");
        utils.push_str("    push rdx\n");
        utils.push_str("    push rdi\n");
        utils.push_str("    \n");
        utils.push_str("    test rax, rax\n");
        utils.push_str("    jnz .not_zero\n");
        utils.push_str("    \n");
        utils.push_str("    mov rdi, 0xB8000\n");
        utils.push_str("    add rdi, 320\n");
        utils.push_str("    mov byte [rdi], '0'\n");
        utils.push_str("    inc rdi\n");
        utils.push_str("    mov byte [rdi], 0x0F\n");
        utils.push_str("    jmp .print_done_decimal\n");
        utils.push_str("    \n");
        utils.push_str(".not_zero:\n");
        utils.push_str("    mov rcx, rax\n");
        utils.push_str("    sar rcx, 63\n");
        utils.push_str("    xor rax, rcx\n");
        utils.push_str("    sub rax, rcx\n");
        utils.push_str("    \n");
        utils.push_str("    mov rdi, 0xB8000\n");
        utils.push_str("    add rdi, 320\n");
        utils.push_str("    add rdi, 158\n");
        utils.push_str("    \n");
        utils.push_str("    mov rbx, 10\n");
        utils.push_str("    mov rcx, 0\n");
        utils.push_str("    \n");
        utils.push_str(".convert_loop:\n");
        utils.push_str("    xor rdx, rdx\n");
        utils.push_str("    div rbx\n");
        utils.push_str("    add dl, '0'\n");
        utils.push_str("    mov [rdi], dl\n");
        utils.push_str("    dec rdi\n");
        utils.push_str("    mov byte [rdi], 0x0F\n");
        utils.push_str("    dec rdi\n");
        utils.push_str("    inc rcx\n");
        utils.push_str("    test rax, rax\n");
        utils.push_str("    jnz .convert_loop\n");
        utils.push_str("    \n");
        utils.push_str("    test rcx, rcx\n");
        utils.push_str("    jns .print_done_decimal\n");
        utils.push_str("    \n");
        utils.push_str("    mov byte [rdi], '-'\n");
        utils.push_str("    dec rdi\n");
        utils.push_str("    mov byte [rdi], 0x0F\n");
        utils.push_str("    \n");
        utils.push_str(".print_done_decimal:\n");
        utils.push_str("    pop rdi\n");
        utils.push_str("    pop rdx\n");
        utils.push_str("    pop rcx\n");
        utils.push_str("    pop rbx\n");
        utils.push_str("    pop rax\n");
        utils.push_str("    ret\n\n");
        
        utils.push_str("; Simple delay function\n");
        utils.push_str("; Input: RCX = number of iterations\n");
        utils.push_str("delay:\n");
        utils.push_str("    push rcx\n");
        utils.push_str(".delay_loop:\n");
        utils.push_str("    nop\n");
        utils.push_str("    nop\n");
        utils.push_str("    nop\n");
        utils.push_str("    nop\n");
        utils.push_str("    loop .delay_loop\n");
        utils.push_str("    pop rcx\n");
        utils.push_str("    ret\n\n");
        
        utils.push_str("; Copy memory\n");
        utils.push_str("; Input: RDI = destination, RSI = source, RCX = count\n");
        utils.push_str("memcpy:\n");
        utils.push_str("    push rax\n");
        utils.push_str("    push rdi\n");
        utils.push_str("    push rsi\n");
        utils.push_str("    push rcx\n");
        utils.push_str("    \n");
        utils.push_str("    test rcx, rcx\n");
        utils.push_str("    jz .memcpy_done\n");
        utils.push_str("    \n");
        utils.push_str(".memcpy_loop:\n");
        utils.push_str("    mov al, [rsi]\n");
        utils.push_str("    mov [rdi], al\n");
        utils.push_str("    inc rsi\n");
        utils.push_str("    inc rdi\n");
        utils.push_str("    dec rcx\n");
        utils.push_str("    jnz .memcpy_loop\n");
        utils.push_str("    \n");
        utils.push_str(".memcpy_done:\n");
        utils.push_str("    pop rcx\n");
        utils.push_str("    pop rsi\n");
        utils.push_str("    pop rdi\n");
        utils.push_str("    pop rax\n");
        utils.push_str("    ret\n\n");
        
        utils.push_str("; Set memory\n");
        utils.push_str("; Input: RDI = destination, AL = value, RCX = count\n");
        utils.push_str("memset:\n");
        utils.push_str("    push rdi\n");
        utils.push_str("    push rcx\n");
        utils.push_str("    \n");
        utils.push_str("    test rcx, rcx\n");
        utils.push_str("    jz .memset_done\n");
        utils.push_str("    \n");
        utils.push_str(".memset_loop:\n");
        utils.push_str("    mov [rdi], al\n");
        utils.push_str("    inc rdi\n");
        utils.push_str("    dec rcx\n");
        utils.push_str("    jnz .memset_loop\n");
        utils.push_str("    \n");
        utils.push_str(".memset_done:\n");
        utils.push_str("    pop rcx\n");
        utils.push_str("    pop rdi\n");
        utils.push_str("    ret\n");
        
        utils
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
        
        asm.push_str("; Earthang BIOS 64-bit Backend\n");
        asm.push_str("; Generated from Earthang AST\n");
        asm.push_str("; Memory Manager: Bare Metal Symbol Table\n\n");
        
        asm.push_str("    org 0x7C00\n");
        asm.push_str("    bits 16\n\n");
        
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
        
        asm.push_str("; Memory Layout for Bare Metal:\n");
        asm.push_str(";   0x000000-0x0003FF: Interrupt Vector Table (1KB)\n");
        asm.push_str(";   0x000400-0x0004FF: BIOS Data Area (256B)\n");
        asm.push_str(";   0x000500-0x07BFF: Free conventional memory (~30KB)\n");
        asm.push_str(";   0x07C00-0x07DFF: Boot sector (512B) - Our code\n");
        asm.push_str(";   0x07E00-0x07FFF: Boot sector stack (512B)\n");
        asm.push_str(";   0x08000-0x9FFFF: Extended conventional memory (~608KB)\n");
        asm.push_str(";   0x100000-0x4000000: Heap (64MB) - Dynamic allocation\n");
        asm.push_str(";   0x200000-0x400000: Data section (2MB) - Global variables\n");
        asm.push_str(";   0xA0000-0xBFFFF: Video memory (128KB)\n");
        asm.push_str(";   0xC0000-0xFFFFF: BIOS ROM (256KB)\n\n");
        
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
        asm.push_str("    mov esp, 0x90000\n\n");
        
        asm.push_str("    ; Setup paging for 64-bit mode\n");
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
        
        asm.push_str("    mov eax, cr4\n");
        asm.push_str("    or eax, (1 << 5)\n");
        asm.push_str("    mov cr4, eax\n\n");
        
        asm.push_str("    mov eax, 0x1000\n");
        asm.push_str("    mov cr3, eax\n\n");
        
        asm.push_str("    mov ecx, 0xC0000080\n");
        asm.push_str("    rdmsr\n");
        asm.push_str("    or eax, (1 << 8)\n");
        asm.push_str("    wrmsr\n\n");
        
        asm.push_str("    mov eax, cr0\n");
        asm.push_str("    or eax, (1 << 31)\n");
        asm.push_str("    mov cr0, eax\n\n");
        
        asm.push_str("    lgdt [gdt64_desc]\n\n");
        
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
        
        asm.push_str("    ; Initialize data section base register\n");
        asm.push_str("    mov r15, 0x200000\n\n");
        
        asm.push_str("    ; Clear screen\n");
        asm.push_str("    mov rdi, 0xB8000\n");
        asm.push_str("    mov rax, 0x0720072007200720\n");
        asm.push_str("    mov rcx, 1000\n");
        asm.push_str("    rep stosq\n\n");
        
        let (welcome_addr, _) = self.store_string_literal("Earthang 64-bit Bare Metal");
        asm.push_str("    ; Print welcome message\n");
        asm.push_str(&format!("    mov rsi, {:#x}\n", welcome_addr));
        asm.push_str("    mov rdi, 0xB8000\n");
        asm.push_str("    call print_string_64\n\n");
        
        let mut functions = Vec::new();
        let mut main_statements = Vec::new();
        
        for stmt in &program.body {
            match stmt {
                Statement::FunctionDef { name, args, body, span: _ } => {
                    functions.push((name.clone(), args.clone(), body.clone()));
                }
                Statement::HardwareFunctionDef { device, name, args, body, span: _ } => {
                    let hw_code = self.compile_hardware_function_def(device, name, args, body)?;
                    asm.push_str(&hw_code);
                }
                _ => {
                    main_statements.push(stmt.clone());
                }
            }
        }
        
        if !main_statements.is_empty() {
            asm.push_str("    ; Main program code\n");
            
            let mut main_ctx = FunctionContext::new("main".to_string(), vec![], 
                                                   std::rc::Rc::clone(&self.memory_manager));
            
            let local_count = main_statements.iter()
                .filter(|stmt| matches!(stmt, Statement::VarDecl { .. }))
                .count();
            
            if local_count > 0 {
                let stack_size = ((local_count * 8) + 15) & !15;
                asm.push_str(&format!("    sub rsp, {}\n", stack_size));
                main_ctx.stack_size = stack_size as i32;
            }
            
            for stmt in main_statements {
                match stmt {
                    Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                        match value {
                            Expr::Number(n, _) => {
                                let address = self.allocate_global_variable(&name, 8, "int", Some(n.to_string()))?;
                                asm.push_str(&format!("    ; Global variable: {} = {} at {:#x}\n", name, n, address));
                                asm.push_str(&format!("    mov rax, {}\n", n));
                                asm.push_str(&format!("    mov [{:#x}], rax\n", address));
                            }
                            Expr::String(s, _) => {
                                let (str_addr, _) = self.store_string_literal(&s);
                                let address = self.allocate_global_variable(&name, 8, "string", Some(s.to_string()))?;
                                asm.push_str(&format!("    ; Global string: {} = '{}' at {:#x}\n", name, s.replace("'", "''"), address));
                                asm.push_str(&format!("    mov rax, {:#x}\n", str_addr));
                                asm.push_str(&format!("    mov [{:#x}], rax\n", address));
                            }
                            _ => {
                                let mut ctx_ref = Some(&mut main_ctx);
                                let value_code = self.compile_expression_impl(&value, &mut ctx_ref)?;
                                let address = self.allocate_global_variable(&name, 8, "int", None)?;
                                asm.push_str(&format!("    ; Global variable: {} at {:#x}\n", name, address));
                                asm.push_str(&value_code);
                                asm.push_str(&format!("    mov [{:#x}], rax\n", address));
                            }
                        }
                    }
                    Statement::Expr(expr) => {
                        let mut ctx_ref = Some(&mut main_ctx);
                        let expr_code = self.compile_expression_impl(&expr, &mut ctx_ref)?;
                        asm.push_str(&expr_code);
                    }
                    Statement::Assign { target, value, span: _ } => {
                        if let Ok(address) = self.get_variable_address(&target) {
                            asm.push_str(&format!("    ; Assignment to global: {} at {:#x}\n", target, address));
                            let mut ctx_ref = Some(&mut main_ctx);
                            let value_code = self.compile_expression_impl(&value, &mut ctx_ref)?;
                            asm.push_str(&value_code);
                            asm.push_str(&format!("    mov [{:#x}], rax\n", address));
                        } else {
                            return Err(format!("Undefined variable: {}", target));
                        }
                    }
                    Statement::If { condition, then_block, elif_blocks, else_block, span: _ } => {
                        let label_id = self.get_next_label_id();
                        let else_label = format!("main_if_else_{}", label_id);
                        let end_label = format!("main_if_end_{}", label_id);
                        
                        let mut ctx_ref = Some(&mut main_ctx);
                        let cond_code = self.compile_expression_impl(&condition, &mut ctx_ref)?;
                        asm.push_str(&cond_code);
                        
                        asm.push_str("    test rax, rax\n");
                        asm.push_str(&format!("    jz {}\n", else_label));
                        
                        for stmt in then_block {
                            let stmt_code = self.compile_statement(&stmt, &mut main_ctx)?;
                            asm.push_str(&stmt_code);
                        }
                        asm.push_str(&format!("    jmp {}\n", end_label));
                        
                        for (elif_cond, elif_body) in &elif_blocks {
                            asm.push_str(&format!("{}:\n", else_label));
                            let mut ctx_ref = Some(&mut main_ctx);
                            let elif_cond_code = self.compile_expression_impl(&elif_cond, &mut ctx_ref)?;
                            asm.push_str(&elif_cond_code);
                            asm.push_str("    test rax, rax\n");
                            asm.push_str(&format!("    jz {}_elif\n", else_label));
                            
                            for stmt in elif_body {
                                let stmt_code = self.compile_statement(&stmt, &mut main_ctx)?;
                                asm.push_str(&stmt_code);
                            }
                            asm.push_str(&format!("    jmp {}\n", end_label));
                            asm.push_str(&format!("{}_elif:\n", else_label));
                        }
                        
                        if let Some(else_body) = else_block {
                            if elif_blocks.is_empty() {
                                asm.push_str(&format!("{}:\n", else_label));
                            }
                            
                            for stmt in else_body {
                                let stmt_code = self.compile_statement(&stmt, &mut main_ctx)?;
                                asm.push_str(&stmt_code);
                            }
                        } else if elif_blocks.is_empty() {
                            asm.push_str(&format!("{}:\n", else_label));
                        }
                        
                        asm.push_str(&format!("{}:\n", end_label));
                    }
                    Statement::While { condition, body, orelse: _, span: _ } => {
                        let label_id = self.get_next_label_id();
                        let while_start = format!("main_while_start_{}", label_id);
                        let while_end = format!("main_while_end_{}", label_id);
                        
                        asm.push_str(&format!("{}:\n", while_start));
                        
                        let mut ctx_ref = Some(&mut main_ctx);
                        let cond_code = self.compile_expression_impl(&condition, &mut ctx_ref)?;
                        asm.push_str(&cond_code);
                        
                        asm.push_str("    test rax, rax\n");
                        asm.push_str(&format!("    jz {}\n", while_end));
                        
                        for stmt in body {
                            let stmt_code = self.compile_statement(&stmt, &mut main_ctx)?;
                            asm.push_str(&stmt_code);
                        }
                        
                        asm.push_str(&format!("    jmp {}\n", while_start));
                        asm.push_str(&format!("{}:\n", while_end));
                    }
                    _ => {
                        asm.push_str(&format!("    ; [Statement type not handled in main: {:?}]\n", stmt));
                    }
                }
            }
            
            if main_ctx.stack_size > 0 {
                asm.push_str(&format!("    add rsp, {}\n", main_ctx.stack_size));
            }
        }
        
        for (name, args, body) in functions {
            asm.push_str(&format!("\n; Function: {}\n", name));
            let func_code = self.compile_function(&name, &args, &body)?;
            asm.push_str(&func_code);
        }
        
        asm.push_str("\n; ========== HARDWARE LIBRARY ==========\n");
        asm.push_str(&self.hardware_dsl.generate_hardware_library());
        
        asm.push_str(&self.generate_utility_functions());
        
        asm.push_str(&self.generate_data_section());
        asm.push_str(&self.generate_bss_section());
        
        asm.push_str("\n; ========== GDT ==========\n");
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
        
        asm.push_str("    times 510-($-$$) db 0\n");
        asm.push_str("    dw 0xAA55\n");
        
        Ok(asm)
    }
    
    fn compile_expression(&mut self, expr: &Expr) -> Result<String, String> {
        // Create a mutable reference to None for the context
        let mut dummy_ctx = None;
        self.compile_expression_impl(expr, &mut dummy_ctx)
    }
    
    fn function_prologue(&self, func: &BackendFunction) -> String {
        let mut prologue = String::new();
        prologue.push_str(&format!("{}:\n", func.name));
        prologue.push_str("    push rbp\n");
        prologue.push_str("    mov rbp, rsp\n");
        
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
    label_counter: RefCell<u32>,
}

impl Linux64Backend {
    pub fn new() -> Self {
        Self {
            string_counter: RefCell::new(0),
            string_literals: RefCell::new(HashMap::new()),
            symbol_table: RefCell::new(HashMap::new()),
            current_stack_offset: RefCell::new(0),
            label_counter: RefCell::new(0),
        }
    }

    fn compile_binary_op(&self, left: &Expr, op: &Op, right: &Expr) -> Result<String, String> {
    let mut code = String::new();
    
    match left {
        Expr::Number(n, _) => {
            code.push_str(&format!("    mov rax, {}\n", n));
        }
        Expr::Var(_name, _) => {
            code.push_str("    ; Load variable from memory\n");
            code.push_str("    mov rax, [rsp + 0]\n");
        }
        _ => {
            return Err("Unsupported left operand type in binary operation".to_string());
        }
    }
    
    code.push_str("    push rax\n");
    
    match right {
        Expr::Number(n, _) => {
            code.push_str(&format!("    mov rbx, {}\n", n));
        }
        Expr::Var(_name, _) => {
            code.push_str("    ; Load variable from memory\n");
            code.push_str("    mov rbx, [rsp + 8]\n");
        }
        _ => {
            return Err("Unsupported right operand type in binary operation".to_string());
        }
    }
    
    code.push_str("    pop rax\n");
    
    match op {
        Op::Add => code.push_str("    add rax, rbx\n"),
        Op::Sub => code.push_str("    sub rax, rbx\n"),
        Op::Mul => code.push_str("    imul rax, rbx\n"),
        Op::Div => {
            code.push_str("    xor rdx, rdx\n");
            code.push_str("    idiv rbx\n");
        }
        Op::Mod => {
            code.push_str("    xor rdx, rdx\n");
            code.push_str("    idiv rbx\n");
            code.push_str("    mov rax, rdx\n");
        }
        _ => return Err(format!("Unsupported operator: {:?}", op)),
    }
    
    Ok(code)
}
    
    fn allocate_variable_sp_relative(&self, name: &str) -> i32 {
        let mut offset = self.current_stack_offset.borrow_mut();
        
        if (*offset % 16) != 0 {
            *offset += 8;
        }
        
        let current = *offset;
        *offset += 8;
        
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
        helpers.push_str("    mov rsi, rdi\n");
        helpers.push_str("    xor rdx, rdx\n");
        helpers.push_str(".count_loop:\n");
        helpers.push_str("    cmp byte [rsi + rdx], 0\n");
        helpers.push_str("    je .count_done\n");
        helpers.push_str("    inc rdx\n");
        helpers.push_str("    jmp .count_loop\n");
        helpers.push_str(".count_done:\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Write to stdout\n");
        helpers.push_str("    mov rax, 1\n");
        helpers.push_str("    mov rdi, 1\n");
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
        helpers.push_str("    sub rsp, 32\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Save registers\n");
        helpers.push_str("    push rbx\n");
        helpers.push_str("    push rcx\n");
        helpers.push_str("    push rdx\n");
        helpers.push_str("    push rsi\n");
        helpers.push_str("    push rdi\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Save the number\n");
        helpers.push_str("    mov [rbp - 8], rax\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Point to buffer end\n");
        helpers.push_str("    lea rdi, [rsp + 31]\n");
        helpers.push_str("    mov byte [rdi], 0\n");
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
        helpers.push_str("    div rbx\n");
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
        helpers.push_str("    lea rsi, [rsp + 32]\n");
        helpers.push_str("    sub rsi, rdi\n");
        helpers.push_str("    \n");
        helpers.push_str("    ; Print the number\n");
        helpers.push_str("    mov rax, 1\n");
        helpers.push_str("    mov rdx, rsi\n");
        helpers.push_str("    mov rsi, rdi\n");
        helpers.push_str("    mov rdi, 1\n");
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
    
    asm.push_str("; Earthang Linux 64-bit Backend\n");
    asm.push_str("; Generated from Earthang AST\n\n");
    asm.push_str("    bits 64\n");
    asm.push_str("    default rel\n\n");
    
    asm.push_str("    section .text\n");
    asm.push_str("    global _start\n\n");
    
    asm.push_str("_start:\n");
    asm.push_str("    ; Set up stack\n");
    asm.push_str("    mov rbp, rsp\n");
    asm.push_str("    and rsp, -16\n");
    asm.push_str("    call main\n");
    asm.push_str("    ; Exit after main returns\n");
    asm.push_str("    mov rax, 60\n");
    asm.push_str("    xor rdi, rdi\n");
    asm.push_str("    syscall\n\n");
    
    asm.push_str("main:\n");
    asm.push_str("    push rbp\n");
    asm.push_str("    mov rbp, rsp\n");
    asm.push_str("    and rsp, -16\n");
    
    let var_count = self.count_variables(program);
    if var_count > 0 {
        let total_space = ((var_count * 8) + 15) & !15;
        asm.push_str(&format!("    sub rsp, {}\n", total_space));
    }
    
    {
        let mut offset = self.current_stack_offset.borrow_mut();
        *offset = 16;
        self.symbol_table.borrow_mut().clear();
    }
    
    for stmt in &program.body {
        match stmt {
            Statement::Expr(expr) => {
                let expr_code = self.compile_expression(&expr)?;
                asm.push_str(&expr_code);
            }
            Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                asm.push_str(&format!("    ; Variable declaration: {}\n", name));
                
                let offset = self.allocate_variable_sp_relative(&name);
                
                let value_code = self.compile_expression(&value)?;
                asm.push_str(&value_code);
                
                asm.push_str(&format!("    mov [rsp + {}], rax\n", offset));
            }
            Statement::If { condition, then_block, elif_blocks, else_block, span: _ } => {
                let label_id = self.get_next_label_id();
                let else_label = format!("if_else_{}", label_id);
                let end_label = format!("if_end_{}", label_id);
                
                asm.push_str("    ; If condition\n");
                let cond_code = self.compile_expression(&condition)?;
                asm.push_str(&cond_code);
                
                asm.push_str("    test rax, rax\n");
                asm.push_str(&format!("    jz {}\n", else_label));
                
                asm.push_str("    ; Then block\n");
                for stmt in then_block {
                    match stmt {
                        Statement::Expr(expr) => {
                            let expr_code = self.compile_expression(&expr)?;
                            asm.push_str(&expr_code);
                        }
                        _ => {}
                    }
                }
                asm.push_str(&format!("    jmp {}\n", end_label));
                
                // Use a reference to iterate
                for (elif_cond, elif_body) in elif_blocks {
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
            Statement::FunctionDef { name, .. } => {
                asm.push_str(&format!("    ; Function definition skipped: {}\n", name));
            }
            _ => {
                asm.push_str("    ; [Statement type not implemented]\n");
            }
        }
    }
    
    asm.push_str("    ; Return from main\n");
    asm.push_str("    mov rax, 0\n");
    asm.push_str("    mov rsp, rbp\n");
    asm.push_str("    pop rbp\n");
    asm.push_str("    ret\n\n");
    
    asm.push_str(&self.generate_helper_function());
    
    asm.push_str("    section .data\n");
    asm.push_str("newline:\n");
    asm.push_str("    db 10, 0\n\n");
    
    asm.push_str("; String literals\n");
    asm.push_str(&self.generate_string_data());
    
    Ok(asm)
}
    
    fn compile_expression(&mut self, expr: &Expr) -> Result<String, String> {
    match expr {
        Expr::Number(n, _) => {
            Ok(format!("    ; Number: {}\n    mov rax, {}\n", n, n))
        }
        Expr::String(s, _) => {
            let label = self.get_string_label(s);
            Ok(format!("    ; String: '{}'\n    lea rax, [{}]\n", s, label))
        }
        Expr::Var(_name, _) => {
            if let Some(offset) = self.get_variable_offset_sp_relative(_name) {
                Ok(format!("    ; Variable: {}\n    mov rax, [rsp + {}]\n", _name, offset))
            } else {
                Err(format!("Undefined variable: {}", _name))
            }
        }
        Expr::Call { func, args, kwargs: _, span: _ } if func == "print" => {
            let mut code = String::new();
            
            for arg in args {
                match arg {
                    Expr::String(s, _) => {
                        let label = self.get_string_label(s);
                        code.push_str(&format!("    ; String: '{}'\n", s));
                        code.push_str(&format!("    lea rdi, [{}]\n", label));
                        code.push_str("    call print_string\n");
                        code.push_str("    call print_newline\n");
                    }
                    Expr::Number(n, _) => {
                        code.push_str(&format!("    ; Number: {}\n", n));
                        code.push_str(&format!("    mov rax, {}\n", n));
                        code.push_str("    call print_decimal\n");
                    }
                    Expr::Var(_name, _) => {
                        if let Some(offset) = self.get_variable_offset_sp_relative(_name) {
                            code.push_str(&format!("    ; Variable: {}\n", _name));
                            code.push_str(&format!("    mov rax, [rsp + {}]\n", offset));
                            code.push_str("    call print_decimal\n");
                        } else {
                            return Err(format!("Undefined variable: {}", _name));
                        }
                    }
                    _ => {
                        return Err(format!("Unsupported argument type in print: {:?}", arg));
                    }
                }
            }
            
            Ok(code)
        }
        Expr::BinOp { left, op, right, span: _ } => {
            let mut code = String::new();
            code.push_str("    ; Binary operation\n");
            
            fn get_value_code(backend: &Linux64Backend, expr: &Expr) -> Result<String, String> {
                match expr {
                    Expr::Number(n, _) => Ok(format!("    mov rax, {}\n", n)),
                    Expr::Var(_name, _) => {
                        if let Some(offset) = backend.get_variable_offset_sp_relative(_name) {
                            Ok(format!("    mov rax, [rsp + {}]\n", offset))
                        } else {
                            Err(format!("Undefined variable: {}", _name))
                        }
                    }
                    _ => Err("Unsupported expression type".to_string()),
                }
            }
            
            code.push_str(&get_value_code(self, left)?);
            code.push_str("    push rax\n");
            
            code.push_str(&get_value_code(self, right)?);
            code.push_str("    mov rbx, rax\n");
            code.push_str("    pop rax\n");
            
            match op {
                Op::Add => code.push_str("    add rax, rbx\n"),
                Op::Sub => code.push_str("    sub rax, rbx\n"),
                Op::Mul => code.push_str("    imul rax, rbx\n"),
                Op::Div => {
                    code.push_str("    xor rdx, rdx\n");
                    code.push_str("    div rbx\n");
                }
                _ => return Err(format!("Unsupported operator: {:?}", op)),
            }
            
            Ok(code)
        }
        Expr::Compare { left, ops, comparators, span: _ } if ops.len() == 1 => {
            let mut code = String::new();
            code.push_str("    ; Comparison operation\n");
            
            match left.as_ref() {
                Expr::Var(_name, _) => {
                    if let Some(offset) = self.get_variable_offset_sp_relative(_name) {
                        code.push_str(&format!("    mov rax, [rsp + {}]\n", offset));
                    } else {
                        return Err(format!("Undefined variable: {}", _name));
                    }
                }
                Expr::Number(n, _) => {
                    code.push_str(&format!("    mov rax, {}\n", n));
                }
                _ => return Err("Unsupported left operand in comparison".to_string()),
            }
            
            code.push_str("    push rax\n");
            
            if let Some(right_expr) = comparators.get(0) {
                match right_expr {
                    Expr::Var(_name, _) => {
                        if let Some(offset) = self.get_variable_offset_sp_relative(_name) {
                            code.push_str(&format!("    mov rbx, [rsp + {}]\n", offset));
                        } else {
                            return Err(format!("Undefined variable: {}", _name));
                        }
                    }
                    Expr::Number(n, _) => {
                        code.push_str(&format!("    mov rbx, {}\n", n));
                    }
                    _ => return Err("Unsupported right operand in comparison".to_string()),
                }
            }
            
            code.push_str("    pop rax\n");
            
            match ops[0] {
                CompareOp::Lt => {
                    code.push_str("    cmp rax, rbx\n");
                    code.push_str("    setl al\n");
                    code.push_str("    movzx rax, al\n");
                }
                _ => return Err("Unsupported comparison operator".to_string()),
            }
            
            Ok(code)
        }
        _ => {
            Err(format!("Unsupported expression: {:?}", expr))
        }
    }
}

    
    fn function_prologue(&self, func: &BackendFunction) -> String {
        let mut prologue = String::new();
        prologue.push_str(&format!("{}:\n", func.name));
        prologue.push_str("    push rbp\n");
        prologue.push_str("    mov rbp, rsp\n");
        
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