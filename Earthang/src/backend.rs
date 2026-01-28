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
use crate::parser::{Program, Statement, Expr, Op, CompareOp};
use crate::dsl::{HardwareDSL, DeviceType};
use std::collections::HashMap;
use std::cell::RefCell;
use std::any::Any;

// Target platforms
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Target {
    Linux64,
}

// Capabilities for backend selection
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Capability {
    // Architecture
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
        // Since we only have Linux64 target, remove the match
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
        
        // Only register Linux64 backend
        registry.register(Box::new(Linux64Backend::new()));
        
        registry
    }
}

pub struct Linux64Backend {
    string_counter: RefCell<u32>,
    string_literals: RefCell<HashMap<String, String>>,
    symbol_table: RefCell<HashMap<String, VariableInfo>>,
    current_stack_offset: RefCell<i32>,
    label_counter: RefCell<u32>,
    hardware_dsl: RefCell<Option<HardwareDSL>>, // Changed to RefCell<Option<HardwareDSL>>
}

impl Linux64Backend {
    pub fn new() -> Self {
        Self {
            string_counter: RefCell::new(0),
            string_literals: RefCell::new(HashMap::new()),
            symbol_table: RefCell::new(HashMap::new()),
            current_stack_offset: RefCell::new(0),
            label_counter: RefCell::new(0),
            hardware_dsl: RefCell::new(None), // Initialize as None in RefCell
        }
    }

    pub fn with_hardware_dsl(mut self, dsl: HardwareDSL) -> Self {
        self.hardware_dsl = RefCell::new(Some(dsl));
        self
    }

    // RBP-RELATIVE ADDRESSING (FIXED VERSION)
    fn allocate_variable_rbp_relative(&self, name: &str) -> i32 {
        let mut offset = self.current_stack_offset.borrow_mut();
        
        // First variable at [rbp - 8], second at [rbp - 16], etc.
        *offset -= 8;
        let current = *offset;  // This is NEGATIVE (e.g., -8, -16, -24)
        
        self.symbol_table.borrow_mut().insert(name.to_string(), VariableInfo {
            name: name.to_string(),
            offset: current,
            type_hint: Some("int".to_string()),
        });
        
        current
    }
    
    fn ensure_variable_exists_rbp_relative(&self, name: &str) -> i32 {
        if let Some(var_info) = self.symbol_table.borrow().get(name) {
            var_info.offset
        } else {
            self.allocate_variable_rbp_relative(name)
        }
    }
    
    fn get_variable_offset_rbp_relative(&self, name: &str) -> Option<i32> {
        self.symbol_table.borrow().get(name).map(|v| v.offset)
    }
    
    fn get_absolute_offset(&self, offset: i32) -> i32 {
        if offset < 0 { -offset } else { offset }
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
        data.push_str(&format!("    .asciz \"{}\"\n", content.replace("\"", "\\\"")));
    }
    data
}
    
    fn get_next_label_id(&self) -> u32 {
        let mut counter = self.label_counter.borrow_mut();
        let id = *counter;
        *counter += 1;
        id
    }
    
    fn generate_helper_function(&self) -> String {
    let mut helpers = String::new();
    
    helpers.push_str("print_string:\n");
    helpers.push_str("    push rax\n");
    helpers.push_str("    push rdi\n");
    helpers.push_str("    push rsi\n");
    helpers.push_str("    push rdx\n");
    helpers.push_str("    #\n");
    helpers.push_str("    mov rsi, rdi\n");
    helpers.push_str("    xor rdx, rdx\n");
    helpers.push_str(".count_loop:\n");
    helpers.push_str("    cmp BYTE PTR [rsi + rdx], 0\n");
    helpers.push_str("    je .count_done\n");
    helpers.push_str("    inc rdx\n");
    helpers.push_str("    jmp .count_loop\n");
    helpers.push_str(".count_done:\n");
    helpers.push_str("    #\n");
    helpers.push_str("    mov rax, 1\n");
    helpers.push_str("    mov rdi, 1\n");
    helpers.push_str("    syscall\n");
    helpers.push_str("    #\n");
    helpers.push_str("    pop rdx\n");
    helpers.push_str("    pop rsi\n");
    helpers.push_str("    pop rdi\n");
    helpers.push_str("    pop rax\n");
    helpers.push_str("    ret\n\n");

    helpers.push_str("print_decimal:\n");
    helpers.push_str("    # Input: rax = integer\n");
    helpers.push_str("    push rbp\n");
    helpers.push_str("    mov rbp, rsp\n");
    helpers.push_str("    sub rsp, 32\n");
    helpers.push_str("    #\n");
    helpers.push_str("    push rbx\n");
    helpers.push_str("    push rcx\n");
    helpers.push_str("    push rdx\n");
    helpers.push_str("    push rsi\n");
    helpers.push_str("    push rdi\n");
    helpers.push_str("    #\n");
    helpers.push_str("    mov QWORD PTR [rbp - 8], rax\n");
    helpers.push_str("    #\n");
    helpers.push_str("    lea rdi, [rsp + 31]\n");
    helpers.push_str("    mov BYTE PTR [rdi], 0\n");
    helpers.push_str("    #\n");
    helpers.push_str("    mov rax, QWORD PTR [rbp - 8]\n");
    helpers.push_str("    test rax, rax\n");
    helpers.push_str("    jns .positive\n");
    helpers.push_str("    neg rax\n");
    helpers.push_str("    #\n");
    helpers.push_str(".positive:\n");
    helpers.push_str("    mov rbx, 10\n");
    helpers.push_str("    #\n");
    helpers.push_str(".convert_loop:\n");
    helpers.push_str("    xor rdx, rdx\n");
    helpers.push_str("    div rbx\n");
    helpers.push_str("    add dl, '0'\n");
    helpers.push_str("    dec rdi\n");
    helpers.push_str("    mov BYTE PTR [rdi], dl\n");
    helpers.push_str("    test rax, rax\n");
    helpers.push_str("    jnz .convert_loop\n");
    helpers.push_str("    #\n");
    helpers.push_str("    mov rax, QWORD PTR [rbp - 8]\n");
    helpers.push_str("    test rax, rax\n");
    helpers.push_str("    jns .print_it\n");
    helpers.push_str("    dec rdi\n");
    helpers.push_str("    mov BYTE PTR [rdi], '-'\n");
    helpers.push_str("    #\n");
    helpers.push_str(".print_it:\n");
    helpers.push_str("    lea rsi, [rsp + 32]\n");
    helpers.push_str("    sub rsi, rdi\n");
    helpers.push_str("    #\n");
    helpers.push_str("    mov rax, 1\n");
    helpers.push_str("    mov rdx, rsi\n");
    helpers.push_str("    mov rsi, rdi\n");
    helpers.push_str("    mov rdi, 1\n");
    helpers.push_str("    syscall\n");
    helpers.push_str("    #\n");
    helpers.push_str("    mov rax, 1\n");
    helpers.push_str("    mov rdi, 1\n");
    helpers.push_str("    lea rsi, [newline]\n");
    helpers.push_str("    mov rdx, 1\n");
    helpers.push_str("    syscall\n");
    helpers.push_str("    #\n");
    helpers.push_str("    pop rdi\n");
    helpers.push_str("    pop rsi\n");
    helpers.push_str("    pop rdx\n");
    helpers.push_str("    pop rcx\n");
    helpers.push_str("    pop rbx\n");
    helpers.push_str("    #\n");
    helpers.push_str("    mov rsp, rbp\n");
    helpers.push_str("    pop rbp\n");
    helpers.push_str("    ret\n\n");
    
    helpers.push_str("print_newline:\n");
    helpers.push_str("    push rax\n");
    helpers.push_str("    push rdi\n");
    helpers.push_str("    push rsi\n");
    helpers.push_str("    push rdx\n");
    helpers.push_str("    #\n");
    helpers.push_str("    mov rax, 1\n");
    helpers.push_str("    mov rdi, 1\n");
    helpers.push_str("    lea rsi, [newline]\n");
    helpers.push_str("    mov rdx, 1\n");
    helpers.push_str("    syscall\n");
    helpers.push_str("    #\n");
    helpers.push_str("    pop rdx\n");
    helpers.push_str("    pop rsi\n");
    helpers.push_str("    pop rdi\n");
    helpers.push_str("    pop rax\n");
    helpers.push_str("    ret\n");
    
    helpers
}
    
    fn compile_statement_in_context(&mut self, stmt: &Statement) -> Result<String, String> {
    let mut code = String::new();
    
    match stmt {
        Statement::Expr(expr) => {
            code.push_str(&self.compile_expression(&expr)?);
        }
        Statement::VarDecl { name, value, type_hint: _, span: _ } => {
            code.push_str(&format!("    # Variable declaration: {}\n", name));
            let offset = self.allocate_variable_rbp_relative(&name);
            let value_code = self.compile_expression(&value)?;
            code.push_str(&value_code);
            let abs_offset = self.get_absolute_offset(offset);
            code.push_str(&format!("    mov QWORD PTR [rbp - {}], rax\n", abs_offset));
        }
        Statement::Assign { target, value, span: _ } => {
            code.push_str(&format!("    # Assignment to {}\n", target));
            let offset = self.ensure_variable_exists_rbp_relative(&target);
            let value_code = self.compile_expression(&value)?;
            code.push_str(&value_code);
            let abs_offset = self.get_absolute_offset(offset);
            code.push_str(&format!("    mov QWORD PTR [rbp - {}], rax\n", abs_offset));
        }
        Statement::AugAssign { target, op, value, span: _ } => {
            code.push_str(&format!("    # Augmented assignment to {}\n", target));
            let offset = self.ensure_variable_exists_rbp_relative(&target);
            let abs_offset = self.get_absolute_offset(offset);
            
            // Load current value
            code.push_str(&format!("    mov rax, QWORD PTR [rbp - {}]\n", abs_offset));
            code.push_str("    push rax\n");
            
            // Compile right-hand side
            let value_code = self.compile_expression(&value)?;
            code.push_str(&value_code);
            
            code.push_str("    mov rbx, rax\n");
            code.push_str("    pop rax\n");
            
            // Perform operation
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
                _ => return Err(format!("Unsupported augmented assignment operator: {:?}", op)),
            }
            
            // Store back
            code.push_str(&format!("    mov QWORD PTR [rbp - {}], rax\n", abs_offset));
        }
        Statement::HardwareFunctionDef { device, name, args: _, body, span: _ } => {
            // Compile hardware function using DSL
            code.push_str(&format!("    # Hardware function: {} for device {}\n", name, device));
            
            // Generate prologue first
            let prologue = if let Some(ref dsl) = *self.hardware_dsl.borrow() {
                dsl.generate_device_function_prologue(&device, &name)
                    .unwrap_or_else(|e| format!("; Error generating prologue: {}\n", e))
            } else {
                String::from("    ; Hardware DSL not available\n")
            };
            code.push_str(&prologue);
            
            // Compile body statements
            for body_stmt in body {
                code.push_str(&self.compile_statement_in_context(body_stmt)?);
            }
            
            // Generate epilogue
            let epilogue = if let Some(ref dsl) = *self.hardware_dsl.borrow() {
                dsl.generate_device_function_epilogue(&device)
                    .unwrap_or_else(|e| format!("; Error generating epilogue: {}\n", e))
            } else {
                String::from("    ; Hardware DSL not available\n")
            };
            code.push_str(&epilogue);
        }
        Statement::HardwareDecl { device: _, config: _, span: _ } => {
            // Handle hardware declaration
            code.push_str("    # Hardware declaration (ignored in context)\n");
        }
        _ => {
            code.push_str(&format!("    # [Statement type not handled in context: {:?}]\n", stmt));
        }
    }
    
    Ok(code)
    }
}

impl Backend for Linux64Backend {
    fn name(&self) -> &str {
        "linux64"
    }
    
    fn generate_header(&self) -> String {
    String::from("    .intel_syntax noprefix
    .section .text
    .globl _start

_start:
    mov rbp, rsp
    and rsp, -16        # 16-byte align stack
    call main
    mov rdi, rax        # exit code
    mov rax, 60         # syscall: exit
    syscall\n\n")
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

    // GAS directives for Intel syntax
    asm.push_str("    .intel_syntax noprefix\n");
    asm.push_str("    .section .text\n");
    asm.push_str("    .globl _start\n\n");
    
    asm.push_str("_start:\n");
    asm.push_str("    mov rbp, rsp\n");
    asm.push_str("    and rsp, -16        # 16-byte align stack\n");
    asm.push_str("    \n");
    asm.push_str("    call main\n");
    asm.push_str("    \n");
    asm.push_str("    mov rdi, rax        # exit code\n");
    asm.push_str("    mov rax, 60         # syscall: exit\n");
    asm.push_str("    syscall\n\n");
    
    asm.push_str("main:\n");
    asm.push_str("    push rbp\n");
    asm.push_str("    mov rbp, rsp\n");
    
    // Allocate variables and track maximum negative offset
    {
        let mut offset = self.current_stack_offset.borrow_mut();
        *offset = 0;  // RBP itself is at offset 0
        self.symbol_table.borrow_mut().clear();
    }
    
    // Track the most negative offset need
    let mut max_negative_offset = 0;
    
    // Walk through program to allocate all variables
    for stmt in &program.body {
        match stmt {
            Statement::VarDecl { name, .. } => {
                let offset = self.allocate_variable_rbp_relative(name);
                if offset < max_negative_offset {
                    max_negative_offset = offset;
                }
            }
            Statement::Assign { target, .. } => {
                let offset = self.ensure_variable_exists_rbp_relative(target);
                if offset < max_negative_offset {
                    max_negative_offset = offset;
                }
            }
            Statement::AugAssign { target, .. } => {
                let offset = self.ensure_variable_exists_rbp_relative(target);
                if offset < max_negative_offset {
                    max_negative_offset = offset;
                }
            }
            _ => {}
        }
    }
    
    // Allocate stack space based on the most negative offset
    // Since offsets are negative, need to allocate -max_negative_offset bytes
    if max_negative_offset < 0 {
        let stack_space = (-max_negative_offset + 15) & !15;
        asm.push_str(&format!("    sub rsp, {}        # Allocate {} bytes for locals\n", stack_space, stack_space));
        asm.push_str(&format!("    # Variables span from [rbp - 8] to [rbp - {}]\n", -max_negative_offset));
    }
    
    asm.push_str("\n");
    
    for stmt in &program.body {
        match stmt {
            Statement::Expr(expr) => {
                let expr_code = self.compile_expression(expr)?;
                asm.push_str(&expr_code);
            }
            Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                if let Some(offset) = self.get_variable_offset_rbp_relative(name) {
                    // Compile the value
                    let value_code = self.compile_expression(value)?;
                    asm.push_str(&value_code);
                    
                    // Store to stack at [rbp - abs_offset]
                    let abs_offset = self.get_absolute_offset(offset);
                    asm.push_str(&format!("    mov QWORD PTR [rbp - {}], rax\n", abs_offset));
                } else {
                    return Err(format!("Variable {} not allocated", name));
                }
            }
            Statement::Assign { target, value, span: _ } => {
                asm.push_str(&format!("    # Assignment to {}\n", target));
                
                if let Some(offset) = self.get_variable_offset_rbp_relative(target) {
                    let value_code = self.compile_expression(value)?;
                    asm.push_str(&value_code);
                    
                    let abs_offset = self.get_absolute_offset(offset);
                    asm.push_str(&format!("    mov QWORD PTR [rbp - {}], rax\n", abs_offset));
                } else {
                    return Err(format!("Variable {} not found", target));
                }
            }
            Statement::AugAssign { target, op, value, span: _ } => {
                if let Some(offset) = self.get_variable_offset_rbp_relative(target) {
                    let abs_offset = self.get_absolute_offset(offset);
                    
                    asm.push_str(&format!("    mov rax, QWORD PTR [rbp - {}]\n", abs_offset));
                    asm.push_str("    push rax\n");
                    
                    // Compile right-hand side
                    let value_code = self.compile_expression(value)?;
                    asm.push_str(&value_code);
                    
                    asm.push_str("    mov rbx, rax\n");
                    asm.push_str("    pop rax\n");
                    
                    // Perform operation
                    match op {
                        Op::Add => asm.push_str("    add rax, rbx\n"),
                        Op::Sub => asm.push_str("    sub rax, rbx\n"),
                        Op::Mul => asm.push_str("    imul rax, rbx\n"),
                        Op::Div => {
                            asm.push_str("    xor rdx, rdx\n");
                            asm.push_str("    idiv rbx\n");
                        }
                        Op::Mod => {
                            asm.push_str("    xor rdx, rdx\n");
                            asm.push_str("    div rbx\n");
                            asm.push_str("    mov rax, rdx\n");
                        }
                        _ => return Err(format!("Unsupported augmented assignment operator: {:?}", op)),
                    }
                    
                    // Store back
                    asm.push_str(&format!("    mov QWORD PTR [rbp - {}], rax\n", abs_offset));
                } else {
                    return Err(format!("Variable {} not found", target));
                }
            }
            Statement::If { condition, then_block, elif_blocks, else_block, span: _ } => {
                let label_id = self.get_next_label_id();
                let else_label = format!("if_else_{}", label_id);
                let end_label = format!("if_end_{}", label_id);
                
                asm.push_str("    # If condition\n");
                let cond_code = self.compile_expression(condition)?;
                asm.push_str(&cond_code);
                
                asm.push_str("    test rax, rax\n");
                asm.push_str(&format!("    jz {}\n", else_label));
                
                asm.push_str("    # Then block\n");
                for stmt in then_block {
                    let stmt_code = self.compile_statement_in_context(stmt)?;
                    asm.push_str(&stmt_code);
                }
                asm.push_str(&format!("    jmp {}\n", end_label));
                
                // Process elif blocks
                for (elif_cond, elif_body) in elif_blocks {
                    asm.push_str(&format!("{}:\n", else_label));
                    let elif_cond_code = self.compile_expression(elif_cond)?;
                    asm.push_str(&elif_cond_code);
                    asm.push_str("    test rax, rax\n");
                    asm.push_str(&format!("    jz {}_elif\n", else_label));
                    
                    asm.push_str("    # Elif body\n");
                    for stmt in elif_body {
                        let stmt_code = self.compile_statement_in_context(stmt)?;
                        asm.push_str(&stmt_code);
                    }
                    asm.push_str(&format!("    jmp {}\n", end_label));
                    asm.push_str(&format!("{}_elif:\n", else_label));
                }
                
                // Process else block
                if let Some(else_body) = else_block {
                    if elif_blocks.is_empty() {
                        asm.push_str(&format!("{}:\n", else_label));
                    }
                    
                    asm.push_str("    # Else block\n");
                    for stmt in else_body {
                        let stmt_code = self.compile_statement_in_context(stmt)?;
                        asm.push_str(&stmt_code);
                    }
                } else if elif_blocks.is_empty() {
                    asm.push_str(&format!("{}:\n", else_label));
                }
                
                asm.push_str(&format!("{}:\n", end_label));
            }
            Statement::While { condition, body, orelse: _, span: _ } => {
                let label_id = self.get_next_label_id();
                let while_start = format!("while_start_{}", label_id);
                let while_end = format!("while_end_{}", label_id);
                
                asm.push_str("    # While loop\n");
                asm.push_str(&format!("{}:\n", while_start));
                
                // Compile condition
                let cond_code = self.compile_expression(condition)?;
                asm.push_str(&cond_code);
                
                asm.push_str("    test rax, rax\n");
                asm.push_str(&format!("    jz {}\n", while_end));
                
                asm.push_str("    # While body\n");
                for stmt in body {
                    let stmt_code = self.compile_statement_in_context(stmt)?;
                    asm.push_str(&stmt_code);
                }
                
                // Jump back to start
                asm.push_str(&format!("    jmp {}\n", while_start));
                
                // End label
                asm.push_str(&format!("{}:\n", while_end));
            }
            Statement::FunctionDef { name, args, body, span: _ } => {
                // Skip function compilation for now
                asm.push_str(&format!("    # Function definition: {}\n", name));
            }
            Statement::HardwareFunctionDef { device, name, args: _, body, span: _ } => {
                // Handle hardware function definition
                asm.push_str(&format!("    # Hardware function: {} for device {}\n", name, device));
                
                // Generate prologue first
                let prologue = if let Some(ref dsl) = *self.hardware_dsl.borrow() {
                    dsl.generate_device_function_prologue(&device, &name)
                        .unwrap_or_else(|e| format!("; Error generating prologue: {}\n", e))
                } else {
                    String::from("    ; Hardware DSL not available\n")
                };
                asm.push_str(&prologue);
                
                // Compile body statements
                for body_stmt in body {
                    let stmt_code = self.compile_statement_in_context(body_stmt)?;
                    asm.push_str(&stmt_code);
                }
                
                // Generate epilogue
                let epilogue = if let Some(ref dsl) = *self.hardware_dsl.borrow() {
                    dsl.generate_device_function_epilogue(&device)
                        .unwrap_or_else(|e| format!("; Error generating epilogue: {}\n", e))
                } else {
                    String::from("    ; Hardware DSL not available\n")
                };
                asm.push_str(&epilogue);
            }
            Statement::HardwareDecl { device, config: _, span: _ } => {
                // Handle hardware declaration
                asm.push_str(&format!("    # Hardware declaration: {}\n", device));
                // We might need to generate configuration code here
                // For now, just ignore
            }
            Statement::Return(expr) => {
                asm.push_str("    # Return statement\n");
                if let Some(expr) = expr {
                    let expr_code = self.compile_expression(expr)?;
                    asm.push_str(&expr_code);
                } else {
                    asm.push_str("    xor rax, rax\n");
                }
                asm.push_str("    jmp .main_epilogue\n");
            }
            Statement::Pass => asm.push_str("    # pass\n"),
            Statement::Break => asm.push_str("    # break\n"),
            Statement::Continue => asm.push_str("    # continue\n"),
            Statement::Include { filename, span: _ } => {
                asm.push_str(&format!("    # Include: {}\n", filename));
            }
        }
    }
    
    // Main function epilogue
    asm.push_str("\n.main_epilogue:\n");
    asm.push_str("    mov rsp, rbp\n");
    asm.push_str("    pop rbp\n");
    asm.push_str("    ret\n\n");
    
    // Generate helper functions
    asm.push_str(&self.generate_helper_function());
    
    // Generate hardware library if DSL is available
    if let Some(ref dsl) = *self.hardware_dsl.borrow() {
        asm.push_str("\n; ========== HARDWARE SUPPORT ==========\n");
        asm.push_str(&dsl.generate_hardware_library());
    }
    
    // Generate data section
    asm.push_str("    .section .data\n");
    asm.push_str("newline:\n");
    asm.push_str("    .byte 10, 0\n\n");
    
    asm.push_str("# String literals\n");
    asm.push_str(&self.generate_string_data());
    
    // Add AT&T syntax directive for compatibility
    asm.push_str("\n    .att_syntax\n");
    
    Ok(asm)
}
    
    fn compile_expression(&mut self, expr: &Expr) -> Result<String, String> {
    match expr {
        Expr::Number(n, _) => {
            Ok(format!("    # Number: {}\n    mov rax, {}\n", n, n))
        }
        Expr::String(s, _) => {
            let label = self.get_string_label(s);
            Ok(format!("    # String: '{}'\n    lea rax, [{}]\n", s, label))
        }
        Expr::Var(name, _) => {
            // Use RBP-relative addressing ONLY
            if let Some(offset) = self.get_variable_offset_rbp_relative(name) {
                let abs_offset = self.get_absolute_offset(offset);
                Ok(format!("    # Variable: {} at [rbp - {}]\n    mov rax, QWORD PTR [rbp - {}]\n", 
                           name, abs_offset, abs_offset))
            } else {
                Err(format!("Undefined variable: {}", name))
            }
        }
        Expr::Call { func, args, kwargs: _, span: _ } if func == "print" => {
            let mut code = String::new();
            
            for arg in args {
                match arg {
                    Expr::String(s, _) => {
                        let label = self.get_string_label(s);
                        code.push_str(&format!("    # String: '{}'\n", s));
                        code.push_str(&format!("    lea rdi, [{}]\n", label));
                        code.push_str("    call print_string\n");
                        code.push_str("    call print_newline\n");
                    }
                    Expr::Number(n, _) => {
                        code.push_str(&format!("    # Number: {}\n", n));
                        code.push_str(&format!("    mov rax, {}\n", n));
                        code.push_str("    call print_decimal\n");
                    }
                    Expr::Var(name, _) => {
                        if let Some(offset) = self.get_variable_offset_rbp_relative(name) {
                            let abs_offset = self.get_absolute_offset(offset);
                            code.push_str(&format!("    # Variable: {}\n", name));
                            code.push_str(&format!("    mov rax, QWORD PTR [rbp - {}]\n", abs_offset));
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
            
            Ok(code)
        }
        Expr::Call { func, args, kwargs: _, span: _ } if func.starts_with("hw_") || 
                                                          func == "write_register" || 
                                                          func == "read_register" ||
                                                          func == "dma_transfer" ||
                                                          func == "port_in" ||
                                                          func == "port_out" => {
            // Handle hardware intrinsics
            let mut code = String::new();
            code.push_str(&format!("    # Hardware intrinsic: {}\n", func));
            
            if let Some(ref mut dsl) = *self.hardware_dsl.borrow_mut() {
                // Convert arguments to strings for DSL parsing
                let arg_strings: Vec<String> = args.iter().map(|arg| {
                    match arg {
                        Expr::Number(n, _) => n.to_string(),
                        Expr::String(s, _) => format!("\"{}\"", s),
                        Expr::Var(name, _) => name.clone(),
                        _ => "0".to_string(),
                    }
                }).collect();
                
                let intrinsic = if !arg_strings.is_empty() {
                    format!("{}({})", func, arg_strings.join(", "))
                } else {
                    func.clone()
                };
                
                match dsl.parse_hardware_statement(&intrinsic) {
                    Ok(asm_lines) => {
                        for line in asm_lines {
                            // Adapt 16/32-bit assembly to 64-bit
                            let adapted_line = line
                                .replace("ax", "rax")
                                .replace("bx", "rbx")
                                .replace("cx", "rcx")
                                .replace("dx", "rdx")
                                .replace("si", "rsi")
                                .replace("di", "rdi");
                            code.push_str(&format!("    {}\n", adapted_line));
                        }
                        Ok(code)
                    }
                    Err(e) => Err(format!("Hardware DSL error: {}", e)),
                }
            } else {
                Err("Hardware DSL not available for hardware intrinsic".to_string())
            }
        }
        Expr::Call { func, args, kwargs: _, span: _ } => {
            // General function call
            let mut code = String::new();
            
            code.push_str(&format!("    # Function call: {}\n", func));
            
            // Process arguments
            for (i, arg) in args.iter().enumerate() {
                let arg_code = self.compile_expression(arg)?;
                code.push_str(&arg_code);
                
                // Move to appropriate register (System V ABI)
                match i {
                    0 => code.push_str("    mov rdi, rax\n"),
                    1 => code.push_str("    mov rsi, rax\n"),
                    2 => code.push_str("    mov rdx, rax\n"),
                    3 => code.push_str("    mov rcx, rax\n"),
                    4 => code.push_str("    mov r8, rax\n"),
                    5 => code.push_str("    mov r9, rax\n"),
                    _ => code.push_str("    push rax\n"),
                }
            }
            
            // Align stack if needed
            if args.len() > 6 {
                let extra_args = args.len() - 6;
                if extra_args % 2 != 0 {
                    code.push_str("    sub rsp, 8\n");
                }
            }
            
            // Call the function
            code.push_str(&format!("    call {}\n", func));
            
            // Clean up stack
            if args.len() > 6 {
                let extra_args = args.len() - 6;
                let stack_adjust = extra_args * 8;
                if extra_args % 2 != 0 {
                    code.push_str(&format!("    add rsp, {}\n", stack_adjust + 8));
                } else {
                    code.push_str(&format!("    add rsp, {}\n", stack_adjust));
                }
            }
            
            Ok(code)
        }
        Expr::BinOp { left, op, right, span: _ } => {
            let mut code = String::new();
            code.push_str("    # Binary operation\n");
            
            // Compile left operand
            let left_code = self.compile_expression(left)?;
            code.push_str(&left_code);
            code.push_str("    push rax\n");
            
            // Compile right operand
            let right_code = self.compile_expression(right)?;
            code.push_str(&right_code);
            code.push_str("    mov rbx, rax\n");
            code.push_str("    pop rax\n");
            
            // Perform operation
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
                Op::BitAnd => code.push_str("    and rax, rbx\n"),
                Op::BitOr => code.push_str("    or rax, rbx\n"),
                Op::BitXor => code.push_str("    xor rax, rbx\n"),
                _ => return Err(format!("Unsupported operator: {:?}", op)),
            }
            
            Ok(code)
        }
        Expr::Compare { left, ops, comparators, span: _ } if ops.len() == 1 => {
            let mut code = String::new();
            code.push_str("    # Comparison operation\n");
            
            // Compile left operand
            let left_code = self.compile_expression(left)?;
            code.push_str(&left_code);
            code.push_str("    push rax\n");
            
            if let Some(right_expr) = comparators.get(0) {
                let right_code = self.compile_expression(right_expr)?;
                code.push_str(&right_code);
                code.push_str("    mov rbx, rax\n");
                code.push_str("    pop rax\n");
                
                code.push_str("    cmp rax, rbx\n");
                
                match ops[0] {
                    CompareOp::Lt => {
                        code.push_str("    setl al\n");
                        code.push_str("    movzx rax, al\n");
                    }
                    CompareOp::Gt => {
                        code.push_str("    setg al\n");
                        code.push_str("    movzx rax, al\n");
                    }
                    CompareOp::Eq => {
                        code.push_str("    sete al\n");
                        code.push_str("    movzx rax, al\n");
                    }
                    CompareOp::Ne => {
                        code.push_str("    setne al\n");
                        code.push_str("    movzx rax, al\n");
                    }
                    CompareOp::Le => {
                        code.push_str("    setle al\n");
                        code.push_str("    movzx rax, al\n");
                    }
                    CompareOp::Ge => {
                        code.push_str("    setge al\n");
                        code.push_str("    movzx rax, al\n");
                    }
                    CompareOp::In | CompareOp::NotIn | CompareOp::Is | CompareOp::IsNot => {
                        return Err(format!("Comparison operator {:?} not supported", ops[0]));
                    }
                }
            } else {
                code.push_str("    pop rax\n");
                return Err("Missing comparator".to_string());
            }
            
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