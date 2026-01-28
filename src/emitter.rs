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
use std::collections::HashMap;
use crate::parser::{Program, Statement, Expr, CompareOp};

#[derive(Debug, Clone, PartialEq)]
pub enum TargetPlatform {
    Linux64,
}

#[derive(Debug, Clone)]
pub struct TargetConfig {
    pub platform: TargetPlatform,
    pub bits: u8,
    pub format: &'static str,
    pub entry_point: &'static str,
}

impl TargetConfig {
    pub fn linux64() -> Self { Self { platform: TargetPlatform::Linux64, bits: 64, format: "elf64", entry_point: "_start" } }
    
    pub fn is_linux(&self) -> bool { self.platform == TargetPlatform::Linux64 }
}


pub struct NasmEmitter {
    pub target: TargetConfig,
    label_counter: u32,
    variable_offsets: HashMap<String, i32>,
    byte_counter: ByteCounter,
    string_literals: HashMap<String, String>, // Maps Earthang string to NASM label
    data_labels: Vec<String>, // Track data section labels
    hardware_mode: bool, // Enable hardware DSL mode
}

impl NasmEmitter {
    pub fn new() -> Self {
        Self {
            target: TargetConfig::linux64(),
            label_counter: 0,
            variable_offsets: HashMap::new(),
            byte_counter: ByteCounter::new(),
            string_literals: HashMap::new(),
            data_labels: Vec::new(),
            hardware_mode: false,
        }
    }
    
    // Target configuration methods
    pub fn set_target_linux(&mut self) { self.target = TargetConfig::linux64(); }
    
    pub fn enable_hardware_mode(&mut self, enable: bool) {
        self.hardware_mode = enable;
    }
    
    fn new_label(&mut self, prefix: &str) -> String {
        let label = format!("{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }
    
    // Collect all string literals from the program
    fn collect_string_literals(&mut self, program: &Program) {
        self.string_literals.clear();
        self.data_labels.clear();
        
        for stmt in &program.body {
            self.collect_strings_from_stmt(stmt);
        }
    }
    
    fn collect_strings_from_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Expr(expr) => self.collect_strings_from_expr(expr),
            Statement::VarDecl { value, .. } => self.collect_strings_from_expr(value),
            Statement::Assign { value, .. } => self.collect_strings_from_expr(value),
            Statement::FunctionDef { body, .. } => {
                for stmt in body {
                    self.collect_strings_from_stmt(stmt);
                }
            }
            Statement::HardwareFunctionDef { body, .. } => {
                for stmt in body {
                    self.collect_strings_from_stmt(stmt);
                }
            }
            Statement::If { condition, then_block, elif_blocks, else_block, .. } => {
                self.collect_strings_from_expr(condition);
                for stmt in then_block {
                    self.collect_strings_from_stmt(stmt);
                }
                for (cond, block) in elif_blocks {
                    self.collect_strings_from_expr(cond);
                    for stmt in block {
                        self.collect_strings_from_stmt(stmt);
                    }
                }
                if let Some(else_block) = else_block {
                    for stmt in else_block {
                        self.collect_strings_from_stmt(stmt);
                    }
                }
            }
            Statement::Return(value) => {
                if let Some(expr) = value {
                    self.collect_strings_from_expr(expr);
                }
            }
            _ => {}
        }
    }
    
    fn collect_strings_from_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::String(s, _) => {
                if !self.string_literals.contains_key(s) {
                    let label = self.new_label("str");
                    self.string_literals.insert(s.clone(), label.clone());
                    self.data_labels.push(label);
                }
            }
            Expr::Call { args, .. } => {
                for arg in args {
                    self.collect_strings_from_expr(arg);
                }
            }
            Expr::BinOp { left, right, .. } => {
                self.collect_strings_from_expr(left);
                self.collect_strings_from_expr(right);
            }
            Expr::UnaryOp { operand, .. } => {
                self.collect_strings_from_expr(operand);
            }
            _ => {}
        }
    }
    
    // Check if program has imports
    fn has_imports(&self, program: &Program) -> bool {
        program.body.iter().any(|stmt| {
            match stmt {
                Statement::Expr(Expr::Call { func, .. }) => func == "import",
                _ => false,
            }
        })
    }
    
    // Variable management methods
    fn allocate_variable(&mut self, name: &str) -> i32 {
        let offset = (self.variable_offsets.len() as i32 + 1) * 8;
        self.variable_offsets.insert(name.to_string(), offset);
        offset
    }
    
    fn get_variable_offset(&self, name: &str) -> Option<i32> {
        self.variable_offsets.get(name).copied()
    }
    
    fn count_variables(&self, program: &Program) -> usize {
        let mut count = 0;
        for stmt in &program.body {
            match stmt {
                Statement::VarDecl { name, .. } => {
                    if !self.variable_offsets.contains_key(name) {
                        count += 1;
                    }
                }
                Statement::Assign { target, .. } => {
                    if !self.variable_offsets.contains_key(target) {
                        count += 1;
                    }
                }
                _ => {}
            }
        }
        count
    }
    
    // Main compilation
    pub fn compile_program(&mut self, program: &Program) -> Result<String, String> {
        self.label_counter = 0;
        self.variable_offsets.clear();
        self.byte_counter.reset();
        self.collect_string_literals(program);
        
        // Check for imports first
        if self.has_imports(program) {
            return self.compile_with_imports(program);
        }
        
        self.compile_standard(program)
    }
    
    fn compile_linux64(&mut self, program: &Program) -> Result<String, String> {
    let mut code = String::new();
    
    code.push_str("; Earthang Linux 64-bit Executable\n");
    code.push_str("; Format: ELF64\n");
    if self.hardware_mode {
        code.push_str("; Hardware DSL: Enabled\n");
    }
    code.push_str("\n");
    
    code.push_str("section .text\n");
    code.push_str("    bits 64\n");
    code.push_str("    global _start\n\n");
    
    // FIXED: Proper entry point
    code.push_str("_start:\n");
    code.push_str("    ; Set up stack\n");
    code.push_str("    mov rbp, rsp\n");
    code.push_str("    and rsp, -16         ; Align stack to 16 bytes\n");
    code.push_str("    call main\n");
    code.push_str("    ; Exit after main returns\n");
    code.push_str("    mov rax, 60         ; sys_exit\n");
    code.push_str("    xor rdi, rdi        ; exit code 0\n");
    code.push_str("    syscall\n\n");
    
    // Main function
    code.push_str("main:\n");
    code.push_str("    push rbp\n");
    code.push_str("    mov rbp, rsp\n");
    code.push_str("    ; Linux syscall calling convention: rax=syscall#, rdi, rsi, rdx, r10, r8, r9\n\n");
    
    // Compile program statements
    self.compile_linux64_statements(&mut code, &program.body)?;
    
    code.push_str("    ; Return from main\n");
    code.push_str("    mov rsp, rbp\n");
    code.push_str("    pop rbp\n");
    code.push_str("    ret\n\n");
    
    // Linux syscall helpers
    code.push_str("; ========== LINUX HELPERS ==========\n\n");
    
    code.push_str("linux_print:\n");
    code.push_str("    ; RDI = string pointer\n");
    code.push_str("    push rdi\n");
    code.push_str("    push rsi\n");
    code.push_str("    push rdx\n");
    code.push_str("    push rax\n");
    code.push_str("    \n");
    code.push_str("    ; Calculate length\n");
    code.push_str("    mov rsi, rdi\n");
    code.push_str("    xor rdx, rdx\n");
    code.push_str(".len_loop:\n");
    code.push_str("    cmp byte [rsi + rdx], 0\n");
    code.push_str("    je .len_done\n");
    code.push_str("    inc rdx\n");
    code.push_str("    jmp .len_loop\n");
    code.push_str(".len_done:\n");
    code.push_str("    \n");
    code.push_str("    ; Write to stdout\n");
    code.push_str("    mov rax, 1          ; sys_write\n");
    code.push_str("    mov rdi, 1          ; fd = stdout\n");
    code.push_str("    ; rsi already set to buffer\n");
    code.push_str("    ; rdx already set to count\n");
    code.push_str("    syscall\n");
    code.push_str("    \n");
    code.push_str("    pop rax\n");
    code.push_str("    pop rdx\n");
    code.push_str("    pop rsi\n");
    code.push_str("    pop rdi\n");
    code.push_str("    ret\n\n");
    
    // Add hardware helper functions if in hardware mode
    if self.hardware_mode {
        code.push_str("; ========== HARDWARE HELPERS ==========\n\n");
        code.push_str("hardware_delay:\n");
        code.push_str("    ; Delay for hardware operations\n");
        code.push_str("    ; Input: RCX = delay count\n");
        code.push_str("    push rcx\n");
        code.push_str(".delay_loop:\n");
        code.push_str("    nop\n");
        code.push_str("    nop\n");
        code.push_str("    nop\n");
        code.push_str("    nop\n");
        code.push_str("    loop .delay_loop\n");
        code.push_str("    pop rcx\n");
        code.push_str("    ret\n\n");
        
        code.push_str("hardware_port_in_8:\n");
        code.push_str("    ; 8-bit port input\n");
        code.push_str("    ; Input: RDX = port\n");
        code.push_str("    ; Output: AL = value\n");
        code.push_str("    in al, dx\n");
        code.push_str("    ret\n\n");
        
        code.push_str("hardware_port_out_8:\n");
        code.push_str("    ; 8-bit port output\n");
        code.push_str("    ; Input: RDX = port, AL = value\n");
        code.push_str("    out dx, al\n");
        code.push_str("    ret\n");
    }
    
    // Data section
    code.push_str("section .data\n");
    
    // String literals
    for (string, label) in &self.string_literals {
        code.push_str(&format!("{}: db '{}', 10, 0\n", label, string)); // Add newline for Linux
    }
    
    Ok(code)
}
    
    fn compile_linux64_statements(&mut self, code: &mut String, statements: &[Statement]) -> Result<(), String> {
    for stmt in statements {
        match stmt {
            Statement::Expr(expr) => {
                code.push_str("    ; Expression\n");
                match expr {
                    Expr::String(s, _) => {
                        if let Some(label) = self.string_literals.get(s) {
                            code.push_str(&format!("    ; String: '{}'\n", s));
                            code.push_str(&format!("    lea rdi, [{}]\n", label));
                            code.push_str("    call linux_print\n");
                        } else {
                            return Err(format!("String literal '{}' not found in data section", s));
                        }
                    }
                    Expr::Call { func, args, kwargs: _, span: _ } if func == "print" => {
                        code.push_str("    ; print statement\n");
                        for arg in args {
                            match arg {
                                Expr::String(s, _) => {
                                    if let Some(label) = self.string_literals.get(s) {
                                        code.push_str(&format!("    lea rdi, [{}]\n", label));
                                        code.push_str("    call linux_print\n");
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {
                        code.push_str("    ; [Expression]\n");
                    }
                }
            }
            Statement::VarDecl { name, value: _, .. } => {
                code.push_str("    ; var ");
                code.push_str(name);
                code.push_str(" = \n");
            }
            Statement::While { condition, body, orelse: _, span: _ } => {
                let label_id = self.new_label("while");
                let while_start = format!("while_start_{}", label_id);
                let while_end = format!("while_end_{}", label_id);
                
                code.push_str("    ; While loop\n");
                code.push_str(&format!("{}:\n", while_start));
                
                // Compile condition
                match condition {
                    Expr::Compare { left, ops, comparators, .. } if ops.len() == 1 => {
                        // Simple comparison
                        match (left.as_ref(), comparators.get(0)) {
                            (Expr::Var(left_name, _), Some(Expr::Number(right_val, _))) => {
                                // Load left variable
                                if let Some(offset) = self.get_variable_offset(left_name) {
                                    code.push_str(&format!("    ; Load variable {} from [rsp + {}]\n", left_name, offset));
                                    code.push_str(&format!("    mov rax, [rsp + {}]\n", offset));
                                    code.push_str(&format!("    cmp rax, {}\n", right_val));
                                    
                                    match ops[0] {
                                        CompareOp::Lt => {
                                            code.push_str("    jge ");
                                        }
                                        CompareOp::Gt => {
                                            code.push_str("    jle ");
                                        }
                                        CompareOp::Eq => {
                                            code.push_str("    jne ");
                                        }
                                        CompareOp::Ne => {
                                            code.push_str("    je ");
                                        }
                                        CompareOp::Le => {
                                            code.push_str("    jg ");
                                        }
                                        CompareOp::Ge => {
                                            code.push_str("    jl ");
                                        }
                                        _ => {
                                            code.push_str("    jz ");
                                        }
                                    }
                                    code.push_str(&format!("{}\n", while_end));
                                } else {
                                    return Err(format!("Undefined variable: {}", left_name));
                                }
                            }
                            _ => {
                                code.push_str("    ; Complex condition - TODO implement\n");
                                code.push_str(&format!("    jmp {}\n", while_end));
                            }
                        }
                    }
                    Expr::Var(name, _) => {
                        // Variable as condition (non-zero is true)
                        if let Some(offset) = self.get_variable_offset(name) {
                            code.push_str(&format!("    ; Load variable {} from [rsp + {}]\n", name, offset));
                            code.push_str(&format!("    mov rax, [rsp + {}]\n", offset));
                            code.push_str("    test rax, rax\n");
                            code.push_str(&format!("    jz {}\n", while_end));
                        } else {
                            return Err(format!("Undefined variable: {}", name));
                        }
                    }
                    _ => {
                        code.push_str("    ; Complex condition\n");
                        code.push_str(&format!("    jmp {}\n", while_end));
                    }
                }
                
                // Compile body
                code.push_str("    ; While body\n");
                for body_stmt in body {
                    match body_stmt {
                        Statement::Expr(expr) => {
                            match expr {
                                Expr::Call { func, args, kwargs: _, span: _ } if func == "print" => {
                                    for arg in args {
                                        match arg {
                                            Expr::String(s, _) => {
                                                if let Some(label) = self.string_literals.get(s) {
                                                    code.push_str(&format!("    lea rdi, [{}]\n", label));
                                                    code.push_str("    call linux_print\n");
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                        Statement::Assign { target, value, span: _ } => {
                            if let Some(offset) = self.get_variable_offset(target) {
                                code.push_str(&format!("    ; Assignment to {} at [rsp + {}]\n", target, offset));
                                match value {
                                    Expr::Number(n, _) => {
                                        code.push_str(&format!("    mov rax, {}\n", n));
                                    }
                                    Expr::Var(var_name, _) => {
                                        if let Some(var_offset) = self.get_variable_offset(var_name) {
                                            code.push_str(&format!("    mov rax, [rsp + {}]\n", var_offset));
                                        } else {
                                            return Err(format!("Undefined variable: {}", var_name));
                                        }
                                    }
                                    _ => {
                                        code.push_str("    ; Complex value\n");
                                    }
                                }
                                code.push_str(&format!("    mov [rsp + {}], rax\n", offset));
                            } else {
                                return Err(format!("Undefined variable: {}", target));
                            }
                        }
                        _ => {
                            code.push_str("    ; [Statement in while body]\n");
                        }
                    }
                }
                
                code.push_str(&format!("    jmp {}\n", while_start));
                code.push_str(&format!("{}:\n", while_end));
            }
            Statement::HardwareFunctionDef { device, name, args, body, span: _ } => {
                // Handle hardware function definition
                code.push_str(&format!("    ; Hardware function: {} for device {}\n", name, device));
                code.push_str(&format!("{}:\n", name));
                code.push_str("    push rbp\n");
                code.push_str("    mov rbp, rsp\n");
                
                // Allocate space for arguments
                if !args.is_empty() {
                    code.push_str(&format!("    sub rsp, {}\n", args.len() * 8));
                }
                
                // Compile body statements
                for body_stmt in body {
                    match body_stmt {
                        Statement::Expr(Expr::Call { func, args: hw_args, kwargs: _, span: _ }) 
                            if func.starts_with("hw_") || func == "write_register" => {
                            // Handle hardware intrinsics
                            code.push_str(&format!("    ; Hardware intrinsic: {}\n", func));
                            if hw_args.len() >= 2 {
                                match (&hw_args[0], &hw_args[1]) {
                                    (Expr::Number(port, _), Expr::Number(value, _)) => {
                                        code.push_str(&format!("    mov dx, {}\n", port));
                                        code.push_str(&format!("    mov al, {}\n", value));
                                        code.push_str("    out dx, al\n");
                                    }
                                    _ => {}
                                }
                            }
                        }
                        _ => {
                            code.push_str("    ; [Hardware function body]\n");
                        }
                    }
                }
                
                code.push_str("    mov rsp, rbp\n");
                code.push_str("    pop rbp\n");
                code.push_str("    ret\n");
            }
            _ => {
                code.push_str("    ; [Statement type not implemented]\n");
            }
        }
    }
    Ok(())
}
    
    // Compilation helpers
    fn compile_bios64_expression(&mut self, code: &mut String, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::Number(n, _) => {
                self.byte_counter.add(15);
                code.push_str(&format!("    ; Number: {}\n", n));
                code.push_str(&format!("    mov rax, {}\n", n));
                code.push_str("    call print_decimal_64\n");
            }
            Expr::String(s, _) => {
                self.byte_counter.add(s.len() + 25);
                if self.byte_counter.exceeds_limit(80) {
                    return Err(format!(
                        "String '{}' would exceed boot sector limit. Reduce string literals.",
                        if s.len() > 20 { format!("{}...", &s[..20]) } else { s.clone() }
                    ));
                }
                code.push_str(&format!("    ; String: '{}'\n", s));
                code.push_str("    ; Strings must be in data section at 0x7E00\n");
            }
            Expr::Call { func, args, kwargs: _, span: _ } if func == "print" => {
                self.byte_counter.add(20); // Print overhead
                for arg in args {
                    self.compile_bios64_expression(code, arg)?;
                }
            }
            _ => {
                self.byte_counter.add(5);
                code.push_str("    ; [Expression]\n");
            }
        }
        Ok(())
    }
    
    fn compile_standard(&mut self, program: &Program) -> Result<String, String> {
        match self.target.platform {
            TargetPlatform::Linux64 => self.compile_linux64(program),
        }
    }
    
    // Import handling (placeholder - needs to be implemented)
    fn compile_with_imports(&mut self, program: &Program) -> Result<String, String> {
        // For now, just compile without imports
        self.compile_standard(program)
    }
}

// Byte counter for size limiting
struct ByteCounter {
    total: usize,
    limit: usize,
}

impl ByteCounter {
    fn new() -> Self {
        Self { total: 0, limit: 512 }
    }
    
    fn reset(&mut self) {
        self.total = 0;
    }
    
    fn add(&mut self, bytes: usize) {
        self.total += bytes;
    }
    
    fn exceeds_limit(&self, threshold_percent: u8) -> bool {
        let threshold = (self.limit * threshold_percent as usize) / 100;
        self.total > threshold
    }
    
    fn total(&self) -> usize {
        self.total
    }
}

// Public interface
pub fn compile_to_nasm(program: &Program) -> Result<String, String> {
    let mut emitter = NasmEmitter::new();
    emitter.compile_program(program)
}