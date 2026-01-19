use std::collections::HashMap;
use crate::parser::{Program, Statement, Expr, CompareOp};


#[derive(Debug, Clone, PartialEq)]
pub enum TargetPlatform {
    Linux64,
    Bios16,
    Bios32,
    Bios64,
    Bios64SSE,
    Bios64AVX,
    Bios64AVX512,
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
    pub fn bios16() -> Self { Self { platform: TargetPlatform::Bios16, bits: 16, format: "bin", entry_point: "_start" } }
    pub fn bios32() -> Self { Self { platform: TargetPlatform::Bios32, bits: 32, format: "bin", entry_point: "_start" } }
    pub fn bios64() -> Self { Self { platform: TargetPlatform::Bios64, bits: 64, format: "bin", entry_point: "_start" } }
    pub fn bios64_sse() -> Self { Self { platform: TargetPlatform::Bios64SSE, bits: 64, format: "bin", entry_point: "_start" } }
    pub fn bios64_avx() -> Self { Self { platform: TargetPlatform::Bios64AVX, bits: 64, format: "bin", entry_point: "_start" } }
    pub fn bios64_avx512() -> Self { Self { platform: TargetPlatform::Bios64AVX512, bits: 64, format: "bin", entry_point: "_start" } }
    
    pub fn is_bios(&self) -> bool {
        matches!(self.platform, TargetPlatform::Bios16 | TargetPlatform::Bios32 | 
                 TargetPlatform::Bios64 | TargetPlatform::Bios64SSE |
                 TargetPlatform::Bios64AVX | TargetPlatform::Bios64AVX512)
    }
    pub fn is_linux(&self) -> bool { self.platform == TargetPlatform::Linux64 }
}


pub struct NasmEmitter {
    pub target: TargetConfig,
    label_counter: u32,
    variable_offsets: HashMap<String, i32>,
    byte_counter: ByteCounter,
    string_literals: HashMap<String, String>, // Maps Earthang string to NASM label
    data_labels: Vec<String>, // Track data section labels
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
        }
    }
    
    // Target configuration methods
    pub fn set_target_linux(&mut self) { self.target = TargetConfig::linux64(); }
    pub fn set_target_bios16(&mut self) { self.target = TargetConfig::bios16(); }
    pub fn set_target_bios32(&mut self) { self.target = TargetConfig::bios32(); }
    pub fn set_target_bios64(&mut self) { self.target = TargetConfig::bios64(); }
    pub fn set_target_bios64_sse(&mut self) { self.target = TargetConfig::bios64_sse(); }
    pub fn set_target_bios64_avx(&mut self) { self.target = TargetConfig::bios64_avx(); }
    pub fn set_target_bios64_avx512(&mut self) { self.target = TargetConfig::bios64_avx512(); }
    
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
        
        if self.target.is_bios() {
            self.compile_bios(program)
        } else {
            self.compile_standard(program)
        }
    }
    
    fn compile_linux64(&mut self, program: &Program) -> Result<String, String> {
    let mut code = String::new();
    
    code.push_str("; Earthang Linux 64-bit Executable\n");
    code.push_str("; Format: ELF64\n\n");
    
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
            _ => {
                code.push_str("    ; [Statement type not implemented]\n");
            }
        }
    }
    Ok(())
}
    
    fn compile_bios(&mut self, program: &Program) -> Result<String, String> {
        let mut code = String::new();
        
        match self.target.platform {
            TargetPlatform::Bios16 => self.compile_bios16(&mut code, program)?,
            TargetPlatform::Bios32 => self.compile_bios32(&mut code, program)?,
            TargetPlatform::Bios64 => self.compile_bios64(&mut code, program)?,
            TargetPlatform::Bios64SSE => self.compile_bios64_sse(&mut code, program)?,
            TargetPlatform::Bios64AVX => self.compile_bios64_avx(&mut code, program)?,
            TargetPlatform::Bios64AVX512 => self.compile_bios64_avx512(&mut code, program)?,
            _ => { code.push_str("; Unsupported BIOS target\n"); }
        }
        
        // Check size limits
        if self.byte_counter.exceeds_limit(80) {
            return Err(format!(
                "Code size limit exceeded: estimated {} bytes (80% of 512 byte limit). Reduce print statements or string literals.",
                self.byte_counter.total()
            ));
        }
        
        Ok(code)
    }
    
    fn compile_bios16(&mut self, code: &mut String, program: &Program) -> Result<(), String> {
        code.push_str("; Earthang 16-bit Bootloader\n\n");
        code.push_str("    org 0x7C00\n");
        code.push_str("    bits 16\n\n");
        code.push_str("start:\n");
        code.push_str("    cli\n");
        code.push_str("    xor ax, ax\n");
        code.push_str("    mov ds, ax\n");
        code.push_str("    mov es, ax\n");
        code.push_str("    mov ss, ax\n");
        code.push_str("    mov sp, 0x7C00\n");
        code.push_str("    sti\n");
        code.push_str("    cld\n\n");
        
        code.push_str("    mov ax, 0x0003\n");
        code.push_str("    int 0x10\n\n");
        
        code.push_str("    mov si, 0x7E00  ; String data starts after 512 bytes\n");
        code.push_str("    call print_string\n\n");
        
        self.compile_bios16_statements(code, &program.body);
        
        code.push_str("\n    cli\n");
        code.push_str("    hlt\n");
        code.push_str("    jmp $\n\n");
        
        // 16-bit subroutines
        code.push_str("print_string:\n");
        code.push_str("    pusha\n");
        code.push_str("    mov ah, 0x0E\n");
        code.push_str(".loop:\n");
        code.push_str("    lodsb\n");
        code.push_str("    test al, al\n");
        code.push_str("    jz .done\n");
        code.push_str("    int 0x10\n");
        code.push_str("    jmp .loop\n");
        code.push_str(".done:\n");
        code.push_str("    popa\n");
        code.push_str("    ret\n\n");
        
        code.push_str("print_decimal:\n");
        code.push_str("    ; Print decimal number in AX\n");
        code.push_str("    pusha\n");
        code.push_str("    mov cx, 0\n");
        code.push_str("    mov bx, 10\n");
        code.push_str(".div_loop:\n");
        code.push_str("    xor dx, dx\n");
        code.push_str("    div bx\n");
        code.push_str("    push dx\n");
        code.push_str("    inc cx\n");
        code.push_str("    test ax, ax\n");
        code.push_str("    jnz .div_loop\n");
        code.push_str(".print_loop:\n");
        code.push_str("    pop ax\n");
        code.push_str("    add al, '0'\n");
        code.push_str("    mov ah, 0x0E\n");
        code.push_str("    int 0x10\n");
        code.push_str("    loop .print_loop\n");
        code.push_str("    popa\n");
        code.push_str("    ret\n\n");
        
        // Boot sector padding and signature
        code.push_str("    times 510-($-$$) db 0\n");
        code.push_str("    dw 0xAA55\n\n");
        
        // String data AFTER boot sector
        code.push_str("    ; String data at 0x7E00 (after boot sector)\n");
        code.push_str("    times 512 db 0  ; Boot sector padding\n");
        code.push_str("    db 'Earthang 16-bit', 0\n");
        
        Ok(())
    }
    
    fn compile_bios32(&mut self, code: &mut String, program: &Program) -> Result<(), String> {
        code.push_str("; Earthang 32-bit Bootloader\n\n");
        code.push_str("    org 0x7C00\n");
        code.push_str("    bits 16\n\n");
        code.push_str("start:\n");
        code.push_str("    cli\n");
        code.push_str("    xor ax, ax\n");
        code.push_str("    mov ds, ax\n");
        code.push_str("    mov es, ax\n");
        code.push_str("    mov ss, ax\n");
        code.push_str("    mov sp, 0x7C00\n");
        code.push_str("    sti\n");
        code.push_str("    cld\n\n");
        
        code.push_str("    mov ax, 0x0003\n");
        code.push_str("    int 0x10\n\n");
        
        code.push_str("    in al, 0x92\n");
        code.push_str("    or al, 2\n");
        code.push_str("    out 0x92, al\n\n");
        
        code.push_str("    lgdt [gdt32_desc]\n\n");
        
        code.push_str("    mov eax, cr0\n");
        code.push_str("    or eax, 1\n");
        code.push_str("    mov cr0, eax\n\n");
        
        code.push_str("    jmp 0x08:protected_mode\n\n");
        
        code.push_str("    bits 32\n");
        code.push_str("protected_mode:\n");
        code.push_str("    mov ax, 0x10\n");
        code.push_str("    mov ds, ax\n");
        code.push_str("    mov es, ax\n");
        code.push_str("    mov fs, ax\n");
        code.push_str("    mov gs, ax\n");
        code.push_str("    mov ss, ax\n");
        code.push_str("    mov esp, 0x7C00\n\n");
        
        code.push_str("    mov esi, 0x7E00  ; String data starts after 512 bytes\n");
        code.push_str("    mov edi, 0xB8000\n");
        code.push_str("    call print_string_32\n\n");
        
        self.compile_bios32_statements(code, &program.body);
        
        code.push_str("\n    cli\n");
        code.push_str("    hlt\n");
        code.push_str("    jmp $\n\n");
        
        // 32-bit subroutines
        code.push_str("print_string_32:\n");
        code.push_str("    pusha\n");
        code.push_str(".loop:\n");
        code.push_str("    mov al, [esi]\n");
        code.push_str("    test al, al\n");
        code.push_str("    jz .done\n");
        code.push_str("    mov [edi], al\n");
        code.push_str("    inc esi\n");
        code.push_str("    inc edi\n");
        code.push_str("    mov byte [edi], 0x0F\n");
        code.push_str("    inc edi\n");
        code.push_str("    jmp .loop\n");
        code.push_str(".done:\n");
        code.push_str("    popa\n");
        code.push_str("    ret\n\n");
        
        code.push_str("print_decimal_32:\n");
        code.push_str("    ; Print decimal number in EAX\n");
        code.push_str("    pusha\n");
        code.push_str("    mov ecx, 0\n");
        code.push_str("    mov ebx, 10\n");
        code.push_str("    mov edi, 0xB8000 + 160  ; Second line\n");
        code.push_str(".div_loop:\n");
        code.push_str("    xor edx, edx\n");
        code.push_str("    div ebx\n");
        code.push_str("    push dx\n");
        code.push_str("    inc ecx\n");
        code.push_str("    test eax, eax\n");
        code.push_str("    jnz .div_loop\n");
        code.push_str(".print_loop:\n");
        code.push_str("    pop ax\n");
        code.push_str("    add al, '0'\n");
        code.push_str("    mov [edi], al\n");
        code.push_str("    inc edi\n");
        code.push_str("    mov byte [edi], 0x0F\n");
        code.push_str("    inc edi\n");
        code.push_str("    loop .print_loop\n");
        code.push_str("    popa\n");
        code.push_str("    ret\n\n");
        
        // GDT
        code.push_str("gdt32:\n");
        code.push_str("    dq 0x0000000000000000\n");
        code.push_str("    dq 0x00CF9A000000FFFF\n");
        code.push_str("    dq 0x00CF92000000FFFF\n");
        code.push_str("gdt32_end:\n\n");
        
        code.push_str("gdt32_desc:\n");
        code.push_str("    dw gdt32_end - gdt32 - 1\n");
        code.push_str("    dd gdt32\n\n");
        
        // Boot sector padding and signature
        code.push_str("    times 510-($-$$) db 0\n");
        code.push_str("    dw 0xAA55\n\n");
        
        // String data AFTER boot sector
        code.push_str("    ; String data at 0x7E00 (after boot sector)\n");
        code.push_str("    times 512 db 0  ; Boot sector padding\n");
        code.push_str("    db 'Earthang 32-bit', 0\n");
        
        Ok(())
    }
    
    fn compile_bios64(&mut self, code: &mut String, program: &Program) -> Result<(), String> {
    let mut code_buffer = String::new();
    
    // Start with proper boot sector
    code_buffer.push_str("; Earthang 64-bit Bootloader\n\n");
    code_buffer.push_str("    org 0x7C00\n");
    code_buffer.push_str("    bits 16\n\n");
    
    code_buffer.push_str("start:\n");
    code_buffer.push_str("    cli                     ; Disable interrupts - CRITICAL!\n");
    code_buffer.push_str("    xor ax, ax\n");
    code_buffer.push_str("    mov ds, ax\n");
    code_buffer.push_str("    mov es, ax\n");
    code_buffer.push_str("    mov ss, ax\n");
    code_buffer.push_str("    mov sp, 0x7C00          ; Stack grows downward from bootloader\n");
    code_buffer.push_str("    sti                     ; Enable interrupts for BIOS calls\n");
    code_buffer.push_str("    cld                     ; Clear direction flag\n\n");
    
    code_buffer.push_str("    ; Clear screen\n");
    code_buffer.push_str("    mov ax, 0x0003          ; 80x25 text mode\n");
    code_buffer.push_str("    int 0x10\n\n");
    
    // Save boot drive
    code_buffer.push_str("    mov [boot_drive], dl    ; Save boot drive number\n\n");
    
    // Print loading message
    code_buffer.push_str("    ; Print loading message\n");
    code_buffer.push_str("    mov si, loading_msg\n");
    code_buffer.push_str("    call print_string_16\n\n");
    
    // Check for required BIOS extensions (LBA)
    code_buffer.push_str("    ; Check for LBA extensions\n");
    code_buffer.push_str("    mov ah, 0x41\n");
    code_buffer.push_str("    mov bx, 0x55AA\n");
    code_buffer.push_str("    mov dl, [boot_drive]\n");
    code_buffer.push_str("    int 0x13\n");
    code_buffer.push_str("    jc disk_error           ; No LBA support\n");
    code_buffer.push_str("    cmp bx, 0xAA55\n");
    code_buffer.push_str("    jne disk_error\n");
    code_buffer.push_str("    test cl, 1\n");
    code_buffer.push_str("    jz disk_error\n\n");
    
    // Disable interrupts before disk operations
    code_buffer.push_str("    ; Disable interrupts for disk operations\n");
    code_buffer.push_str("    cli\n\n");
    
    // Load additional sectors (stage 2) to 0x7E00
    code_buffer.push_str("    ; Load stage 2 to 0x7E00\n");
    code_buffer.push_str("    mov ah, 0x42           ; Extended Read Sectors\n");
    code_buffer.push_str("    mov dl, [boot_drive]\n");
    code_buffer.push_str("    mov si, dap\n");
    code_buffer.push_str("    int 0x13\n");
    code_buffer.push_str("    jc disk_error\n\n");
    
    // CRITICAL: Properly jump to stage 2 at 0x7E00
    code_buffer.push_str("    ; Jump to stage 2 - interrupts STAY DISABLED!\n");
    code_buffer.push_str("    cli\n");
    code_buffer.push_str("    jmp 0x0000:0x7E00      ; Far jump to stage 2\n\n");
    
    // 16-bit print function (must be in first sector)
    code_buffer.push_str("print_string_16:\n");
    code_buffer.push_str("    pusha\n");
    code_buffer.push_str("    mov ah, 0x0E\n");
    code_buffer.push_str(".loop_16:\n");
    code_buffer.push_str("    lodsb\n");
    code_buffer.push_str("    test al, al\n");
    code_buffer.push_str("    jz .done_16\n");
    code_buffer.push_str("    int 0x10\n");
    code_buffer.push_str("    jmp .loop_16\n");
    code_buffer.push_str(".done_16:\n");
    code_buffer.push_str("    popa\n");
    code_buffer.push_str("    ret\n\n");
    
    // Disk error handler
    code_buffer.push_str("disk_error:\n");
    code_buffer.push_str("    mov si, disk_err_msg\n");
    code_buffer.push_str("    call print_string_16\n");
    code_buffer.push_str("    ; Hang on error\n");
    code_buffer.push_str("    cli\n");
    code_buffer.push_str("    hlt\n");
    code_buffer.push_str("    jmp $\n\n");
    
    // Disk Address Packet
    code_buffer.push_str("dap:\n");
    code_buffer.push_str("    db 0x10                ; Size of DAP (16 bytes)\n");
    code_buffer.push_str("    db 0                   ; Unused, should be 0\n");
    code_buffer.push_str("    dw 32                  ; Number of sectors to read (16KB)\n");
    code_buffer.push_str("    dw 0x7E00              ; Offset (0x7E00)\n");
    code_buffer.push_str("    dw 0x0000              ; Segment (0x0000)\n");
    code_buffer.push_str("    dq 1                   ; Starting LBA (sector 2, 0-based)\n\n");
    
    // Data in first sector
    code_buffer.push_str("boot_drive:\n");
    code_buffer.push_str("    db 0\n\n");
    
    code_buffer.push_str("loading_msg:\n");
    code_buffer.push_str("    db 'Loading Earthang 64-bit...', 0\n");
    code_buffer.push_str("disk_err_msg:\n");
    code_buffer.push_str("    db 'Disk Error!', 0\n\n");
    
    // Pad boot sector to 510 bytes and add signature
    code_buffer.push_str("    times 510-($-$$) db 0\n");
    code_buffer.push_str("    dw 0xAA55\n\n");
    
    // ============================================
    // Stage 2: Loaded at 0x7E00 (sector 2+)
    // ============================================
    
    code_buffer.push_str("; ============================================\n");
    code_buffer.push_str("; Stage 2: 64-bit Mode Setup\n");
    code_buffer.push_str("; ============================================\n\n");
    
    code_buffer.push_str("    bits 16\n");
    code_buffer.push_str("stage2_start:\n");
    code_buffer.push_str("    ; INTERRUPTS MUST REMAIN DISABLED!\n");
    code_buffer.push_str("    cli\n\n");
    
    // Enable A20 line
    code_buffer.push_str("    ; Enable A20 line via keyboard controller\n");
    code_buffer.push_str("    call enable_a20\n\n");
    
    // Load 32-bit GDT
    code_buffer.push_str("    ; Load 32-bit GDT\n");
    code_buffer.push_str("    lgdt [gdt32_desc]\n\n");
    
    // Enter protected mode
    code_buffer.push_str("    ; Enter protected mode (interrupts remain disabled)\n");
    code_buffer.push_str("    mov eax, cr0\n");
    code_buffer.push_str("    or eax, 1\n");
    code_buffer.push_str("    mov cr0, eax\n\n");
    
    code_buffer.push_str("    ; Far jump to protected mode (clear pipeline)\n");
    code_buffer.push_str("    jmp 0x08:protected_mode\n\n");
    
    // A20 enable function
    code_buffer.push_str("enable_a20:\n");
    code_buffer.push_str("    ; Try keyboard controller method\n");
    code_buffer.push_str("    call .wait_kbd_in\n");
    code_buffer.push_str("    mov al, 0xAD\n");
    code_buffer.push_str("    out 0x64, al\n");
    code_buffer.push_str("    call .wait_kbd_in\n");
    code_buffer.push_str("    mov al, 0xD0\n");
    code_buffer.push_str("    out 0x64, al\n");
    code_buffer.push_str("    call .wait_kbd_out\n");
    code_buffer.push_str("    in al, 0x60\n");
    code_buffer.push_str("    push eax\n");
    code_buffer.push_str("    call .wait_kbd_in\n");
    code_buffer.push_str("    mov al, 0xD1\n");
    code_buffer.push_str("    out 0x64, al\n");
    code_buffer.push_str("    call .wait_kbd_in\n");
    code_buffer.push_str("    pop eax\n");
    code_buffer.push_str("    or al, 2\n");
    code_buffer.push_str("    out 0x60, al\n");
    code_buffer.push_str("    call .wait_kbd_in\n");
    code_buffer.push_str("    mov al, 0xAE\n");
    code_buffer.push_str("    out 0x64, al\n");
    code_buffer.push_str("    call .wait_kbd_in\n");
    code_buffer.push_str("    ret\n");
    code_buffer.push_str(".wait_kbd_in:\n");
    code_buffer.push_str("    in al, 0x64\n");
    code_buffer.push_str("    test al, 2\n");
    code_buffer.push_str("    jnz .wait_kbd_in\n");
    code_buffer.push_str("    ret\n");
    code_buffer.push_str(".wait_kbd_out:\n");
    code_buffer.push_str("    in al, 0x64\n");
    code_buffer.push_str("    test al, 1\n");
    code_buffer.push_str("    jz .wait_kbd_out\n");
    code_buffer.push_str("    ret\n\n");
    
    // 32-bit protected mode
    code_buffer.push_str("    bits 32\n");
    code_buffer.push_str("protected_mode:\n");
    code_buffer.push_str("    ; Set up segment registers\n");
    code_buffer.push_str("    mov ax, 0x10\n");
    code_buffer.push_str("    mov ds, ax\n");
    code_buffer.push_str("    mov es, ax\n");
    code_buffer.push_str("    mov fs, ax\n");
    code_buffer.push_str("    mov gs, ax\n");
    code_buffer.push_str("    mov ss, ax\n");
    code_buffer.push_str("    mov esp, 0x90000        ; Stack at 0x90000\n");
    code_buffer.push_str("\n");
    
    // Setup null IDT for protected mode
    code_buffer.push_str("    ; Setup null IDT to disable interrupts\n");
    code_buffer.push_str("    lidt [idtr_32]\n\n");
    
    // Setup PAE paging for 64-bit mode
    code_buffer.push_str("    ; Setup PAE paging tables\n");
    code_buffer.push_str("    mov edi, 0x1000\n");
    code_buffer.push_str("    mov cr3, edi\n");
    code_buffer.push_str("    xor eax, eax\n");
    code_buffer.push_str("    mov ecx, 4096\n");
    code_buffer.push_str("    rep stosd\n");
    
    code_buffer.push_str("    mov edi, 0x1000\n");
    code_buffer.push_str("    ; PML4 entry pointing to PDPT\n");
    code_buffer.push_str("    mov dword [edi], 0x2003\n");
    code_buffer.push_str("    add edi, 0x1000\n");
    code_buffer.push_str("    ; PDPT entry pointing to PD\n");
    code_buffer.push_str("    mov dword [edi], 0x3003\n");
    code_buffer.push_str("    add edi, 0x1000\n");
    
    code_buffer.push_str("    ; Page directory entries (2MB pages, identity mapping)\n");
    code_buffer.push_str("    mov ebx, 0x00000083    ; Present + Writable + Large Page\n");
    code_buffer.push_str("    mov ecx, 512           ; Map 512 * 2MB = 1GB\n");
    code_buffer.push_str(".set_entry:\n");
    code_buffer.push_str("    mov dword [edi], ebx\n");
    code_buffer.push_str("    add ebx, 0x200000      ; Next 2MB page\n");
    code_buffer.push_str("    add edi, 8\n");
    code_buffer.push_str("    loop .set_entry\n\n");
    
    // Enable PAE
    code_buffer.push_str("    ; Enable PAE\n");
    code_buffer.push_str("    mov eax, cr4\n");
    code_buffer.push_str("    or eax, (1 << 5)\n");
    code_buffer.push_str("    mov cr4, eax\n\n");
    
    // Enable long mode in EFER
    code_buffer.push_str("    ; Enable long mode\n");
    code_buffer.push_str("    mov ecx, 0xC0000080    ; EFER MSR\n");
    code_buffer.push_str("    rdmsr\n");
    code_buffer.push_str("    or eax, (1 << 8)       ; LME bit\n");
    code_buffer.push_str("    wrmsr\n\n");
    
    // Enable paging
    code_buffer.push_str("    ; Enable paging\n");
    code_buffer.push_str("    mov eax, cr0\n");
    code_buffer.push_str("    or eax, (1 << 31)      ; PG bit\n");
    code_buffer.push_str("    mov cr0, eax\n\n");
    
    // Load 64-bit GDT
    code_buffer.push_str("    ; Load 64-bit GDT\n");
    code_buffer.push_str("    lgdt [gdt64_desc]\n\n");
    
    // Far jump to 64-bit mode
    code_buffer.push_str("    ; Jump to 64-bit mode\n");
    code_buffer.push_str("    jmp 0x08:long_mode\n\n");
    
    // 64-bit long mode
    code_buffer.push_str("    bits 64\n");
    code_buffer.push_str("long_mode:\n");
    code_buffer.push_str("    ; Set up segment registers for 64-bit\n");
    code_buffer.push_str("    mov ax, 0x10\n");
    code_buffer.push_str("    mov ds, ax\n");
    code_buffer.push_str("    mov es, ax\n");
    code_buffer.push_str("    mov fs, ax\n");
    code_buffer.push_str("    mov gs, ax\n");
    code_buffer.push_str("    mov ss, ax\n");
    code_buffer.push_str("    mov rsp, 0x90000       ; 64-bit stack\n");
    code_buffer.push_str("\n");
    code_buffer.push_str("    ; Setup null IDT for 64-bit mode\n");
    code_buffer.push_str("    lidt [idtr_64]\n\n");
    
    // Clear screen (VGA text mode)
    code_buffer.push_str("    ; Clear VGA text buffer\n");
    code_buffer.push_str("    mov rdi, 0xB8000\n");
    code_buffer.push_str("    mov rax, 0x0720072007200720  ; Space + light gray\n");
    code_buffer.push_str("    mov rcx, 1000           ; 80x25 / 8\n");
    code_buffer.push_str("    rep stosq\n");
    code_buffer.push_str("\n");
    
    // Print welcome message
    code_buffer.push_str("    ; Print welcome message\n");
    code_buffer.push_str("    mov rsi, welcome_msg\n");
    code_buffer.push_str("    mov rdi, 0xB8000\n");
    code_buffer.push_str("    call print_string_64\n");
    code_buffer.push_str("\n");
    
    // Collect string literals from program
    self.collect_string_literals(program);
    
    // Compile the Earthang program
    let mut main_code = String::new();
    for stmt in &program.body {
        match stmt {
            Statement::Expr(expr) => {
                main_code.push_str("    ; Expression\n");
                match expr {
                    Expr::Number(n, _) => {
                        // Just store number, don't print
                        main_code.push_str(&format!("    ; Number constant: {}\n", n));
                    }
                    Expr::String(s, _) => {
                        // Strings are handled in data section
                        main_code.push_str(&format!("    ; String constant: '{}'\n", s));
                    }
                    Expr::Call { func, args, kwargs: _, span: _ } if func == "print" => {
                        main_code.push_str("    ; Print statement\n");
                        for arg in args {
                            match arg {
                                Expr::String(s, _) => {
                                    if let Some(label) = self.string_literals.get(s) {
                                        main_code.push_str(&format!("    mov rsi, {}\n", label));
                                        main_code.push_str("    mov rdi, 0xB8000 + 160\n");
                                        main_code.push_str("    call print_string_64\n");
                                    }
                                }
                                Expr::Number(n, _) => {
                                    main_code.push_str(&format!("    mov rax, {}\n", n));
                                    main_code.push_str("    call print_decimal_64\n");
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }
            }
            Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                main_code.push_str(&format!("    ; Variable declaration: {}\n", name));
                match value {
                    Expr::Number(n, _) => {
                        // Store in memory (simplified)
                        main_code.push_str(&format!("    mov qword [var_{}], {}\n", name, n));
                    }
                    _ => {}
                }
            }
            Statement::Assign { target, value, span: _ } => {
                main_code.push_str(&format!("    ; Assignment: {} = ...\n", target));
                match value {
                    Expr::Number(n, _) => {
                        main_code.push_str(&format!("    mov qword [var_{}], {}\n", target, n));
                    }
                    _ => {}
                }
            }
            Statement::While { condition, body, orelse: _, span: _ } => {
                let label_id = self.new_label("while");
                let while_start = format!("while_start_{}", label_id);
                let while_end = format!("while_end_{}", label_id);
                
                main_code.push_str("    ; While loop\n");
                main_code.push_str(&format!("{}:\n", while_start));
                
                // Simple condition check
                match condition {
                    Expr::Compare { left, ops, comparators, .. } if ops.len() == 1 => {
                        match (left.as_ref(), comparators.get(0)) {
                            (Expr::Var(var_name, _), Some(Expr::Number(num, _))) => {
                                // Load variable and compare
                                main_code.push_str(&format!("    mov rax, [var_{}]\n", var_name));
                                main_code.push_str(&format!("    cmp rax, {}\n", num));
                                
                                // Jump based on comparison
                                match ops[0] {
                                    CompareOp::Lt => main_code.push_str(&format!("    jge {}\n", while_end)),
                                    CompareOp::Gt => main_code.push_str(&format!("    jle {}\n", while_end)),
                                    CompareOp::Eq => main_code.push_str(&format!("    jne {}\n", while_end)),
                                    CompareOp::Ne => main_code.push_str(&format!("    je {}\n", while_end)),
                                    CompareOp::Le => main_code.push_str(&format!("    jg {}\n", while_end)),
                                    CompareOp::Ge => main_code.push_str(&format!("    jl {}\n", while_end)),
                                    _ => main_code.push_str(&format!("    jz {}\n", while_end)),
                                }
                            }
                            _ => {
                                main_code.push_str("    ; Complex condition - break\n");
                                main_code.push_str(&format!("    jmp {}\n", while_end));
                            }
                        }
                    }
                    _ => {
                        main_code.push_str("    ; Condition not implemented\n");
                        main_code.push_str(&format!("    jmp {}\n", while_end));
                    }
                }
                
                // Body
                for body_stmt in body {
                    match body_stmt {
                        Statement::Expr(expr) => {
                            match expr {
                                Expr::Call { func, args, kwargs: _, span: _ } if func == "print" => {
                                    for arg in args {
                                        match arg {
                                            Expr::String(s, _) => {
                                                if let Some(label) = self.string_literals.get(s) {
                                                    main_code.push_str(&format!("    mov rsi, {}\n", label));
                                                    main_code.push_str("    mov rdi, 0xB8000 + 320\n");
                                                    main_code.push_str("    call print_string_64\n");
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
                
                main_code.push_str(&format!("    jmp {}\n", while_start));
                main_code.push_str(&format!("{}:\n", while_end));
            }
            _ => {
                main_code.push_str("    ; [Statement type not implemented]\n");
            }
        }
    }
    
    // Add main program code
    if !main_code.is_empty() {
        code_buffer.push_str("    ; Earthang program\n");
        code_buffer.push_str(&main_code);
        code_buffer.push_str("\n");
    }
    
    // CRITICAL: Infinite loop to prevent CPU from running into garbage
    code_buffer.push_str("    ; Program complete - halt CPU\n");
    code_buffer.push_str("    cli\n");
    code_buffer.push_str("    hlt\n");
    code_buffer.push_str("    jmp $\n\n");
    
    // 64-bit utility functions
    code_buffer.push_str("; 64-bit utility functions\n");
    code_buffer.push_str("print_string_64:\n");
    code_buffer.push_str("    ; RSI = string pointer, RDI = VGA buffer position\n");
    code_buffer.push_str("    push rax\n");
    code_buffer.push_str("    push rdi\n");
    code_buffer.push_str("    push rsi\n");
    code_buffer.push_str(".loop_64:\n");
    code_buffer.push_str("    mov al, [rsi]\n");
    code_buffer.push_str("    test al, al\n");
    code_buffer.push_str("    jz .done_64\n");
    code_buffer.push_str("    mov [rdi], al\n");
    code_buffer.push_str("    inc rdi\n");
    code_buffer.push_str("    mov byte [rdi], 0x0F    ; Light gray on black\n");
    code_buffer.push_str("    inc rdi\n");
    code_buffer.push_str("    inc rsi\n");
    code_buffer.push_str("    jmp .loop_64\n");
    code_buffer.push_str(".done_64:\n");
    code_buffer.push_str("    pop rsi\n");
    code_buffer.push_str("    pop rdi\n");
    code_buffer.push_str("    pop rax\n");
    code_buffer.push_str("    ret\n\n");
    
    code_buffer.push_str("print_decimal_64:\n");
    code_buffer.push_str("    ; RAX = number to print\n");
    code_buffer.push_str("    push rbx\n");
    code_buffer.push_str("    push rcx\n");
    code_buffer.push_str("    push rdx\n");
    code_buffer.push_str("    push rdi\n");
    code_buffer.push_str("    \n");
    code_buffer.push_str("    mov rdi, 0xB8000 + 320  ; Third line\n");
    code_buffer.push_str("    mov rbx, 10\n");
    code_buffer.push_str("    mov rcx, 0\n");
    code_buffer.push_str("    \n");
    code_buffer.push_str("    ; Handle zero\n");
    code_buffer.push_str("    test rax, rax\n");
    code_buffer.push_str("    jnz .convert\n");
    code_buffer.push_str("    mov byte [rdi], '0'\n");
    code_buffer.push_str("    inc rdi\n");
    code_buffer.push_str("    mov byte [rdi], 0x0F\n");
    code_buffer.push_str("    jmp .done_decimal\n");
    code_buffer.push_str("    \n");
    code_buffer.push_str(".convert:\n");
    code_buffer.push_str("    xor rdx, rdx\n");
    code_buffer.push_str("    div rbx\n");
    code_buffer.push_str("    add dl, '0'\n");
    code_buffer.push_str("    push rdx\n");
    code_buffer.push_str("    inc rcx\n");
    code_buffer.push_str("    test rax, rax\n");
    code_buffer.push_str("    jnz .convert\n");
    code_buffer.push_str("    \n");
    code_buffer.push_str(".print_digits:\n");
    code_buffer.push_str("    pop rax\n");
    code_buffer.push_str("    mov [rdi], al\n");
    code_buffer.push_str("    inc rdi\n");
    code_buffer.push_str("    mov byte [rdi], 0x0F\n");
    code_buffer.push_str("    inc rdi\n");
    code_buffer.push_str("    loop .print_digits\n");
    code_buffer.push_str("    \n");
    code_buffer.push_str(".done_decimal:\n");
    code_buffer.push_str("    pop rdi\n");
    code_buffer.push_str("    pop rdx\n");
    code_buffer.push_str("    pop rcx\n");
    code_buffer.push_str("    pop rbx\n");
    code_buffer.push_str("    ret\n\n");
    
    // GDT tables
    code_buffer.push_str("; GDT Tables\n");
    code_buffer.push_str("gdt32:\n");
    code_buffer.push_str("    dq 0x0000000000000000    ; Null descriptor\n");
    code_buffer.push_str("    dq 0x00CF9A000000FFFF    ; Code segment (32-bit)\n");
    code_buffer.push_str("    dq 0x00CF92000000FFFF    ; Data segment (32-bit)\n");
    code_buffer.push_str("gdt32_end:\n\n");
    
    code_buffer.push_str("gdt32_desc:\n");
    code_buffer.push_str("    dw gdt32_end - gdt32 - 1\n");
    code_buffer.push_str("    dd gdt32\n\n");
    
    code_buffer.push_str("gdt64:\n");
    code_buffer.push_str("    dq 0x0000000000000000    ; Null descriptor\n");
    code_buffer.push_str("    dq 0x00209A0000000000    ; Code segment (64-bit)\n");
    code_buffer.push_str("    dq 0x0000920000000000    ; Data segment (64-bit)\n");
    code_buffer.push_str("gdt64_end:\n\n");
    
    code_buffer.push_str("gdt64_desc:\n");
    code_buffer.push_str("    dw gdt64_end - gdt64 - 1\n");
    code_buffer.push_str("    dq gdt64\n\n");
    
    // IDT tables (null)
    code_buffer.push_str("idtr_32:\n");
    code_buffer.push_str("    dw 0                     ; Limit (0 = no IDT)\n");
    code_buffer.push_str("    dd 0                     ; Base (null)\n\n");
    
    code_buffer.push_str("idtr_64:\n");
    code_buffer.push_str("    dw 0                     ; Limit (0 = no IDT)\n");
    code_buffer.push_str("    dq 0                     ; Base (null)\n\n");
    
    // Data section
    code_buffer.push_str("; Data section\n");
    code_buffer.push_str("welcome_msg:\n");
    code_buffer.push_str("    db 'Earthang 64-bit Mode', 0\n\n");
    
    // String literals from Earthang program
    if !self.string_literals.is_empty() {
        code_buffer.push_str("; Earthang string literals\n");
        for (string, label) in &self.string_literals {
            code_buffer.push_str(&format!("{}:\n", label));
            code_buffer.push_str(&format!("    db '{}', 0\n", string));
        }
    }
    
    // Variable storage (simplified)
    code_buffer.push_str("\n; Variable storage\n");
    for stmt in &program.body {
        match stmt {
            Statement::VarDecl { name, .. } => {
                code_buffer.push_str(&format!("var_{}:\n", name));
                code_buffer.push_str("    dq 0\n");
            }
            Statement::Assign { target, .. } => {
                code_buffer.push_str(&format!("var_{}:\n", target));
                code_buffer.push_str("    dq 0\n");
            }
            _ => {}
        }
    }
    
    // Pad to ensure all sectors are written
    code_buffer.push_str("\n    ; Pad stage 2 to 16KB\n");
    code_buffer.push_str("    times 16384 - ($ - $$) db 0\n");
    
    // Write the complete code to output
    code.push_str(&code_buffer);
    
    Ok(())
}
    
    // Other BIOS variants
    fn compile_bios64_sse(&mut self, code: &mut String, program: &Program) -> Result<(), String> {
        self.compile_bios64(code, program)?;
        code.push_str("\n    ; SSE enabled\n");
        Ok(())
    }
    
    fn compile_bios64_avx(&mut self, code: &mut String, program: &Program) -> Result<(), String> {
        self.compile_bios64(code, program)?;
        code.push_str("\n    ; AVX enabled\n");
        Ok(())
    }
    
    fn compile_bios64_avx512(&mut self, code: &mut String, program: &Program) -> Result<(), String> {
        self.compile_bios64(code, program)?;
        code.push_str("\n    ; AVX-512 enabled\n");
        Ok(())
    }
    
    // Compilation helpers
    fn compile_bios16_statements(&mut self, code: &mut String, statements: &[Statement]) {
        for stmt in statements {
            match stmt {
                Statement::Expr(expr) => {
                    code.push_str("    ; Expression\n");
                    self.compile_bios16_expression(code, expr);
                }
                Statement::VarDecl { name, value, .. } => {
                    code.push_str("    ; var ");
                    code.push_str(name);
                    code.push_str(" = ");
                    self.compile_bios16_expression(code, value);
                }
                Statement::Assign { target, value, .. } => {
                    code.push_str("    ; ");
                    code.push_str(target);
                    code.push_str(" = ");
                    self.compile_bios16_expression(code, value);
                }
                _ => code.push_str("    ; [Statement]\n"),
            }
        }
    }
    
    fn compile_bios32_statements(&mut self, code: &mut String, statements: &[Statement]) {
        for stmt in statements {
            match stmt {
                Statement::Expr(expr) => {
                    code.push_str("    ; Expression\n");
                    self.compile_bios32_expression(code, expr);
                }
                Statement::VarDecl { name, value, .. } => {
                    code.push_str("    ; var ");
                    code.push_str(name);
                    code.push_str(" = ");
                    self.compile_bios32_expression(code, value);
                }
                Statement::Assign { target, value, .. } => {
                    code.push_str("    ; ");
                    code.push_str(target);
                    code.push_str(" = ");
                    self.compile_bios32_expression(code, value);
                }
                _ => code.push_str("    ; [Statement]\n"),
            }
        }
    }
    
    fn compile_bios64_statements(&mut self, code: &mut String, statements: &[Statement]) -> Result<(), String> {
    for stmt in statements {
        match stmt {
            Statement::Expr(expr) => {
                code.push_str("    ; Expression\n");
                self.compile_bios64_expression(code, expr)?;
            }
            Statement::VarDecl { name, value, .. } => {
                code.push_str("    ; var ");
                code.push_str(name);
                code.push_str(" = ");
                self.compile_bios64_expression(code, value)?;
            }
            Statement::Assign { target, value, .. } => {
                code.push_str("    ; ");
                code.push_str(target);
                code.push_str(" = ");
                self.compile_bios64_expression(code, value)?;
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
                        // Simple comparison like i < 10
                        match (left.as_ref(), comparators.get(0)) {
                            (Expr::Var(var_name, _), Some(Expr::Number(num, _))) => {
                                // Load variable from memory (assume at [rbp - offset])
                                code.push_str(&format!("    ; Load variable {} for comparison\n", var_name));
                                code.push_str("    ; TODO: Implement variable loading\n");
                                
                                // Compare with immediate value
                                code.push_str(&format!("    cmp rax, {}\n", num));
                                
                                // Jump based on comparison
                                match ops[0] {
                                    CompareOp::Lt => {
                                        code.push_str(&format!("    jge {}\n", while_end));
                                    }
                                    CompareOp::Gt => {
                                        code.push_str(&format!("    jle {}\n", while_end));
                                    }
                                    CompareOp::Eq => {
                                        code.push_str(&format!("    jne {}\n", while_end));
                                    }
                                    CompareOp::Ne => {
                                        code.push_str(&format!("    je {}\n", while_end));
                                    }
                                    CompareOp::Le => {
                                        code.push_str(&format!("    jg {}\n", while_end));
                                    }
                                    CompareOp::Ge => {
                                        code.push_str(&format!("    jl {}\n", while_end));
                                    }
                                    _ => {
                                        code.push_str(&format!("    jz {}\n", while_end));
                                    }
                                }
                            }
                            _ => {
                                // Complex comparison
                                code.push_str("    ; Complex comparison condition\n");
                                code.push_str("    ; TODO: Implement complex comparison\n");
                                code.push_str(&format!("    jz {}\n", while_end));
                            }
                        }
                    }
                    Expr::Var(name, _) => {
                        // Variable as condition (non-zero is true)
                        code.push_str(&format!("    ; Variable condition: {}\n", name));
                        code.push_str("    ; TODO: Load variable and test\n");
                        code.push_str("    test rax, rax\n");
                        code.push_str(&format!("    jz {}\n", while_end));
                    }
                    _ => {
                        // Compile complex condition as expression
                        self.compile_bios64_expression(code, condition)?;
                        code.push_str("    test rax, rax\n");
                        code.push_str(&format!("    jz {}\n", while_end));
                    }
                }
                
                // Compile body
                code.push_str("    ; While body\n");
                for body_stmt in body {
                    match body_stmt {
                        Statement::Expr(expr) => {
                            self.compile_bios64_expression(code, expr)?;
                        }
                        Statement::VarDecl { name, value, .. } => {
                            code.push_str(&format!("    ; var {} = ", name));
                            self.compile_bios64_expression(code, value)?;
                        }
                        Statement::Assign { target, value, .. } => {
                            code.push_str(&format!("    ; {} = ", target));
                            self.compile_bios64_expression(code, value)?;
                        }
                        _ => {
                            code.push_str("    ; [Statement in while body]\n");
                        }
                    }
                }
                
                code.push_str(&format!("    jmp {}\n", while_start));
                code.push_str(&format!("{}:\n", while_end));
            }
            _ => code.push_str("    ; [Statement]\n"),
        }
    }
    Ok(())
}
    
    fn compile_bios16_expression(&mut self, code: &mut String, expr: &Expr) {
        match expr {
            Expr::Number(n, _) => {
                self.byte_counter.add(10); // mov ax + call overhead
                code.push_str(&format!("    ; Number: {}\n", n));
                code.push_str(&format!("    mov ax, {}\n", n));
                code.push_str("    call print_decimal\n");
            }
            Expr::String(s, _) => {
                self.byte_counter.add(s.len() + 20); // String overhead
                code.push_str(&format!("    ; String: '{}'\n", s));
                code.push_str("    ; Strings must be in data section\n");
            }
            _ => {
                self.byte_counter.add(5);
                code.push_str("    ; [Expression]\n");
            }
        }
    }
    
    fn compile_bios32_expression(&mut self, code: &mut String, expr: &Expr) {
        match expr {
            Expr::Number(n, _) => {
                self.byte_counter.add(10);
                code.push_str(&format!("    ; Number: {}\n", n));
                code.push_str(&format!("    mov eax, {}\n", n));
                code.push_str("    call print_decimal_32\n");
            }
            Expr::String(s, _) => {
                self.byte_counter.add(s.len() + 20);
                code.push_str(&format!("    ; String: '{}'\n", s));
                code.push_str("    ; Strings must be in data section\n");
            }
            _ => {
                self.byte_counter.add(5);
                code.push_str("    ; [Expression]\n");
            }
        }
    }
    
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
            _ => Err(format!("Unsupported platform for standard compilation: {:?}", self.target.platform))
        }
    }
    
    // Import handling (placeholder - needs to be implemented)
    fn compile_with_imports(&mut self, program: &Program) -> Result<String, String> {
        // For now, just compile without imports
        if self.target.is_bios() {
            self.compile_bios(program)
        } else {
            self.compile_standard(program)
        }
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