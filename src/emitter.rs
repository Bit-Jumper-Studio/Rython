use std::collections::HashMap;
use crate::parser::{Program, Statement, Expr}; 


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
                    // Compile expression
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
                        Expr::Call { func, args: _, kwargs: _, span: _ } if func == "print" => {
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
                _ => {
                    code.push_str("    ; [Statement]\n");
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
        code.push_str("; Earthang 64-bit Bootloader - Fixed Version\n\n");
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
        
        // Simple A20 enable
        code.push_str("    in al, 0x92\n");
        code.push_str("    or al, 2\n");
        code.push_str("    out 0x92, al\n\n");
        
        // Load 32-bit GDT
        code.push_str("    lgdt [gdt32_desc]\n\n");
        
        // Enter protected mode
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
        code.push_str("    mov esp, 0x90000\n\n");
        
        // Setup paging (32-bit code)
        code.push_str("    ; Setup paging\n");
        code.push_str("    mov edi, 0x1000\n");
        code.push_str("    mov cr3, edi\n");
        code.push_str("    xor eax, eax\n");
        code.push_str("    mov ecx, 4096\n");
        code.push_str("    rep stosd\n");
        
        code.push_str("    mov edi, 0x1000\n");
        code.push_str("    mov dword [edi], 0x2003\n");
        code.push_str("    add edi, 0x1000\n");
        code.push_str("    mov dword [edi], 0x3003\n");
        code.push_str("    add edi, 0x1000\n");
        
        code.push_str("    mov ebx, 0x00000083\n");
        code.push_str("    mov ecx, 512\n");
        code.push_str(".set_entry:\n");
        code.push_str("    mov dword [edi], ebx\n");
        code.push_str("    add ebx, 0x200000\n");
        code.push_str("    add edi, 8\n");
        code.push_str("    loop .set_entry\n\n");
        
        // Enable PAE
        code.push_str("    mov eax, cr4\n");
        code.push_str("    or eax, (1 << 5)\n");
        code.push_str("    mov cr4, eax\n\n");
        
        // Set CR3
        code.push_str("    mov eax, 0x1000\n");
        code.push_str("    mov cr3, eax\n\n");
        
        // Enable long mode
        code.push_str("    mov ecx, 0xC0000080\n");
        code.push_str("    rdmsr\n");
        code.push_str("    or eax, (1 << 8)\n");
        code.push_str("    wrmsr\n\n");
        
        // Enable paging
        code.push_str("    mov eax, cr0\n");
        code.push_str("    or eax, (1 << 31)\n");
        code.push_str("    mov cr0, eax\n\n");
        
        // Load 64-bit GDT
        code.push_str("    lgdt [gdt64_desc]\n\n");
        
        // Jump to 64-bit mode
        code.push_str("    jmp 0x08:long_mode\n\n");
        
        code.push_str("    bits 64\n");
        code.push_str("long_mode:\n");
        
        code.push_str("    mov ax, 0x10\n");
        code.push_str("    mov ds, ax\n");
        code.push_str("    mov es, ax\n");
        code.push_str("    mov fs, ax\n");
        code.push_str("    mov gs, ax\n");
        code.push_str("    mov ss, ax\n");
        code.push_str("    mov rsp, 0x90000\n\n");
        
        // Clear screen (64-bit)
        code.push_str("    mov rdi, 0xB8000\n");
        code.push_str("    mov rax, 0x0720072007200720\n");
        code.push_str("    mov rcx, 1000\n");
        code.push_str("    rep stosq\n\n");
        
        // Print message (64-bit) - FIXED: Load from fixed data segment
        code.push_str("    mov rsi, 0x7E00  ; String data starts after 512 bytes\n");
        code.push_str("    mov rdi, 0xB8000\n");
        code.push_str("    call print_string_64\n\n");
        
        // Compile program
        self.compile_bios64_statements(code, &program.body)?;
        
        code.push_str("\n    cli\n");
        code.push_str("    hlt\n");
        code.push_str("    jmp $\n\n");
        
        code.push_str("print_string_64:\n");
        code.push_str("    push rdi\n");
        code.push_str(".loop:\n");
        code.push_str("    mov al, [rsi]\n");
        code.push_str("    test al, al\n");
        code.push_str("    jz .done\n");
        code.push_str("    stosb\n");
        code.push_str("    mov al, 0x0F\n");
        code.push_str("    stosb\n");
        code.push_str("    inc rsi\n");
        code.push_str("    jmp .loop\n");
        code.push_str(".done:\n");
        code.push_str("    pop rdi\n");
        code.push_str("    ret\n\n");
        
        // Add missing print_decimal_64 function
        code.push_str("print_decimal_64:\n");
        code.push_str("    ; Print decimal number in RAX\n");
        code.push_str("    push rdi\n");
        code.push_str("    push rcx\n");
        code.push_str("    push rdx\n");
        code.push_str("    push rbx\n");
        code.push_str("    \n");
        code.push_str("    mov rdi, 0xB8000 + 160  ; Second line\n");
        code.push_str("    mov rcx, 0\n");
        code.push_str("    mov rbx, 10\n");
        code.push_str(".div_loop:\n");
        code.push_str("    xor rdx, rdx\n");
        code.push_str("    div rbx\n");
        code.push_str("    push dx\n");
        code.push_str("    inc rcx\n");
        code.push_str("    test rax, rax\n");
        code.push_str("    jnz .div_loop\n");
        code.push_str(".print_loop:\n");
        code.push_str("    pop ax\n");
        code.push_str("    add al, '0'\n");
        code.push_str("    stosb\n");
        code.push_str("    mov al, 0x0F\n");
        code.push_str("    stosb\n");
        code.push_str("    loop .print_loop\n");
        code.push_str("    \n");
        code.push_str("    pop rbx\n");
        code.push_str("    pop rdx\n");
        code.push_str("    pop rcx\n");
        code.push_str("    pop rdi\n");
        code.push_str("    ret\n\n");
        
        code.push_str("gdt32:\n");
        code.push_str("    dq 0x0000000000000000\n");
        code.push_str("    dq 0x00CF9A000000FFFF\n");
        code.push_str("    dq 0x00CF92000000FFFF\n");
        code.push_str("gdt32_end:\n\n");
        
        code.push_str("gdt32_desc:\n");
        code.push_str("    dw gdt32_end - gdt32 - 1\n");
        code.push_str("    dd gdt32\n\n");
        
        code.push_str("gdt64:\n");
        code.push_str("    dq 0x0000000000000000\n");
        code.push_str("    dq 0x00209A0000000000\n");
        code.push_str("    dq 0x0000920000000000\n");
        code.push_str("gdt64_end:\n\n");
        
        code.push_str("gdt64_desc:\n");
        code.push_str("    dw gdt64_end - gdt64 - 1\n");
        code.push_str("    dq gdt64\n\n");
        
        // Boot signature
        code.push_str("    times 510-($-$$) db 0\n");
        code.push_str("    dw 0xAA55\n\n");
        
        code.push_str("    ; String data at 0x7E00 (after boot sector)\n");
        code.push_str("    times 512 db 0  ; Boot sector padding\n");
        code.push_str("    db 'Earthang 64-bit', 0\n");
        
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