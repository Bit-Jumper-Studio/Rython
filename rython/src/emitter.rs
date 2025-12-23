// src/emitter.rs - Complete rewrite to use the sophisticated parser
//! NASM Assembly Emitter for Rython AST - Full AST processing

use crate::parser::{Program, Statement, Expr, Op, UnaryOp, BoolOp};
use std::collections::HashMap;

// ========== TARGET CONFIGURATION ==========

#[derive(Debug, Clone, PartialEq)]
pub enum TargetPlatform {
    Linux64,      // Linux x86-64 (ELF64)
    Windows64,    // Windows x64 (PE64)  
    Bios,         // BIOS bootloader (16-bit real mode)
}

#[derive(Debug, Clone)]
pub struct TargetConfig {
    pub platform: TargetPlatform,
    pub bits: u8,
    pub format: &'static str,
    pub entry_point: &'static str,
}

impl TargetConfig {
    pub fn linux64() -> Self {
        Self {
            platform: TargetPlatform::Linux64,
            bits: 64,
            format: "elf64",
            entry_point: "_start",
        }
    }
    
    pub fn windows64() -> Self {
        Self {
            platform: TargetPlatform::Windows64,
            bits: 64,
            format: "win64",
            entry_point: "main",
        }
    }
    
    pub fn bios() -> Self {
        Self {
            platform: TargetPlatform::Bios,
            bits: 16,
            format: "bin",
            entry_point: "_start",
        }
    }
    
    pub fn is_windows(&self) -> bool {
        self.platform == TargetPlatform::Windows64
    }
    
    pub fn is_bios(&self) -> bool {
        self.platform == TargetPlatform::Bios
    }
    
    pub fn is_linux(&self) -> bool {
        self.platform == TargetPlatform::Linux64
    }
}

// ========== NASM EMITTER WITH FULL AST SUPPORT ==========

pub struct NasmEmitter {
    target: TargetConfig,
    label_counter: u32,
    variable_offsets: HashMap<String, i32>,
}

impl NasmEmitter {
    pub fn new() -> Self {
        Self {
            target: TargetConfig::linux64(),
            label_counter: 0,
            variable_offsets: HashMap::new(),
        }
    }
    
    // ========== TARGET CONFIGURATION METHODS ==========
    
    pub fn set_target_linux(&mut self) {
        self.target = TargetConfig::linux64();
    }
    
    pub fn set_target_windows(&mut self) {
        self.target = TargetConfig::windows64();
    }
    
    pub fn set_target_bios(&mut self) {
        self.target = TargetConfig::bios();
    }
    
    // ========== MAIN COMPILATION METHODS ==========
    
    pub fn compile_program(&mut self, program: &Program) -> String {
        // Reset state
        self.label_counter = 0;
        self.variable_offsets.clear();
        
        // Generate code based on target
        match self.target.platform {
            TargetPlatform::Bios => self.compile_bios(program),
            _ => self.compile_standard(program),
        }
    }
    
    fn compile_bios(&mut self, program: &Program) -> String {
        let mut code = String::new();
        
        // BIOS bootloader header
        code.push_str("; ============================================\n");
        code.push_str("; Starting RF");
        code.push_str("; ============================================\n\n");
        
        code.push_str("    org 0x7C00\n");
        code.push_str("    bits 16\n\n");
        
        // Start with initialization
        code.push_str("start:\n");
        code.push_str("    ; Initialize segments and stack\n");
        code.push_str("    cli\n");
        code.push_str("    xor ax, ax\n");
        code.push_str("    mov ds, ax\n");
        code.push_str("    mov es, ax\n");
        code.push_str("    mov ss, ax\n");
        code.push_str("    mov sp, 0x7C00\n");
        code.push_str("    sti\n");
        code.push_str("    cld\n\n");
        
        // Clear screen
        code.push_str("    ; Clear screen\n");
        code.push_str("    mov ax, 0x0003\n");
        code.push_str("    int 0x10\n\n");
        
        // Print welcome message
        code.push_str("    ; Print welcome message\n");
        code.push_str("    mov si, msg_welcome\n");
        code.push_str("    call print_string\n\n");
        
        // Process the program AST
        code.push_str("    ; ===== User Program Starts =====\n");
        self.compile_bios_statements(&mut code, &program.body);
        
        // Halt the system
        code.push_str("\n    ; Halt system\n");
        code.push_str("    mov si, msg_halt\n");
        code.push_str("    call print_string\n");
        code.push_str("halt_loop:\n");
        code.push_str("    hlt\n");
        code.push_str("    jmp short halt_loop\n\n");
        
        // ========== SUBROUTINES ==========
        self.add_bios_subroutines(&mut code);
        
        // ========== DATA SECTION ==========
        self.add_bios_data(&mut code);
        
        // ========== BOOT SIGNATURE ==========
        code.push_str("\n    ; Boot signature\n");
        code.push_str("    times 510-($-$$) db 0\n");
        code.push_str("    dw 0xAA55\n");
        
        code
    }
    
    fn compile_bios_statements(&mut self, code: &mut String, statements: &[Statement]) {
        for stmt in statements {
            match stmt {
                Statement::Expr(expr) => {
                    self.compile_bios_expression(code, expr);
                }
                Statement::VarDecl { name, value, type_hint: _ } => {
                    code.push_str(&format!("    ; var {} = ", name));
                    // In BIOS, we can't easily store variables, so just evaluate
                    self.compile_bios_expression(code, value);
                    code.push_str("    ; (value evaluated)\n");
                }
                Statement::Assign { target, value } => {
                    code.push_str(&format!("    ; {} = ", target));
                    self.compile_bios_expression(code, value);
                }
                Statement::Return(expr) => {
                    if let Some(expr) = expr {
                        code.push_str("    ; return ");
                        self.compile_bios_expression(code, expr);
                    } else {
                        code.push_str("    ; return\n");
                    }
                }
                Statement::If { condition, then_block, elif_blocks, else_block } => {
                    let label = self.new_label("if");
                    code.push_str(&format!("    ; if\n"));
                    self.compile_bios_expression(code, condition);
                    code.push_str(&format!("    ; condition evaluated - if true, jump to .{}\n", label));
                    // TODO: Implement proper conditional jump
                    code.push_str(&format!(".{}:\n", label));
                    self.compile_bios_statements(code, then_block);
                    
                    for (elif_cond, elif_body) in elif_blocks {
                        let elif_label = self.new_label("elif");
                        code.push_str(&format!("    ; elif\n"));
                        self.compile_bios_expression(code, elif_cond);
                        code.push_str(&format!("    ; jump to .{}\n", elif_label));
                        code.push_str(&format!(".{}:\n", elif_label));
                        self.compile_bios_statements(code, elif_body);
                    }
                    
                    if let Some(else_body) = else_block {
                        let else_label = self.new_label("else");
                        code.push_str(&format!("    ; else\n"));
                        code.push_str(&format!(".{}:\n", else_label));
                        self.compile_bios_statements(code, else_body);
                    }
                    
                    let end_label = self.new_label("if_end");
                    code.push_str(&format!(".{}:\n", end_label));
                }
                Statement::While { condition, body, orelse: _ } => {
                    let start_label = self.new_label("while_start");
                    let end_label = self.new_label("while_end");
                    
                    code.push_str(&format!(".{}:\n", start_label));
                    code.push_str("    ; while condition\n");
                    self.compile_bios_expression(code, condition);
                    code.push_str(&format!("    ; if false, jump to .{}\n", end_label));
                    
                    code.push_str("    ; while body\n");
                    self.compile_bios_statements(code, body);
                    code.push_str(&format!("    jmp .{}\n", start_label));
                    
                    code.push_str(&format!(".{}:\n", end_label));
                }
                Statement::FunctionDef { name, args, body } => {
                    code.push_str(&format!("    ; function {}(", name));
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            code.push_str(", ");
                        }
                        code.push_str(arg);
                    }
                    code.push_str(")\n");
                    code.push_str(&format!("{}:\n", name));
                    self.compile_bios_statements(code, body);
                    code.push_str("    ret\n\n");
                }
                Statement::Pass => {
                    code.push_str("    ; pass\n");
                }
                Statement::Break => {
                    code.push_str("    ; break\n");
                    // TODO: Implement break
                }
                Statement::Continue => {
                    code.push_str("    ; continue\n");
                    // TODO: Implement continue
                }
                Statement::AugAssign { target, op, value } => {
                    code.push_str(&format!("    ; {} {}=", target, self.op_to_str(op)));
                    self.compile_bios_expression(code, value);
                }
            }
        }
    }
    
    fn compile_bios_expression(&mut self, code: &mut String, expr: &Expr) {
        match expr {
            Expr::Number(n) => {
                // For BIOS, we might want to print or use the number
                code.push_str(&format!("    ; Number: {}\n", n));
                code.push_str(&format!("    mov ax, {}\n", n));
                code.push_str("    call print_decimal\n");
                code.push_str("    mov si, msg_newline\n");
                code.push_str("    call print_string\n");
            }
            Expr::Float(f) => {
                code.push_str(&format!("    ; Float: {}\n", f));
                // BIOS doesn't have floating point easily, just note it
            }
            Expr::Boolean(b) => {
                code.push_str(&format!("    ; Boolean: {}\n", b));
            }
            Expr::String(s) => {
                code.push_str(&format!("    ; String: \"{}\"\n", s));
                // In BIOS, we could print it
                let label = self.new_label("str");
                code.push_str(&format!("    mov si, .{}\n", label));
                code.push_str("    call print_string\n");
                // Store the string in data section (we'll collect these later)
            }
            Expr::Var(name) => {
                code.push_str(&format!("    ; Variable: {}\n", name));
            }
            Expr::None => {
                code.push_str("    ; None\n");
            }
            Expr::BinOp { left, op, right } => {
                code.push_str(&format!("    ; Binary operation: {} {} {}\n", 
                    self.expr_to_str(left), self.op_to_str(op), self.expr_to_str(right)));
                // Evaluate left and right
                self.compile_bios_expression(code, left);
                // Store left result somewhere (e.g., push to stack)
                code.push_str("    push ax\n");
                self.compile_bios_expression(code, right);
                code.push_str("    mov bx, ax\n");
                code.push_str("    pop ax\n");
                
                // Perform operation
                match op {
                    Op::Add => code.push_str("    add ax, bx\n"),
                    Op::Sub => code.push_str("    sub ax, bx\n"),
                    Op::Mul => code.push_str("    imul ax, bx\n"),
                    Op::Div => {
                        code.push_str("    xor dx, dx\n");
                        code.push_str("    div bx\n");  // AX = (DX:AX) / BX, DX = remainder
                    }
                    _ => code.push_str("    ; operation not implemented in BIOS\n"),
                }
            }
            Expr::UnaryOp { op, operand } => {
                code.push_str(&format!("    ; Unary operation: {:?}\n", op));
                self.compile_bios_expression(code, operand);
                match op {
                    UnaryOp::Minus => code.push_str("    neg ax\n"),
                    UnaryOp::Not => code.push_str("    not ax\n"),
                    _ => code.push_str("    ; unary op not implemented\n"),
                }
            }
            Expr::BoolOp { op, values } => {
                code.push_str(&format!("    ; Boolean operation: {:?} with {} values\n", op, values.len()));
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        code.push_str(&format!("    ; {} operation\n", match op {
                            BoolOp::And => "AND",
                            BoolOp::Or => "OR",
                        }));
                    }
                    self.compile_bios_expression(code, value);
                }
            }
            Expr::Compare { left, ops, comparators } => {
                code.push_str(&format!("    ; Compare: {} ops\n", ops.len()));
                self.compile_bios_expression(code, left);
                for (i, (op, comparator)) in ops.iter().zip(comparators).enumerate() {
                    if i > 0 {
                        code.push_str("    ; and\n");
                    }
                    self.compile_bios_expression(code, comparator);
                    code.push_str(&format!("    ; compare {:?}\n", op));
                }
            }
            Expr::Call { func, args, kwargs } => {
                code.push_str(&format!("    ; Call {}({} args", func, args.len()));
                if !kwargs.is_empty() {
                    code.push_str(&format!(", {} kwargs", kwargs.len()));
                }
                code.push_str(")\n");
                
                // Handle specific functions
                match func.as_str() {
                    "print_int" => {
                        if !args.is_empty() {
                            self.compile_bios_expression(code, &args[0]);
                            code.push_str("    call print_decimal\n");
                            code.push_str("    mov si, msg_newline\n");
                            code.push_str("    call print_string\n");
                        }
                    }
                    "call_bios" => {
                        code.push_str("    ; BIOS call\n");
                        code.push_str("    call custom_bios_function\n");
                    }
                    _ => {
                        // Generic function call
                        for (i, arg) in args.iter().enumerate() {
                            code.push_str(&format!("    ; arg {}: ", i));
                            self.compile_bios_expression(code, arg);
                        }
                        code.push_str(&format!("    call {}\n", func));
                    }
                }
            }
        }
    }
    
    fn op_to_str(&self, op: &Op) -> &str {
        match op {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Mod => "%",
            Op::Pow => "**",
            Op::FloorDiv => "//",
        }
    }
    
    fn expr_to_str(&self, expr: &Expr) -> String {
        match expr {
            Expr::Number(n) => format!("{}", n),
            Expr::Float(f) => format!("{}", f),
            Expr::Boolean(b) => format!("{}", b),
            Expr::String(s) => format!("\"{}\"", s),
            Expr::Var(name) => name.clone(),
            Expr::None => "None".to_string(),
            _ => "...".to_string(),
        }
    }
    
    fn new_label(&mut self, prefix: &str) -> String {
        let label = format!(".L{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }
    
    fn add_bios_subroutines(&mut self, code: &mut String) {
        code.push_str("; ===== BIOS Subroutines =====\n\n");
        
        // print_string: Print null-terminated string at SI
        code.push_str("print_string:\n");
        code.push_str("    push ax\n");
        code.push_str("    push si\n");
        code.push_str(".ps_loop:\n");
        code.push_str("    lodsb\n");
        code.push_str("    or al, al\n");
        code.push_str("    jz .ps_done\n");
        code.push_str("    mov ah, 0x0E\n");
        code.push_str("    int 0x10\n");
        code.push_str("    jmp .ps_loop\n");
        code.push_str(".ps_done:\n");
        code.push_str("    pop si\n");
        code.push_str("    pop ax\n");
        code.push_str("    ret\n\n");
        
        // print_decimal: Print AX as decimal
        code.push_str("print_decimal:\n");
        code.push_str("    push ax\n");
        code.push_str("    push bx\n");
        code.push_str("    push cx\n");
        code.push_str("    push dx\n");
        code.push_str("    push si\n");
        code.push_str("    \n");
        code.push_str("    xor cx, cx\n");
        code.push_str("    mov bx, 10\n");
        code.push_str(".pd_divide:\n");
        code.push_str("    xor dx, dx\n");
        code.push_str("    div bx\n");
        code.push_str("    add dl, '0'\n");
        code.push_str("    push dx\n");
        code.push_str("    inc cx\n");
        code.push_str("    test ax, ax\n");
        code.push_str("    jnz .pd_divide\n");
        code.push_str("    \n");
        code.push_str(".pd_print:\n");
        code.push_str("    pop ax\n");
        code.push_str("    mov ah, 0x0E\n");
        code.push_str("    int 0x10\n");
        code.push_str("    loop .pd_print\n");
        code.push_str("    \n");
        code.push_str("    pop si\n");
        code.push_str("    pop dx\n");
        code.push_str("    pop cx\n");
        code.push_str("    pop bx\n");
        code.push_str("    pop ax\n");
        code.push_str("    ret\n\n");
        
        // print_hex_word: Print AX as hexadecimal
        code.push_str("print_hex_word:\n");
        code.push_str("    push ax\n");
        code.push_str("    push bx\n");
        code.push_str("    push cx\n");
        code.push_str("    push dx\n");
        code.push_str("    \n");
        code.push_str("    mov cx, 4\n");
        code.push_str(".phw_digit:\n");
        code.push_str("    rol ax, 4\n");
        code.push_str("    mov dx, ax\n");
        code.push_str("    and dx, 0x000F\n");
        code.push_str("    add dl, '0'\n");
        code.push_str("    cmp dl, '9'\n");
        code.push_str("    jbe .phw_print\n");
        code.push_str("    add dl, 7\n");
        code.push_str(".phw_print:\n");
        code.push_str("    mov ah, 0x0E\n");
        code.push_str("    int 0x10\n");
        code.push_str("    loop .phw_digit\n");
        code.push_str("    \n");
        code.push_str("    pop dx\n");
        code.push_str("    pop cx\n");
        code.push_str("    pop bx\n");
        code.push_str("    pop ax\n");
        code.push_str("    ret\n\n");
        
        // custom_bios_function: Example BIOS function
        code.push_str("custom_bios_function:\n");
        code.push_str("    ; Custom BIOS function\n");
        code.push_str("    mov si, msg_bios\n");
        code.push_str("    call print_string\n");
        code.push_str("    ret\n\n");
    }
    
    fn add_bios_data(&mut self, code: &mut String) {
        code.push_str("; ===== Data Section =====\n");
        code.push_str("msg_welcome:\n");
        code.push_str("    db 'Rython BIOS Bootloader', 13, 10\n");
        code.push_str("    db '=======================', 13, 10, 13, 10, 0\n");
        code.push_str("\n");
        code.push_str("msg_halt:\n");
        code.push_str("    db 13, 10, 'Program complete. System halted.', 13, 10, 0\n");
        code.push_str("\n");
        code.push_str("msg_newline:\n");
        code.push_str("    db 13, 10, 0\n");
        code.push_str("\n");
        code.push_str("msg_bios:\n");
        code.push_str("    db 'BIOS function called!', 13, 10, 0\n");
    }
    
    fn compile_standard(&mut self, _program: &Program) -> String {
        // For now, just return a placeholder
        String::from("; Standard compilation not yet implemented\n")
    }
}

// ========== PUBLIC INTERFACE ==========

/// Legacy function for backward compatibility
pub fn compile_to_nasm(program: &Program) -> String {
    let mut emitter = NasmEmitter::new();
    emitter.compile_program(program)
}