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
use crate::backend::{Target, Capability};
use crate::parser::Expr;

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
        args: &[Expr], 
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
        // Load the first instance
        if !self.loaded_modules.contains_key(&name) {
            // TODO use Arc
        }
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
    
    /// Check if a function is available in any module
    pub fn has_function(&self, func: &str) -> bool {
        self.find_module_for_function(func).is_some()
    }
}

/// Math module for earthang
pub struct MathModule {
    name: String,
    description: String,
    functions: Vec<String>,
}

impl MathModule {
    pub fn new() -> Self {
        Self {
            name: "math".to_string(),
            description: "Mathematical functions".to_string(),
            functions: vec![
                "sin".to_string(),
                "cos".to_string(),
                "tan".to_string(),
                "sqrt".to_string(),
                "pow".to_string(),
                "abs".to_string(),
                "floor".to_string(),
                "ceil".to_string(),
                "round".to_string(),
            ],
        }
    }
}

impl EarthngModule for MathModule {
    fn name(&self) -> &str {
        &self.name
    }
    
    fn description(&self) -> &str {
        &self.description
    }
    
    fn functions(&self) -> Vec<&str> {
        self.functions.iter().map(|s| s.as_str()).collect()
    }
    
    fn compile_function(
        &self,
        func: &str,
        args: &[Expr],
        _target: &Target,
        emitter: &mut dyn AssemblyEmitter
    ) -> Result<String, String> {
        let mut asm = emitter.emit_comment(&format!("Math.{} call", func));
        
        // Convert arguments to string representation
        let _arg_strings: Vec<String> = args.iter()
            .map(|arg| match arg {
                Expr::Number(n, _) => n.to_string(),
                Expr::Var(name, _) => name.clone(),
                _ => "0".to_string(),
            })
            .collect();
        
        // For now, just emit a placeholder
        match func {
            "abs" => {
                asm.push_str(&emitter.emit_comment("Absolute value"));
                asm.push_str("    ; TODO: Implement abs\n");
                asm.push_str("    mov rax, 0\n");
            }
            "sqrt" => {
                asm.push_str(&emitter.emit_comment("Square root"));
                asm.push_str("    ; TODO: Implement sqrt\n");
                asm.push_str("    mov rax, 0\n");
            }
            _ => {
                asm.push_str(&emitter.emit_comment(&format!("Math function {} not implemented", func)));
                asm.push_str("    mov rax, 0\n");
            }
        }
        
        Ok(asm)
    }
    
    fn init(&mut self, _capabilities: &[Capability]) {
        // Math module doesn't require special initialization
    }
}

/// String module for earthang
pub struct StringModule {
    name: String,
    description: String,
    functions: Vec<String>,
}

impl StringModule {
    pub fn new() -> Self {
        Self {
            name: "string".to_string(),
            description: "String manipulation functions".to_string(),
            functions: vec![
                "length".to_string(),
                "concat".to_string(),
                "substr".to_string(),
                "find".to_string(),
                "replace".to_string(),
                "to_upper".to_string(),
                "to_lower".to_string(),
                "trim".to_string(),
            ],
        }
    }
}

impl EarthngModule for StringModule {
    fn name(&self) -> &str {
        &self.name
    }
    
    fn description(&self) -> &str {
        &self.description
    }
    
    fn functions(&self) -> Vec<&str> {
        self.functions.iter().map(|s| s.as_str()).collect()
    }
    
    fn compile_function(
        &self,
        func: &str,
        _args: &[Expr],
        _target: &Target,
        emitter: &mut dyn AssemblyEmitter
    ) -> Result<String, String> {
        let mut asm = emitter.emit_comment(&format!("String.{} call", func));
        
        match func {
            "length" => {
                asm.push_str(&emitter.emit_comment("String length"));
                asm.push_str("    ; TODO: Implement string length\n");
                asm.push_str("    mov rax, 0\n");
            }
            "concat" => {
                asm.push_str(&emitter.emit_comment("String concatenation"));
                asm.push_str("    ; TODO: Implement string concatenation\n");
                asm.push_str("    mov rax, 0\n");
            }
            _ => {
                asm.push_str(&emitter.emit_comment(&format!("String function {} not implemented", func)));
                asm.push_str("    mov rax, 0\n");
            }
        }
        
        Ok(asm)
    }
    
    fn init(&mut self, _capabilities: &[Capability]) {
        // String module doesn't require special initialization
    }
}

/// System module for earthang
pub struct SystemModule {
    name: String,
    description: String,
    functions: Vec<String>,
}

impl SystemModule {
    pub fn new() -> Self {
        Self {
            name: "system".to_string(),
            description: "System-level functions".to_string(),
            functions: vec![
                "time".to_string(),
                "sleep".to_string(),
                "exit".to_string(),
                "getenv".to_string(),
                "platform".to_string(),
            ],
        }
    }
}

impl EarthngModule for SystemModule {
    fn name(&self) -> &str {
        &self.name
    }
    
    fn description(&self) -> &str {
        &self.description
    }
    
    fn functions(&self) -> Vec<&str> {
        self.functions.iter().map(|s| s.as_str()).collect()
    }
    
    fn compile_function(
        &self,
        func: &str,
        _args: &[Expr],
        target: &Target,
        emitter: &mut dyn AssemblyEmitter
    ) -> Result<String, String> {
        let mut asm = emitter.emit_comment(&format!("System.{} call", func));
        
        match func {
            "exit" => {
                asm.push_str(&emitter.emit_comment("Exit program"));
                // Since we only have Linux64 target, handle it directly
                asm.push_str("    mov rax, 60         ; sys_exit\n");
                asm.push_str("    mov rdi, 0          ; exit code\n");
                asm.push_str("    syscall\n");
            }
            "time" => {
                asm.push_str(&emitter.emit_comment("Get current time"));
                asm.push_str("    ; TODO: Implement time function\n");
                asm.push_str("    mov rax, 0\n");
            }
            _ => {
                asm.push_str(&emitter.emit_comment(&format!("System function {} not implemented", func)));
                asm.push_str("    mov rax, 0\n");
            }
        }
        
        Ok(asm)
    }
    
    fn init(&mut self, capabilities: &[Capability]) {
        // System module might check for specific capabilities
        if capabilities.contains(&Capability::Linux) {
            // Linux-specific initialization
        }
    }
}