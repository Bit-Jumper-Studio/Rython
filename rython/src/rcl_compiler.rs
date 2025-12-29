use std::collections::{HashMap, HashSet};
use std::path::Path;
use crate::parser::{Program, Statement, Expr, Position, Span};
use serde::{Serialize, Deserialize};

/// RCL File Format Version
const RCL_VERSION: &str = "1.0.0";

/// RCL Library Metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RclMetadata {
    pub name: String,
    pub version: String,
    pub author: Option<String>,
    pub description: Option<String>,
    pub dependencies: Vec<String>,
    pub exports: Vec<String>,  // List of exported symbols
    pub target: String,        // e.g., "bios64", "linux64"
    pub rcl_version: String,
    pub capabilities: Vec<String>, // Required capabilities
}

/// Compiled Library Entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RclEntry {
    Function(RclFunction),
    Variable(RclVariable),
    Type(RclType),
    Assembly(RclAssembly),
    Constant(RclConstant),
}

/// Compiled Function
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RclFunction {
    pub name: String,
    pub signature: String,  // e.g., "int add(int, int)"
    pub parameters: Vec<(String, String)>, // (name, type)
    pub return_type: String,
    pub ast: Option<Program>,  // Original AST (for inline expansion)
    pub assembly: Option<String>, // Target assembly
    pub inlineable: bool,
    pub pure: bool,  // No side effects
    pub capabilities: Vec<String>,
}

/// Compiled Variable
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RclVariable {
    pub name: String,
    pub value: String, // Serialized value
    pub type_name: String,
    pub constant: bool,
    pub export: bool,
}

/// Compiled Type Definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RclType {
    pub name: String,
    pub fields: Vec<(String, String)>, // (field_name, type_name)
    pub size: usize,
    pub alignment: usize,
}

/// Pre-compiled Assembly
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RclAssembly {
    pub label: String,
    pub code: String,
    pub target: String,
    pub dependencies: Vec<String>,
    pub capabilities: Vec<String>,
}

/// Compiled Constant
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RclConstant {
    pub name: String,
    pub value_type: String,
    pub value: String,
    pub export: bool,
}

/// RCL Library File
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RclLibrary {
    pub metadata: RclMetadata,
    pub entries: Vec<RclEntry>,
    pub imports: Vec<String>,
    pub symbol_table: HashMap<String, String>, // name -> entry_type
}

/// RCL Compiler
pub struct RclCompiler {
    pub library: RclLibrary,
    symbol_table: HashMap<String, RclEntry>,
    current_target: String,
    export_all: bool,
    export_requests: Vec<String>,
}

impl RclCompiler {
    pub fn new(name: &str, target: &str) -> Self {
        Self {
            library: RclLibrary {
                metadata: RclMetadata {
                    name: name.to_string(),
                    version: "1.0.0".to_string(),
                    author: None,
                    description: None,
                    dependencies: Vec::new(),
                    exports: Vec::new(),
                    target: target.to_string(),
                    rcl_version: RCL_VERSION.to_string(),
                    capabilities: Vec::new(),
                },
                entries: Vec::new(),
                imports: Vec::new(),
                symbol_table: HashMap::new(),
            },
            symbol_table: HashMap::new(),
            current_target: target.to_string(),
            export_all: false,
            export_requests: Vec::new(),
        }
    }
    
    /// Set export all mode
    pub fn export_all(&mut self) {
        self.export_all = true;
    }
    
    /// Compile a Rython program to RCL
    pub fn compile_program(&mut self, program: &Program) -> Result<(), String> {
        // Process all statements
        for stmt in &program.body {
            self.compile_statement(stmt)?;
        }
        
        // Process export requests after all symbols are defined
        for export_name in &self.export_requests {
            if let Some(entry) = self.symbol_table.get_mut(export_name) {
                Self::mark_exported(entry);
            } else {
                return Err(format!("Cannot export undefined symbol: {}", export_name));
            }
        }
        
        // Update exports list
        self.library.metadata.exports = self.symbol_table.keys()
            .filter(|name| {
                if let Some(entry) = self.symbol_table.get(*name) {
                    match entry {
                        RclEntry::Function(f) => f.name.starts_with("export_") || self.export_all,
                        RclEntry::Variable(v) => v.export,
                        RclEntry::Constant(c) => c.export,
                        _ => false,
                    }
                } else {
                    false
                }
            })
            .cloned()
            .collect();
        
        Ok(())
    }
    
    fn compile_statement(&mut self, stmt: &Statement) -> Result<(), String> {
        match stmt {
            Statement::VarDecl { name, value, type_hint: _, span: _ } => {
                self.compile_variable(name, value)?;
            }
            Statement::FunctionDef { name, args, body, span: _ } => {
                self.compile_function(name, args, body)?;
            }
            Statement::Expr(expr) => {
                // Handle export directives first
                if let Expr::Call { func, args, kwargs: _, span: _ } = expr {
                    if func == "export" && !args.is_empty() {
                        if let Expr::String(s, _) = &args[0] {
                            // Store export request for later processing
                            self.export_requests.push(s.clone());
                            return Ok(());
                        }
                    }
                }
                
                // Compile as constant expression if possible
                if let Some(constant) = self.expr_to_constant(expr)? {
                    let entry = RclEntry::Constant(constant);
                    let name = match &entry {
                        RclEntry::Constant(c) => c.name.clone(),
                        _ => unreachable!(),
                    };
                    self.symbol_table.insert(name.clone(), entry.clone());
                    self.library.entries.push(entry);
                }
            }
            Statement::Assign { target, value, span: _ } => {
                self.compile_assignment(target, value)?;
            }
            _ => {
                // Skip other statements for library compilation
            }
        }
        Ok(())
    }
    
    fn compile_variable(&mut self, name: &str, value: &Expr) -> Result<(), String> {
        let value_str = self.expr_to_string(value)?;
        let type_name = "auto".to_string(); // Simplified type inference
        
        let export = self.export_all || name.starts_with("EXPORT_");
        
        let entry = RclEntry::Variable(RclVariable {
            name: name.to_string(),
            value: value_str,
            type_name,
            constant: true,
            export,
        });
        
        self.symbol_table.insert(name.to_string(), entry.clone());
        self.library.entries.push(entry);
        
        Ok(())
    }
    
    fn compile_assignment(&mut self, target: &str, value: &Expr) -> Result<(), String> {
        let value_str = self.expr_to_string(value)?;
        
        let entry = RclEntry::Variable(RclVariable {
            name: target.to_string(),
            value: value_str,
            type_name: "auto".to_string(),
            constant: false,
            export: self.export_all,
        });
        
        self.symbol_table.insert(target.to_string(), entry.clone());
        self.library.entries.push(entry);
        
        Ok(())
    }
    
    fn compile_function(&mut self, name: &str, args: &[String], body: &[Statement]) -> Result<(), String> {
        // Create a minimal program from the function body
        let func_program = Program {
            body: body.to_vec(),
            span: Span::single(Position::start()), // Fixed: Added missing span field
        };
        
        // Determine function signature
        let params: Vec<(String, String)> = args.iter()
            .map(|arg| (arg.clone(), "auto".to_string()))
            .collect();
        
        // Determine if function is pure (no side effects)
        let pure = self.is_pure_function(body);
        
        // Determine if function should be inlined
        let inlineable = self.should_inline_function(body);
        
        // Generate assembly for the function
        let assembly = self.generate_function_assembly(name, args, body)?;
        
        
        let entry = RclEntry::Function(RclFunction {
            name: name.to_string(),
            signature: format!("{}({})", name, args.join(", ")),
            parameters: params,
            return_type: "auto".to_string(),
            ast: Some(func_program),
            assembly: Some(assembly),
            inlineable,
            pure,
            capabilities: Vec::new(),
        });
        
        self.symbol_table.insert(name.to_string(), entry.clone());
        self.library.entries.push(entry);
        
        Ok(())
    }
    
    fn generate_function_assembly(&self, name: &str, args: &[String], _body: &[Statement]) -> Result<String, String> {
        let mut asm = String::new();
        
        asm.push_str(&format!("; RCL Function: {}\n", name));
        asm.push_str(&format!("{}:\n", name));
        
        // Prologue based on target
        match self.current_target.as_str() {
            "bios64" | "bios64_sse" | "bios64_avx" | "bios64_avx512" => {
                asm.push_str("    push rbp\n");
                asm.push_str("    mov rbp, rsp\n");
                
                // Save parameters
                for (i, arg) in args.iter().enumerate() {
                    let offset = (i * 8) + 16; // Skip saved RBP and return address
                    asm.push_str(&format!("    ; Parameter: {} at [rbp+{}]\n", arg, offset));
                }
            }
            "linux64" => {
                asm.push_str("    push rbp\n");
                asm.push_str("    mov rbp, rsp\n");
                
                // Linux System V AMD64 ABI
                for (i, arg) in args.iter().take(6).enumerate() {
                    let reg = match i {
                        0 => "rdi",
                        1 => "rsi",
                        2 => "rdx",
                        3 => "rcx",
                        4 => "r8",
                        5 => "r9",
                        _ => continue,
                    };
                    asm.push_str(&format!("    ; Parameter: {} in {}\n", arg, reg));
                }
            }
            "bios32" => {
                asm.push_str("    push ebp\n");
                asm.push_str("    mov ebp, esp\n");
            }
            "bios16" => {
                asm.push_str("    push bp\n");
                asm.push_str("    mov bp, sp\n");
            }
            _ => {}
        }
        
        // Function body (simplified)
        asm.push_str("    ; Function body\n");
        
        // Add return at the end
        asm.push_str("    ; Return\n");
        match self.current_target.as_str() {
            "bios64" | "bios64_sse" | "bios64_avx" | "bios64_avx512" | "linux64" => {
                asm.push_str("    xor rax, rax\n");
            }
            "bios32" => {
                asm.push_str("    xor eax, eax\n");
            }
            "bios16" => {
                asm.push_str("    xor ax, ax\n");
            }
            _ => {}
        }
        
        // Epilogue
        match self.current_target.as_str() {
            "bios64" | "bios64_sse" | "bios64_avx" | "bios64_avx512" | "linux64" => {
                asm.push_str("    mov rsp, rbp\n");
                asm.push_str("    pop rbp\n");
                asm.push_str("    ret\n");
            }
            "bios32" => {
                asm.push_str("    mov esp, ebp\n");
                asm.push_str("    pop ebp\n");
                asm.push_str("    ret\n");
            }
            "bios16" => {
                asm.push_str("    mov sp, bp\n");
                asm.push_str("    pop bp\n");
                asm.push_str("    ret\n");
            }
            _ => {
                asm.push_str("    ret\n");
            }
        }
        
        Ok(asm)
    }
    
    fn expr_to_string(&self, expr: &Expr) -> Result<String, String> {
        match expr {
            Expr::Number(n, _) => Ok(n.to_string()),
            Expr::Float(f, _) => Ok(f.to_string()),
            Expr::Boolean(b, _) => Ok(b.to_string()),
            Expr::String(s, _) => Ok(s.clone()),
            Expr::BinOp { left, op, right, span: _ } => {
                let left_str = self.expr_to_string(left)?;
                let right_str = self.expr_to_string(right)?;
                let op_str = match op {
                    crate::parser::Op::Add => "+",
                    crate::parser::Op::Sub => "-",
                    crate::parser::Op::Mul => "*",
                    crate::parser::Op::Div => "/",
                    crate::parser::Op::Mod => "%",
                    crate::parser::Op::Pow => "**",
                    crate::parser::Op::FloorDiv => "//",
                };
                Ok(format!("({} {} {})", left_str, op_str, right_str))
            }
            _ => Err("Cannot serialize expression to string".to_string()),
        }
    }
    
    fn expr_to_constant(&self, expr: &Expr) -> Result<Option<RclConstant>, String> {
        // Only simple expressions can be constants
        match expr {
            Expr::Number(n, _) => Ok(Some(RclConstant {
                name: format!("const_{}", n),
                value_type: "int".to_string(),
                value: n.to_string(),
                export: false,
            })),
            Expr::String(s, _) => Ok(Some(RclConstant {
                name: format!("str_{}", s.replace(|c: char| !c.is_alphanumeric(), "_")),
                value_type: "string".to_string(),
                value: s.clone(),
                export: false,
            })),
            _ => Ok(None),
        }
    }
    
    #[allow(unused_variables)]
    fn is_pure_function(&self, body: &[Statement]) -> bool {
        // Simple purity analysis
        for stmt in body {
            match stmt {
                Statement::Expr(Expr::Call { func, args: _, kwargs: _, span: _ }) => {
                    // Functions with side effects
                    if func == "print" || func == "input" || func.starts_with("sys_") {
                        return false;
                    }
                }
                Statement::Assign { .. } => return false,
                Statement::AugAssign { .. } => return false,
                Statement::Return(_) => continue,
                _ => {
                    // Check nested statements
                    match stmt {
                        Statement::If { then_block, elif_blocks, else_block, .. } => {
                            if !self.is_pure_function(then_block) {
                                return false;
                            }
                            for (_, block) in elif_blocks {
                                if !self.is_pure_function(block) {
                                    return false;
                                }
                            }
                            if let Some(block) = else_block {
                                if !self.is_pure_function(block) {
                                    return false;
                                }
                            }
                        }
                        Statement::While { body: loop_body, .. } => {
                            if !self.is_pure_function(loop_body) {
                                return false;
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        true
    }
    
    fn should_inline_function(&self, body: &[Statement]) -> bool {
        // Small functions should be inlined
        let statement_count = body.len();
        let estimated_size = self.estimate_code_size(body);
        
        statement_count <= 5 && estimated_size < 100
    }
    
    fn estimate_code_size(&self, body: &[Statement]) -> usize {
        let mut size = 0;
        for stmt in body {
            size += match stmt {
                Statement::Expr(_) => 10,
                Statement::VarDecl { .. } => 15,
                Statement::Assign { .. } => 10,
                Statement::If { then_block, elif_blocks, else_block, .. } => {
                    5 + self.estimate_code_size(then_block)
                    + elif_blocks.iter().map(|(_, b)| self.estimate_code_size(b)).sum::<usize>()
                    + else_block.as_ref().map_or(0, |b| self.estimate_code_size(b))
                }
                Statement::While { body: loop_body, .. } => 10 + self.estimate_code_size(loop_body),
                _ => 5,
            };
        }
        size
    }
    
    fn mark_exported(entry: &mut RclEntry) {
        // Mark as exported - implementation depends on entry type
        match entry {
            RclEntry::Function(func) => {
                // Function is already marked during creation
                let _ = func;
            }
            RclEntry::Variable(var) => {
                var.export = true;
            }
            RclEntry::Constant(constant) => {
                constant.export = true;
            }
            _ => {}
        }
    }
    
    /// Finalize compilation
    pub fn finalize(&mut self) -> Result<(), String> {
        // Build symbol table
        for entry in &self.library.entries {
            let (name, entry_type) = match entry {
                RclEntry::Function(f) => (f.name.clone(), "function".to_string()),
                RclEntry::Variable(v) => (v.name.clone(), "variable".to_string()),
                RclEntry::Constant(c) => (c.name.clone(), "constant".to_string()),
                RclEntry::Type(t) => (t.name.clone(), "type".to_string()),
                RclEntry::Assembly(a) => (a.label.clone(), "assembly".to_string()),
            };
            self.library.symbol_table.insert(name, entry_type);
        }
        
        Ok(())
    }
    
    /// Save RCL library to file
    pub fn save_to_file(&self, path: &str) -> Result<(), String> {
        let rcl_text = serde_json::to_string_pretty(&self.library)
            .map_err(|e| format!("Failed to serialize RCL: {}", e))?;
        
        std::fs::write(path, rcl_text)
            .map_err(|e| format!("Failed to write RCL file: {}", e))?;
        
        Ok(())
    }
    
    /// Load RCL library from file
    pub fn load_from_file(path: &str) -> Result<RclLibrary, String> {
        let rcl_text = std::fs::read_to_string(path)
            .map_err(|e| format!("Failed to read RCL file: {}", e))?;
        
        let library: RclLibrary = serde_json::from_str(&rcl_text)
            .map_err(|e| format!("Failed to parse RCL file: {}", e))?;
        
        // Check version compatibility
        if library.metadata.rcl_version != RCL_VERSION {
            println!("Warning: RCL version mismatch (file: {}, current: {})",
                library.metadata.rcl_version, RCL_VERSION);
        }
        
        Ok(library)
    }
    
    /// Create a simple RCL library with basic functions
    pub fn create_stdlib(target: &str) -> Result<RclLibrary, String> {
        let source_code = r#"
def add(a, b): return a + b
def subtract(a, b): return a - b
def multiply(a, b): return a * b
def divide(a, b): return a / b
var PI = 3.141592653589793
var E = 2.718281828459045
"#;
        
        let program = crate::parser::parse_program(source_code)
            .map_err(|e| format!("Parse error: {:?}", e))?;
        
        let mut compiler = Self::new("rython_stdlib", target);
        compiler.export_all();
        compiler.compile_program(&program)?;
        compiler.finalize()?;
        
        Ok(compiler.library)
    }
}

/// RCL Import Manager - Handles RCL imports in Rython code
pub struct RclImportManager {
    loaded_libraries: HashMap<String, RclLibrary>,
    import_paths: Vec<String>,
    imported_symbols: HashSet<String>,
}

impl RclImportManager {
    pub fn new() -> Self {
        Self {
            loaded_libraries: HashMap::new(),
            import_paths: vec![
                "./".to_string(),
                "./lib/".to_string(),
                "./rcl/".to_string(),
                "./stdlib/".to_string(),
            ],
            imported_symbols: HashSet::new(),
        }
    }
    
    /// Add import path
    pub fn add_import_path(&mut self, path: &str) {
        self.import_paths.push(path.to_string());
    }
    
    /// Import a library
    pub fn import_library(&mut self, lib_name: &str) -> Result<(), String> {
        // Check if already loaded
        if self.loaded_libraries.contains_key(lib_name) {
            return Ok(());
        }
        
        // Try to load standard library first
        if lib_name == "stdlib" || lib_name == "rython_stdlib" {
            let stdlib = RclCompiler::create_stdlib("bios64")?;
            self.loaded_libraries.insert("stdlib".to_string(), stdlib);
            return Ok(());
        }
        
        // Search for library file
        for path in &self.import_paths {
            let possible_paths = vec![
                format!("{}/{}.rcl", path, lib_name),
                format!("{}/lib_{}.rcl", path, lib_name),
                format!("{}/{}/lib.rcl", path, lib_name),
                format!("{}/{}", path, lib_name),
            ];
            
            for file_path in possible_paths {
                if Path::new(&file_path).exists() {
                    let library = RclCompiler::load_from_file(&file_path)?;
                    self.loaded_libraries.insert(lib_name.to_string(), library);
                    return Ok(());
                }
            }
        }
        
        Err(format!("Library '{}' not found in import paths", lib_name))
    }
    
    /// Check if a symbol is available from loaded libraries
    pub fn resolve_symbol(&self, symbol: &str) -> Option<&RclEntry> {
        for library in self.loaded_libraries.values() {
            for entry in &library.entries {
                match entry {
                    RclEntry::Function(f) if f.name == symbol => return Some(entry),
                    RclEntry::Variable(v) if v.name == symbol => return Some(entry),
                    RclEntry::Constant(c) if c.name == symbol => return Some(entry),
                    RclEntry::Type(t) if t.name == symbol => return Some(entry),
                    _ => continue,
                }
            }
        }
        None
    }
    
    /// Get assembly for a symbol
    pub fn get_assembly(&self, symbol: &str) -> Option<String> {
        for library in self.loaded_libraries.values() {
            for entry in &library.entries {
                match entry {
                    RclEntry::Function(f) if f.name == symbol => {
                        return f.assembly.clone();
                    }
                    RclEntry::Assembly(a) if a.label == symbol => {
                        return Some(a.code.clone());
                    }
                    _ => continue,
                }
            }
        }
        None
    }
    
    /// Expand imports in Rython code
    pub fn expand_imports(&mut self, program: &Program) -> Result<Program, String> {
        let mut expanded_statements = Vec::new();
        
        for stmt in &program.body {
            match stmt {
                Statement::Expr(Expr::Call { func, args, kwargs: _, span: _ }) if func == "import" => {
                    // Handle import statement
                    if let Some(Expr::String(lib_name, _)) = args.get(0) {
                        println!("Importing library: {}", lib_name);
                        // Library is already loaded by the compiler
                    }
                }
                Statement::Expr(Expr::Call { func, args, kwargs: _, span: _ }) if func == "from" => {
                    // Handle: from "library" import symbol
                    if let (Some(Expr::String(lib_name, _)), Some(Expr::Call { func: import_func, args: import_args, kwargs: _, span: _ })) = 
                        (args.get(0), args.get(1)) {
                        if import_func == "import" {
                            if let Some(Expr::String(symbol, _)) = import_args.get(0) {
                                println!("Importing {} from {}", symbol, lib_name);
                                // Mark symbol as imported
                                self.imported_symbols.insert(symbol.clone());
                            }
                        }
                    }
                }
                _ => {
                    expanded_statements.push(stmt.clone());
                }
            }
        }
        
        Ok(Program {
            body: expanded_statements,
            span: program.span, // Fixed: Use the original program's span
        })
    }
    
    /// Get all loaded libraries
    pub fn get_loaded_libraries(&self) -> Vec<&RclLibrary> {
        self.loaded_libraries.values().collect()
    }
    
    /// Clear all loaded libraries
    pub fn clear(&mut self) {
        self.loaded_libraries.clear();
        self.imported_symbols.clear();
    }
}

/// Simple import resolver that automatically finds and includes libraries
pub struct AutoImportResolver {
    search_paths: Vec<String>,
    cache: HashMap<String, RclLibrary>,
}

impl AutoImportResolver {
    pub fn new() -> Self {
        Self {
            search_paths: vec![
                "./".to_string(),
                "./lib/".to_string(),
                "./rcl/".to_string(),
                "./libraries/".to_string(),
                std::env::current_dir()
                    .unwrap_or_default()
                    .join("lib")
                    .to_string_lossy()
                    .to_string(),
            ],
            cache: HashMap::new(),
        }
    }
    
    pub fn add_search_path(&mut self, path: &str) {
        self.search_paths.push(path.to_string());
    }
    
    pub fn resolve_import(&mut self, name: &str) -> Result<Option<RclLibrary>, String> {
        // Check cache first
        if let Some(lib) = self.cache.get(name) {
            return Ok(Some(lib.clone()));
        }
        
        // Search for library
        for path in &self.search_paths {
            let possible_files = vec![
                format!("{}/{}.rcl", path, name),
                format!("{}/lib_{}.rcl", path, name),
                format!("{}/{}", path, name),
            ];
            
            for file in possible_files {
                if std::path::Path::new(&file).exists() {
                    match RclCompiler::load_from_file(&file) {
                        Ok(library) => {
                            self.cache.insert(name.to_string(), library.clone());
                            return Ok(Some(library));
                        }
                        Err(e) => {
                            println!("Warning: Failed to load library {}: {}", file, e);
                        }
                    }
                }
            }
        }
        
        Ok(None)
    }
    
    pub fn extract_imports(&self, program: &Program) -> Vec<String> {
        let mut imports = Vec::new();
        
        for stmt in &program.body {
            if let Statement::Expr(Expr::Call { func, args, kwargs: _, span: _ }) = stmt {
                if func == "import" && !args.is_empty() {
                    if let Expr::String(name, _) = &args[0] {
                        imports.push(name.clone());
                    }
                }
            }
        }
        
        imports
    }
    
    pub fn generate_import_assembly(&mut self, program: &Program, target: &str) -> Result<String, String> {
        let imports = self.extract_imports(program);
        let mut all_asm = String::new();
        
        for import in imports {
            if let Some(library) = self.resolve_import(&import)? {
                let generator = RclAssemblyGenerator::new(target);
                all_asm.push_str(&format!("; Library: {}\n", library.metadata.name));
                all_asm.push_str(&generator.generate_from_library(&library));
                all_asm.push_str("\n");
            } else {
                return Err(format!("Could not find library: {}", import));
            }
        }
        
        Ok(all_asm)
    }
    
    pub fn create_program_without_imports(&self, program: &Program) -> Program {
        let mut filtered = Vec::new();
        
        for stmt in &program.body {
            match stmt {
                Statement::Expr(Expr::Call { func, .. }) if func == "import" => {
                    // Skip import statements
                }
                _ => {
                    filtered.push(stmt.clone());
                }
            }
        }
        
        Program { 
            body: filtered,
            span: program.span // Fixed: Use the original program's span
        }
    }
}

/// RCL Assembly Generator - Converts RCL entries to assembly
pub struct RclAssemblyGenerator {
    target: String,
    indent: String,
}

impl RclAssemblyGenerator {
    pub fn new(target: &str) -> Self {
        Self {
            target: target.to_string(),
            indent: "    ".to_string(),
        }
    }
    
    pub fn generate_from_library(&self, library: &RclLibrary) -> String {
        let mut asm = String::new();
        
        // Header
        asm.push_str(&format!("; RCL Library: {}\n", library.metadata.name));
        asm.push_str(&format!("; Version: {}\n", library.metadata.version));
        asm.push_str(&format!("; Target: {}\n\n", library.metadata.target));
        
        // Generate assembly for each entry
        for entry in &library.entries {
            match entry {
                RclEntry::Function(func) => {
                    if let Some(func_asm) = &func.assembly {
                        asm.push_str(func_asm);
                        asm.push_str("\n");
                    }
                }
                RclEntry::Assembly(asm_entry) => {
                    if asm_entry.target == self.target {
                        asm.push_str(&format!("; Assembly: {}\n", asm_entry.label));
                        asm.push_str(&asm_entry.code);
                        asm.push_str("\n");
                    }
                }
                RclEntry::Variable(var) if var.export => {
                    asm.push_str(&format!("; Exported variable: {}\n", var.name));
                    asm.push_str(&format!("{}:\n", var.name));
                    match var.type_name.as_str() {
                        "int" | "auto" => {
                            asm.push_str(&format!("{}    dq {}\n", self.indent, var.value));
                        }
                        "string" => {
                            asm.push_str(&format!("{}    db '{}', 0\n", self.indent, var.value));
                        }
                        _ => {
                            asm.push_str(&format!("{}    ; Variable of type {}\n", self.indent, var.type_name));
                        }
                    }
                    asm.push_str("\n");
                }
                _ => {}
            }
        }
        
        asm
    }
}

/// Create a minimal RCL library for testing
pub fn create_test_library() -> RclLibrary {
    let metadata = RclMetadata {
        name: "test_lib".to_string(),
        version: "1.0.0".to_string(),
        author: Some("Rython Test".to_string()),
        description: Some("Test library for RCL system".to_string()),
        dependencies: Vec::new(),
        exports: vec!["add".to_string(), "MATH_PI".to_string()],
        target: "bios64".to_string(),
        rcl_version: RCL_VERSION.to_string(),
        capabilities: Vec::new(),
    };
    
    let entries = vec![
        RclEntry::Function(RclFunction {
            name: "add".to_string(),
            signature: "add(a, b)".to_string(),
            parameters: vec![
                ("a".to_string(), "int".to_string()),
                ("b".to_string(), "int".to_string()),
            ],
            return_type: "int".to_string(),
            ast: None,
            assembly: Some(
                "add:\n\
                    push rbp\n\
                    mov rbp, rsp\n\
                    mov rax, [rbp+16]\n\
                    add rax, [rbp+24]\n\
                    pop rbp\n\
                    ret\n".to_string()
            ),
            inlineable: true,
            pure: true,
            capabilities: Vec::new(),
        }),
        RclEntry::Variable(RclVariable {
            name: "MATH_PI".to_string(),
            value: "3.14159".to_string(),
            type_name: "float".to_string(),
            constant: true,
            export: true,
        }),
    ];
    
    let mut symbol_table = HashMap::new();
    symbol_table.insert("add".to_string(), "function".to_string());
    symbol_table.insert("MATH_PI".to_string(), "variable".to_string());
    
    RclLibrary {
        metadata,
        entries,
        imports: Vec::new(),
        symbol_table,
    }
}