use mlua::{Lua, Table, Error as LuaError};
use serde::{Serialize, Deserialize};
use std::collections::HashMap;

// Re-export types for backward compatibility
pub use crate::backend::{Target, Capability};

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Position {
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Self { line, column, offset }
    }
    
    pub fn start() -> Self {
        Self { line: 1, column: 1, offset: 0 }
    }
    
    pub fn advance(&mut self, c: char) {
        self.offset += c.len_utf8();
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
    
    pub fn single(pos: Position) -> Self {
        Self { start: pos, end: pos }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start.line == self.end.line {
            if self.start.column == self.end.column {
                write!(f, "{}:{}", self.start.line, self.start.column)
            } else {
                write!(f, "{}:{}-{}", self.start.line, self.start.column, self.end.column)
            }
        } else {
            write!(f, "{}:{}-{}:{}", self.start.line, self.start.column, self.end.line, self.end.column)
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    LuaError(String),
    SyntaxError {
        message: String,
        span: Span,
        help: Option<String>,
        context: Option<String>,
    },
}

impl ParseError {
    pub fn lua_error(message: impl Into<String>) -> Self {
        ParseError::LuaError(message.into())
    }
    
    pub fn syntax_error(message: impl Into<String>, span: Span) -> Self {
        ParseError::SyntaxError {
            message: message.into(),
            span,
            help: None,
            context: None,
        }
    }
    
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        match &mut self {
            ParseError::SyntaxError { help: h, .. } => *h = Some(help.into()),
            _ => {}
        }
        self
    }
    
    pub fn span(&self) -> Option<Span> {
        match self {
            ParseError::SyntaxError { span, .. } => Some(*span),
            _ => None,
        }
    }
    
    pub fn format_error(&self, source: &str) -> String {
        match self {
            ParseError::LuaError(message) => {
                format!("Lua parsing error: {}", message)
            }
            ParseError::SyntaxError { message, span, help, context } => {
                self.format_detailed("syntax error", message, span, help.as_deref(), context.as_deref(), source)
            }
        }
    }
    
    fn format_detailed(&self, error_type: &str, message: &str, span: &Span, help: Option<&str>, context: Option<&str>, source: &str) -> String {
        use colored::*;
        
        let mut output = String::new();
        
        // Error header
        output.push_str(&format!("{}: {}: {}\n", 
            error_type.red().bold(),
            span.to_string().cyan(),
            message.bold()));
        
        // Extract relevant source lines
        if let Some(source_context) = self.extract_source_context(source, span) {
            output.push_str(&source_context);
            output.push('\n');
        }
        
        // Print context if available
        if let Some(context) = context {
            output.push_str(&format!("  {} {}\n", "context:".dimmed(), context));
        }
        
        // Print help if available
        if let Some(help) = help {
            output.push_str(&format!("  {} {}\n", "help:".green().bold(), help.green()));
        }
        
        output
    }
    
    fn extract_source_context(&self, source: &str, span: &Span) -> Option<String> {
        let lines: Vec<&str> = source.lines().collect();
        
        if span.start.line == 0 || span.start.line > lines.len() {
            return None;
        }
        
        let start_line_idx = span.start.line.saturating_sub(1);
        let end_line_idx = span.end.line.saturating_sub(1);
        
        let mut context = String::new();
        
        // Show up to 2 lines before and after
        let context_start = start_line_idx.saturating_sub(2);
        let context_end = std::cmp::min(end_line_idx + 2, lines.len() - 1);
        
        for i in context_start..=context_end {
            let line_num = i + 1;
            let line = lines[i];
            
            // Line number
            context.push_str(&format!("{:4} │ {}\n", line_num, line));
            
            // Error underline if this line contains the error
            if i >= start_line_idx && i <= end_line_idx {
                let line_start_col = if i == start_line_idx { span.start.column } else { 1 };
                let line_end_col = if i == end_line_idx { span.end.column } else { line.len() + 1 };
                
                // Add underline
                context.push_str("     │ ");
                for _ in 1..line_start_col {
                    context.push(' ');
                }
                for _ in line_start_col..=line_end_col {
                    context.push('^');
                }
                context.push('\n');
            }
        }
        
        Some(context)
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::LuaError(message) => {
                write!(f, "Lua parsing error: {}", message)
            }
            ParseError::SyntaxError { message, span, .. } => {
                write!(f, "syntax error at {}: {}", span, message)
            }
        }
    }
}

impl std::error::Error for ParseError {}

impl From<LuaError> for ParseError {
    fn from(err: LuaError) -> Self {
        ParseError::LuaError(err.to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Op {
    Add, Sub, Mul, Div, Mod, Pow, FloorDiv,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CompareOp {
    Eq, Ne, Lt, Le, Gt, Ge, In, NotIn, Is, IsNot,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BoolOp {
    And, Or,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnaryOp {
    Not, Plus, Minus, Invert,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expr {
    Number(i64, Span),
    Float(f64, Span),
    Boolean(bool, Span),
    String(String, Span),
    Var(String, Span),
    None(Span),
    BinOp { left: Box<Expr>, op: Op, right: Box<Expr>, span: Span },
    UnaryOp { op: UnaryOp, operand: Box<Expr>, span: Span },
    BoolOp { op: BoolOp, values: Vec<Expr>, span: Span },
    Compare { left: Box<Expr>, ops: Vec<CompareOp>, comparators: Vec<Expr>, span: Span },
    Call { func: String, args: Vec<Expr>, kwargs: HashMap<String, Expr>, span: Span },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Number(_, span) => *span,
            Expr::Float(_, span) => *span,
            Expr::Boolean(_, span) => *span,
            Expr::String(_, span) => *span,
            Expr::Var(_, span) => *span,
            Expr::None(span) => *span,
            Expr::BinOp { span, .. } => *span,
            Expr::UnaryOp { span, .. } => *span,
            Expr::BoolOp { span, .. } => *span,
            Expr::Compare { span, .. } => *span,
            Expr::Call { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    VarDecl { name: String, value: Expr, type_hint: Option<String>, span: Span },
    Assign { target: String, value: Expr, span: Span },
    AugAssign { target: String, op: Op, value: Expr, span: Span },
    Expr(Expr),
    Return(Option<Expr>),
    If { condition: Expr, then_block: Vec<Statement>, elif_blocks: Vec<(Expr, Vec<Statement>)>, else_block: Option<Vec<Statement>>, span: Span },
    While { condition: Expr, body: Vec<Statement>, orelse: Option<Vec<Statement>>, span: Span },
    FunctionDef { name: String, args: Vec<String>, body: Vec<Statement>, span: Span },
    HardwareFunctionDef { 
        device: String, 
        name: String, 
        args: Vec<String>, 
        body: Vec<Statement>, 
        span: Span,
    },
    Pass,
    Break,
    Continue,
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::VarDecl { span, .. } => *span,
            Statement::Assign { span, .. } => *span,
            Statement::AugAssign { span, .. } => *span,
            Statement::Expr(expr) => expr.span(),
            Statement::Return(expr) => expr.as_ref().map_or(Span::single(Position::new(0, 0, 0)), |e| e.span()),
            Statement::If { span, .. } => *span,
            Statement::While { span, .. } => *span,
            Statement::FunctionDef { span, .. } => *span,
            Statement::HardwareFunctionDef { span, .. } => *span,
            Statement::Pass => Span::single(Position::new(0, 0, 0)),
            Statement::Break => Span::single(Position::new(0, 0, 0)),
            Statement::Continue => Span::single(Position::new(0, 0, 0)),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    pub body: Vec<Statement>,
    pub span: Span,
}

pub struct LuaFrontend {
    lua_pool: crate::lua_pool::LuaPool,
}

impl LuaFrontend {
    pub fn new() -> Self {
        Self {
            lua_pool: crate::lua_pool::LuaPool::new(10, 100),
        }
    }
    
    pub fn parse_program(&self, source: &str) -> Result<Program, ParseError> {
        let lua = self.lua_pool.get_instance()
            .ok_or_else(|| ParseError::lua_error("Failed to get Lua instance from pool"))?;
        
        // Define the Lua parser script
        let parser_script = r#"
-- Lua-based Earthang Parser
local parser = {}

-- Token types
local TokenType = {
    KEYWORD = 1,
    IDENTIFIER = 2,
    NUMBER = 3,
    STRING = 4,
    OPERATOR = 5,
    PUNCTUATION = 6,
    EOF = 7,
}

-- Keywords
local keywords = {
    ["var"] = true,
    ["if"] = true,
    ["elif"] = true,
    ["else"] = true,
    ["while"] = true,
    ["for"] = true,
    ["in"] = true,
    ["return"] = true,
    ["def"] = true,
    ["and"] = true,
    ["or"] = true,
    ["not"] = true,
    ["pass"] = true,
    ["break"] = true,
    ["continue"] = true,
    ["True"] = true,
    ["False"] = true,
    ["None"] = true,
}

-- Operators
local operators = {
    ["+"] = "ADD",
    ["-"] = "SUB",
    ["*"] = "MUL",
    ["/"] = "DIV",
    ["%"] = "MOD",
    ["**"] = "POW",
    ["=="] = "EQ",
    ["!="] = "NE",
    ["<"] = "LT",
    ["<="] = "LE",
    [">"] = "GT",
    [">="] = "GE",
    ["="] = "ASSIGN",
    ["+="] = "ADD_ASSIGN",
    ["-="] = "SUB_ASSIGN",
    ["*="] = "MUL_ASSIGN",
    ["/="] = "DIV_ASSIGN",
}

-- Lexer
function parser.lex(source)
    local tokens = {}
    local pos = 1
    local line = 1
    local col = 1
    local start_pos = 1
    local start_line = 1
    local start_col = 1
    
    local function add_token(type, value, line, col, length)
        table.insert(tokens, {
            type = type,
            value = value,
            line = line,
            col = col,
            length = length or #value
        })
    end
    
    local function skip_whitespace()
        while pos <= #source do
            local c = source:sub(pos, pos)
            if c == ' ' or c == '\t' then
                pos = pos + 1
                col = col + 1
            elseif c == '\n' then
                pos = pos + 1
                line = line + 1
                col = 1
            elseif c == '#' then
                -- Skip comment
                while pos <= #source and source:sub(pos, pos) ~= '\n' do
                    pos = pos + 1
                end
            else
                break
            end
        end
    end
    
    while pos <= #source do
        skip_whitespace()
        if pos > #source then break end
        
        start_pos = pos
        start_line = line
        start_col = col
        
        local c = source:sub(pos, pos)
        
        -- Numbers
        if c:match('%d') then
            local num_str = ''
            local is_float = false
            
            while pos <= #source do
                local ch = source:sub(pos, pos)
                if ch:match('%d') then
                    num_str = num_str .. ch
                    pos = pos + 1
                    col = col + 1
                elseif ch == '.' and source:sub(pos + 1, pos + 1):match('%d') then
                    is_float = true
                    num_str = num_str .. ch
                    pos = pos + 1
                    col = col + 1
                else
                    break
                end
            end
            
            if is_float then
                add_token(TokenType.NUMBER, tonumber(num_str), start_line, start_col, #num_str)
            else
                add_token(TokenType.NUMBER, tonumber(num_str), start_line, start_col, #num_str)
            end
        
        -- Strings
        elseif c == '"' or c == "'" then
            local quote = c
            pos = pos + 1
            col = col + 1
            local str = ''
            local escape = false
            
            while pos <= #source do
                local ch = source:sub(pos, pos)
                if escape then
                    if ch == 'n' then str = str .. '\n'
                    elseif ch == 't' then str = str .. '\t'
                    elseif ch == 'r' then str = str .. '\r'
                    elseif ch == '\\' then str = str .. '\\'
                    elseif ch == '"' then str = str .. '"'
                    elseif ch == "'" then str = str .. "'"
                    else str = str .. ch end
                    escape = false
                elseif ch == '\\' then
                    escape = true
                elseif ch == quote then
                    pos = pos + 1
                    col = col + 1
                    break
                else
                    str = str .. ch
                end
                pos = pos + 1
                col = col + 1
            end
            
            add_token(TokenType.STRING, str, start_line, start_col, #str + 2)
        
        -- Identifiers and keywords
        elseif c:match('[%a_]') then
            local ident = ''
            while pos <= #source do
                local ch = source:sub(pos, pos)
                if ch:match('[%w_]') then
                    ident = ident .. ch
                    pos = pos + 1
                    col = col + 1
                else
                    break
                end
            end
            
            if keywords[ident] then
                add_token(TokenType.KEYWORD, ident, start_line, start_col, #ident)
            else
                add_token(TokenType.IDENTIFIER, ident, start_line, start_col, #ident)
            end
        
        -- Operators
        else
            -- Check for multi-character operators
            local two_char = source:sub(pos, pos + 1)
            if operators[two_char] then
                add_token(TokenType.OPERATOR, two_char, line, col, 2)
                pos = pos + 2
                col = col + 2
            elseif operators[c] then
                add_token(TokenType.OPERATOR, c, line, col, 1)
                pos = pos + 1
                col = col + 1
            
            -- Punctuation
            elseif c == '(' or c == ')' or c == '{' or c == '}' or 
                   c == '[' or c == ']' or c == ',' or c == ':' or 
                   c == ';' or c == '.' or c == '@' then
                add_token(TokenType.PUNCTUATION, c, line, col, 1)
                pos = pos + 1
                col = col + 1
            
            -- Unknown character
            else
                error("Unexpected character: " .. c .. " at line " .. line .. ", col " .. col)
            end
        end
    end
    
    -- Add EOF token
    add_token(TokenType.EOF, "EOF", line, col, 0)
    
    return tokens
end

-- Parser
function parser.parse(tokens)
    local pos = 1
    
    local function current()
        return tokens[pos]
    end
    
    local function peek()
        return tokens[pos + 1]
    end
    
    local function consume(type, value)
        local token = current()
        if token.type == type and (value == nil or token.value == value) then
            pos = pos + 1
            return token
        else
            error("Expected " .. (value or type) .. ", got " .. token.value)
        end
    end
    
    local function match(type, value)
        local token = current()
        if token.type == type and (value == nil or token.value == value) then
            pos = pos + 1
            return true
        end
        return false
    end
    
    -- First, forward declare all expression parsing functions
    local parse_expression
    local parse_logical_or
    local parse_logical_and
    local parse_comparison
    local parse_addition
    local parse_multiplication
    local parse_unary
    local parse_primary
    
    -- Expression parsing - define functions in reverse order of dependency
    parse_primary = function()
        local token = current()
        
        if token.type == TokenType.NUMBER then
            consume(TokenType.NUMBER)
            return {
                type = "Number",
                value = token.value
            }
        
        elseif token.type == TokenType.STRING then
            consume(TokenType.STRING)
            return {
                type = "String",
                value = token.value
            }
        
        elseif token.type == TokenType.IDENTIFIER then
            consume(TokenType.IDENTIFIER)
            
            -- Check if it's a function call
            if match(TokenType.PUNCTUATION, "(") then
                local args = {}
                
                if not match(TokenType.PUNCTUATION, ")") then
                    repeat
                        table.insert(args, parse_expression())
                    until not match(TokenType.PUNCTUATION, ",")
                    consume(TokenType.PUNCTUATION, ")")
                end
                
                return {
                    type = "Call",
                    func = token.value,
                    args = args
                }
            else
                return {
                    type = "Var",
                    name = token.value
                }
            end
        
        elseif token.type == TokenType.KEYWORD then
            if token.value == "True" then
                consume(TokenType.KEYWORD)
                return {type = "Boolean", value = true}
            elseif token.value == "False" then
                consume(TokenType.KEYWORD)
                return {type = "Boolean", value = false}
            elseif token.value == "None" then
                consume(TokenType.KEYWORD)
                return {type = "None"}
            end
        
        elseif match(TokenType.PUNCTUATION, "(") then
            local expr = parse_expression()
            consume(TokenType.PUNCTUATION, ")")
            return expr
        
        else
            error("Unexpected token: " .. token.value)
        end
    end
    
    parse_unary = function()
        if match(TokenType.OPERATOR, "-") then
            return {
                type = "UnaryOp",
                op = "-",
                operand = parse_unary()
            }
        elseif match(TokenType.OPERATOR, "+") then
            return {
                type = "UnaryOp",
                op = "+",
                operand = parse_unary()
            }
        elseif match(TokenType.KEYWORD, "not") then
            return {
                type = "UnaryOp",
                op = "not",
                operand = parse_unary()
            }
        else
            return parse_primary()
        end
    end
    
    parse_multiplication = function()
        local left = parse_unary()
        
        while true do
            if match(TokenType.OPERATOR, "*") then
                local op = "*"
                local right = parse_unary()
                left = {
                    type = "BinOp",
                    op = op,
                    left = left,
                    right = right
                }
            elseif match(TokenType.OPERATOR, "/") then
                local op = "/"
                local right = parse_unary()
                left = {
                    type = "BinOp",
                    op = op,
                    left = left,
                    right = right
                }
            elseif match(TokenType.OPERATOR, "%") then
                local op = "%"
                local right = parse_unary()
                left = {
                    type = "BinOp",
                    op = op,
                    left = left,
                    right = right
                }
            else
                break
            end
        end
        
        return left
    end
    
    parse_addition = function()
        local left = parse_multiplication()
        
        while true do
            if match(TokenType.OPERATOR, "+") then
                local op = "+"
                local right = parse_multiplication()
                left = {
                    type = "BinOp",
                    op = op,
                    left = left,
                    right = right
                }
            elseif match(TokenType.OPERATOR, "-") then
                local op = "-"
                local right = parse_multiplication()
                left = {
                    type = "BinOp",
                    op = op,
                    left = left,
                    right = right
                }
            else
                break
            end
        end
        
        return left
    end
    
    parse_comparison = function()
        local left = parse_addition()
        
        local ops = {"==", "!=", "<", "<=", ">", ">="}
        for _, op in ipairs(ops) do
            if match(TokenType.OPERATOR, op) then
                local right = parse_addition()
                return {
                    type = "Compare",
                    left = left,
                    ops = {op},
                    comparators = {right}
                }
            end
        end
        
        return left
    end
    
    parse_logical_and = function()
        local left = parse_comparison()
        
        while match(TokenType.KEYWORD, "and") do
            local op = "and"
            local right = parse_comparison()
            left = {
                type = "BoolOp",
                op = op,
                values = {left, right}
            }
        end
        
        return left
    end
    
    parse_logical_or = function()
        local left = parse_logical_and()
        
        while match(TokenType.KEYWORD, "or") do
            local op = "or"
            local right = parse_logical_and()
            left = {
                type = "BoolOp",
                op = op,
                values = {left, right}
            }
        end
        
        return left
    end
    
    parse_expression = function()
        return parse_logical_or()
    end
    
    -- Statement parsing
    function parse_statement()
        local token = current()
        
        if token.type == TokenType.KEYWORD then
            if token.value == "var" then
                return parse_var_decl()
            elseif token.value == "if" then
                return parse_if_statement()
            elseif token.value == "while" then
                return parse_while_statement()
            elseif token.value == "def" then
                return parse_function_def()
            elseif token.value == "return" then
                return parse_return_statement()
            elseif token.value == "pass" then
                consume(TokenType.KEYWORD)
                return {type = "Pass"}
            elseif token.value == "break" then
                consume(TokenType.KEYWORD)
                return {type = "Break"}
            elseif token.value == "continue" then
                consume(TokenType.KEYWORD)
                return {type = "Continue"}
            end
        end
        
        -- Assignment or expression
        if token.type == TokenType.IDENTIFIER then
            local lookahead = peek()
            if lookahead and lookahead.type == TokenType.OPERATOR and 
               (lookahead.value == "=" or lookahead.value:find("=$")) then
                return parse_assignment()
            end
        end
        
        -- Expression statement
        local expr = parse_expression()
        if match(TokenType.PUNCTUATION, ";") then
            -- Optional semicolon
        end
        return {type = "Expr", expr = expr}
    end
    
    function parse_var_decl()
        consume(TokenType.KEYWORD, "var")
        local name = consume(TokenType.IDENTIFIER).value
        
        local type_hint = nil
        if match(TokenType.PUNCTUATION, ":") then
            type_hint = consume(TokenType.IDENTIFIER).value
        end
        
        consume(TokenType.OPERATOR, "=")
        local value = parse_expression()
        
        return {
            type = "VarDecl",
            name = name,
            type_hint = type_hint,
            value = value
        }
    end
    
    function parse_assignment()
        local target = consume(TokenType.IDENTIFIER).value
        local op_token = consume(TokenType.OPERATOR)
        local op = op_token.value
        local value = parse_expression()
        
        if op == "=" then
            return {
                type = "Assign",
                target = target,
                value = value
            }
        else
            -- Augmented assignment
            local base_op = op:sub(1, -2)
            return {
                type = "AugAssign",
                target = target,
                op = base_op,
                value = value
            }
        end
    end
    
    function parse_if_statement()
        consume(TokenType.KEYWORD, "if")
        local condition = parse_expression()
        consume(TokenType.PUNCTUATION, ":")
        
        local then_block = {}
        if match(TokenType.PUNCTUATION, "{") then
            while not match(TokenType.PUNCTUATION, "}") do
                table.insert(then_block, parse_statement())
            end
        else
            table.insert(then_block, parse_statement())
        end
        
        local elif_blocks = {}
        while match(TokenType.KEYWORD, "elif") do
            local elif_cond = parse_expression()
            consume(TokenType.PUNCTUATION, ":")
            local elif_body = {}
            
            if match(TokenType.PUNCTUATION, "{") then
                while not match(TokenType.PUNCTUATION, "}") do
                    table.insert(elif_body, parse_statement())
                end
            else
                table.insert(elif_body, parse_statement())
            end
            
            table.insert(elif_blocks, {condition = elif_cond, body = elif_body})
        end
        
        local else_block = nil
        if match(TokenType.KEYWORD, "else") then
            consume(TokenType.PUNCTUATION, ":")
            else_block = {}
            
            if match(TokenType.PUNCTUATION, "{") then
                while not match(TokenType.PUNCTUATION, "}") do
                    table.insert(else_block, parse_statement())
                end
            else
                table.insert(else_block, parse_statement())
            end
        end
        
        return {
            type = "If",
            condition = condition,
            then_block = then_block,
            elif_blocks = elif_blocks,
            else_block = else_block
        }
    end
    
    function parse_while_statement()
        consume(TokenType.KEYWORD, "while")
        local condition = parse_expression()
        consume(TokenType.PUNCTUATION, ":")
        
        local body = {}
        if match(TokenType.PUNCTUATION, "{") then
            while not match(TokenType.PUNCTUATION, "}") do
                table.insert(body, parse_statement())
            end
        else
            table.insert(body, parse_statement())
        end
        
        return {
            type = "While",
            condition = condition,
            body = body
        }
    end
    
    function parse_function_def()
        consume(TokenType.KEYWORD, "def")
        local name = consume(TokenType.IDENTIFIER).value
        consume(TokenType.PUNCTUATION, "(")
        
        local args = {}
        if not match(TokenType.PUNCTUATION, ")") then
            repeat
                table.insert(args, consume(TokenType.IDENTIFIER).value)
            until not match(TokenType.PUNCTUATION, ",")
            consume(TokenType.PUNCTUATION, ")")
        end
        
        consume(TokenType.PUNCTUATION, ":")
        
        local body = {}
        if match(TokenType.PUNCTUATION, "{") then
            while not match(TokenType.PUNCTUATION, "}") do
                table.insert(body, parse_statement())
            end
        else
            table.insert(body, parse_statement())
        end
        
        return {
            type = "FunctionDef",
            name = name,
            args = args,
            body = body
        }
    end
    
    function parse_return_statement()
        consume(TokenType.KEYWORD, "return")
        local expr = nil
        if current().type ~= TokenType.EOF and 
           current().type ~= TokenType.PUNCTUATION and 
           current().type ~= TokenType.KEYWORD then
            expr = parse_expression()
        end
        return {
            type = "Return",
            expr = expr
        }
    end
    
    -- Parse program
    local statements = {}
    while current().type ~= TokenType.EOF do
        table.insert(statements, parse_statement())
    end
    
    return {
        type = "Program",
        body = statements
    }
end

return parser
"#;
        
        // Load the parser script
        let parser_module: Table = lua.load(parser_script).eval()?;
        
        // Parse the source code
        let ast: Table = lua.scope(|_scope| {
            let parse_func: mlua::Function = parser_module.get("parse")?;
            let lex_func: mlua::Function = parser_module.get("lex")?;
            
            let tokens: Table = lex_func.call(lua.create_string(source)?)?;
            let ast: Table = parse_func.call(tokens)?;
            Ok(ast)
        })?;
        
        // Convert Lua AST to Rust AST
        let program = self.convert_lua_ast_to_rust(&lua, &ast)?;
        
        // Return Lua instance to pool
        self.lua_pool.return_instance(lua);
        
        Ok(program)
    }
    
    fn convert_lua_ast_to_rust(&self, lua: &Lua, lua_ast: &Table) -> Result<Program, ParseError> {
        // Create a dummy span for now
        let dummy_span = Span::single(Position::start());
        
        // Helper function to convert Lua value to Rust Expression
        fn convert_expr(lua: &Lua, expr_table: &Table, span: Span) -> Result<Expr, ParseError> {
            let expr_type: String = expr_table.get("type").map_err(|e| ParseError::lua_error(e.to_string()))?;
            
            match expr_type.as_str() {
                "Number" => {
                    let value: i64 = expr_table.get("value").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    Ok(Expr::Number(value, span))
                }
                "String" => {
                    let value: String = expr_table.get("value").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    Ok(Expr::String(value, span))
                }
                "Boolean" => {
                    let value: bool = expr_table.get("value").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    Ok(Expr::Boolean(value, span))
                }
                "None" => Ok(Expr::None(span)),
                "Var" => {
                    let name: String = expr_table.get("name").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    Ok(Expr::Var(name, span))
                }
                "BinOp" => {
                    let op_str: String = expr_table.get("op").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let op = match op_str.as_str() {
                        "+" => Op::Add,
                        "-" => Op::Sub,
                        "*" => Op::Mul,
                        "/" => Op::Div,
                        "%" => Op::Mod,
                        "**" => Op::Pow,
                        _ => return Err(ParseError::syntax_error(format!("Unknown operator: {}", op_str), span)),
                    };
                    
                    let left_table: Table = expr_table.get("left").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let right_table: Table = expr_table.get("right").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    
                    let left = convert_expr(lua, &left_table, span)?;
                    let right = convert_expr(lua, &right_table, span)?;
                    
                    Ok(Expr::BinOp {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                        span,
                    })
                }
                "UnaryOp" => {
                    let op_str: String = expr_table.get("op").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let op = match op_str.as_str() {
                        "-" => UnaryOp::Minus,
                        "+" => UnaryOp::Plus,
                        "not" => UnaryOp::Not,
                        _ => return Err(ParseError::syntax_error(format!("Unknown unary operator: {}", op_str), span)),
                    };
                    
                    let operand_table: Table = expr_table.get("operand").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let operand = convert_expr(lua, &operand_table, span)?;
                    
                    Ok(Expr::UnaryOp {
                        op,
                        operand: Box::new(operand),
                        span,
                    })
                }
                "BoolOp" => {
                    let op_str: String = expr_table.get("op").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let op = match op_str.as_str() {
                        "and" => BoolOp::And,
                        "or" => BoolOp::Or,
                        _ => return Err(ParseError::syntax_error(format!("Unknown boolean operator: {}", op_str), span)),
                    };
                    
                    let values_table: Table = expr_table.get("values").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let values_len: i64 = values_table.len().map_err(|e: LuaError| ParseError::lua_error(e.to_string()))?;
                    
                    let mut values = Vec::new();
                    for i in 1..=values_len {
                        let value_table: Table = values_table.get(i).map_err(|e| ParseError::lua_error(e.to_string()))?;
                        values.push(convert_expr(lua, &value_table, span)?);
                    }
                    
                    Ok(Expr::BoolOp { op, values, span })
                }
                "Compare" => {
                    let left_table: Table = expr_table.get("left").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let left = convert_expr(lua, &left_table, span)?;
                    
                    let ops_table: Table = expr_table.get("ops").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let ops_len: i64 = ops_table.len().map_err(|e: LuaError| ParseError::lua_error(e.to_string()))?;
                    
                    let mut ops = Vec::new();
                    for i in 1..=ops_len {
                        let op_str: String = ops_table.get(i).map_err(|e| ParseError::lua_error(e.to_string()))?;
                        let op = match op_str.as_str() {
                            "==" => CompareOp::Eq,
                            "!=" => CompareOp::Ne,
                            "<" => CompareOp::Lt,
                            "<=" => CompareOp::Le,
                            ">" => CompareOp::Gt,
                            ">=" => CompareOp::Ge,
                            _ => return Err(ParseError::syntax_error(format!("Unknown comparison operator: {}", op_str), span)),
                        };
                        ops.push(op);
                    }
                    
                    let comparators_table: Table = expr_table.get("comparators").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let comparators_len: i64 = comparators_table.len().map_err(|e: LuaError| ParseError::lua_error(e.to_string()))?;
                    
                    let mut comparators = Vec::new();
                    for i in 1..=comparators_len {
                        let comp_table: Table = comparators_table.get(i).map_err(|e| ParseError::lua_error(e.to_string()))?;
                        comparators.push(convert_expr(lua, &comp_table, span)?);
                    }
                    
                    Ok(Expr::Compare {
                        left: Box::new(left),
                        ops,
                        comparators,
                        span,
                    })
                }
                "Call" => {
                    let func: String = expr_table.get("func").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    
                    let args_table: Table = expr_table.get("args").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let args_len: i64 = args_table.len().map_err(|e: LuaError| ParseError::lua_error(e.to_string()))?;
                    
                    let mut args = Vec::new();
                    for i in 1..=args_len {
                        let arg_table: Table = args_table.get(i).map_err(|e| ParseError::lua_error(e.to_string()))?;
                        args.push(convert_expr(lua, &arg_table, span)?);
                    }
                    
                    Ok(Expr::Call {
                        func,
                        args,
                        kwargs: HashMap::new(),
                        span,
                    })
                }
                _ => Err(ParseError::syntax_error(format!("Unknown expression type: {}", expr_type), span)),
            }
        }
        
        // Helper function to convert Lua value to Rust Statement
        fn convert_stmt(lua: &Lua, stmt_table: &Table, span: Span) -> Result<Statement, ParseError> {
            let stmt_type: String = stmt_table.get("type").map_err(|e| ParseError::lua_error(e.to_string()))?;
            
            match stmt_type.as_str() {
                "VarDecl" => {
                    let name: String = stmt_table.get("name").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let type_hint: Option<String> = stmt_table.get("type_hint").ok();
                    
                    let value_table: Table = stmt_table.get("value").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let value = convert_expr(lua, &value_table, span)?;
                    
                    Ok(Statement::VarDecl {
                        name,
                        value,
                        type_hint,
                        span,
                    })
                }
                "Assign" => {
                    let target: String = stmt_table.get("target").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    
                    let value_table: Table = stmt_table.get("value").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let value = convert_expr(lua, &value_table, span)?;
                    
                    Ok(Statement::Assign {
                        target,
                        value,
                        span,
                    })
                }
                "AugAssign" => {
                    let target: String = stmt_table.get("target").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    
                    let op_str: String = stmt_table.get("op").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let op = match op_str.as_str() {
                        "+" => Op::Add,
                        "-" => Op::Sub,
                        "*" => Op::Mul,
                        "/" => Op::Div,
                        _ => return Err(ParseError::syntax_error(format!("Unknown augmented assignment operator: {}", op_str), span)),
                    };
                    
                    let value_table: Table = stmt_table.get("value").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let value = convert_expr(lua, &value_table, span)?;
                    
                    Ok(Statement::AugAssign {
                        target,
                        op,
                        value,
                        span,
                    })
                }
                "Expr" => {
                    let expr_table: Table = stmt_table.get("expr").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let expr = convert_expr(lua, &expr_table, span)?;
                    Ok(Statement::Expr(expr))
                }
                "Return" => {
                    let expr = if let Ok(expr_table) = stmt_table.get("expr") {
                        Some(convert_expr(lua, &expr_table, span)?)
                    } else {
                        None
                    };
                    Ok(Statement::Return(expr))
                }
                "If" => {
                    let condition_table: Table = stmt_table.get("condition").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let condition = convert_expr(lua, &condition_table, span)?;
                    
                    let then_table: Table = stmt_table.get("then_block").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let then_len: i64 = then_table.len().map_err(|e: LuaError| ParseError::lua_error(e.to_string()))?;
                    
                    let mut then_block = Vec::new();
                    for i in 1..=then_len {
                        let stmt_table: Table = then_table.get(i).map_err(|e| ParseError::lua_error(e.to_string()))?;
                        then_block.push(convert_stmt(lua, &stmt_table, span)?);
                    }
                    
                    // FIXED: No generic type annotations
                    let elif_blocks_table: Table = stmt_table.get("elif_blocks")
                        .unwrap_or_else(|_| lua.create_table().unwrap());
                    
                    let elif_len: i64 = elif_blocks_table.len().map_err(|e: LuaError| ParseError::lua_error(e.to_string()))?;
                    
                    let mut elif_blocks = Vec::new();
                    for i in 1..=elif_len {
                        let elif_table: Table = elif_blocks_table.get(i).map_err(|e| ParseError::lua_error(e.to_string()))?;
                        
                        let elif_cond_table: Table = elif_table.get("condition").map_err(|e| ParseError::lua_error(e.to_string()))?;
                        let elif_cond = convert_expr(lua, &elif_cond_table, span)?;
                        
                        let elif_body_table: Table = elif_table.get("body").map_err(|e| ParseError::lua_error(e.to_string()))?;
                        let elif_body_len: i64 = elif_body_table.len().map_err(|e: LuaError| ParseError::lua_error(e.to_string()))?;
                        
                        let mut elif_body = Vec::new();
                        for j in 1..=elif_body_len {
                            let stmt_table: Table = elif_body_table.get(j).map_err(|e| ParseError::lua_error(e.to_string()))?;
                            elif_body.push(convert_stmt(lua, &stmt_table, span)?);
                        }
                        
                        elif_blocks.push((elif_cond, elif_body));
                    }
                    
                    // FIXED: No generic type annotations
                    let else_block = if let Ok(else_table) = stmt_table.get("else_block") {
                        let else_table: Table = else_table;
                        let else_len: i64 = else_table.len().map_err(|e: LuaError| ParseError::lua_error(e.to_string()))?;
                        
                        let mut else_body = Vec::new();
                        for i in 1..=else_len {
                            let stmt_table: Table = else_table.get(i).map_err(|e| ParseError::lua_error(e.to_string()))?;
                            else_body.push(convert_stmt(lua, &stmt_table, span)?);
                        }
                        Some(else_body)
                    } else {
                        None
                    };
                    
                    Ok(Statement::If {
                        condition,
                        then_block,
                        elif_blocks,
                        else_block,
                        span,
                    })
                }
                "While" => {
                    let condition_table: Table = stmt_table.get("condition").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let condition = convert_expr(lua, &condition_table, span)?;
                    
                    let body_table: Table = stmt_table.get("body").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let body_len: i64 = body_table.len().map_err(|e: LuaError| ParseError::lua_error(e.to_string()))?;
                    
                    let mut body = Vec::new();
                    for i in 1..=body_len {
                        let stmt_table: Table = body_table.get(i).map_err(|e| ParseError::lua_error(e.to_string()))?;
                        body.push(convert_stmt(lua, &stmt_table, span)?);
                    }
                    
                    Ok(Statement::While {
                        condition,
                        body,
                        orelse: None,
                        span,
                    })
                }
                "FunctionDef" => {
                    let name: String = stmt_table.get("name").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    
                    let args_table: Table = stmt_table.get("args").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let args_len: i64 = args_table.len().map_err(|e: LuaError| ParseError::lua_error(e.to_string()))?;
                    
                    let mut args = Vec::new();
                    for i in 1..=args_len {
                        let arg: String = args_table.get(i).map_err(|e| ParseError::lua_error(e.to_string()))?;
                        args.push(arg);
                    }
                    
                    let body_table: Table = stmt_table.get("body").map_err(|e| ParseError::lua_error(e.to_string()))?;
                    let body_len: i64 = body_table.len().map_err(|e: LuaError| ParseError::lua_error(e.to_string()))?;
                    
                    let mut body = Vec::new();
                    for i in 1..=body_len {
                        let stmt_table: Table = body_table.get(i).map_err(|e| ParseError::lua_error(e.to_string()))?;
                        body.push(convert_stmt(lua, &stmt_table, span)?);
                    }
                    
                    Ok(Statement::FunctionDef {
                        name,
                        args,
                        body,
                        span,
                    })
                }
                "Pass" => Ok(Statement::Pass),
                "Break" => Ok(Statement::Break),
                "Continue" => Ok(Statement::Continue),
                _ => Err(ParseError::syntax_error(format!("Unknown statement type: {}", stmt_type), span)),
            }
        }
        
        // Get the program body
        let body_table: Table = lua_ast.get("body").map_err(|e| ParseError::lua_error(e.to_string()))?;
        let body_len: i64 = body_table.len().map_err(|e: LuaError| ParseError::lua_error(e.to_string()))?;
        
        // Convert all statements
        let mut statements = Vec::new();
        for i in 1..=body_len {
            let stmt_table: Table = body_table.get(i).map_err(|e| ParseError::lua_error(e.to_string()))?;
            statements.push(convert_stmt(lua, &stmt_table, dummy_span)?);
        }
        
        Ok(Program {
            body: statements,
            span: dummy_span,
        })
    }
    
    // Added helper function for Lua integration
    pub fn create_lua_system_functions(lua: &Lua) -> Result<(), ParseError> {
        // Create a log function for debugging - FIXED: Added type annotation
        let log_func = lua.create_function(|_, (msg, level): (String, i32)| {
            println!("[LUA LOG level={}] {}", level, msg);
            Ok(())
        })?;
        
        // Create a file open function - FIXED: Added type annotation
        let open_func = lua.create_function(|_, path: String| {
            println!("[LUA] Opening file: {}", path);
            // In a real implementation, this would return a file handle
            Ok(0) // Return a dummy file handle
        })?;
        
        // Create a file read function - FIXED: Added type annotation
        let read_func = lua.create_function(|_, handle: i32| {
            println!("[LUA] Reading from file handle: {}", handle);
            Ok("dummy file content".to_string())
        })?;
        
        // Create a function with no parameters - FIXED: Added type annotation
        let version_func = lua.create_function(|_, (): ()| {
            println!("[LUA] Getting version");
            Ok("1.0.0".to_string())
        })?;
        
        // Create a function with multiple parameters of different types
        let format_func = lua.create_function(|_, (template, _values): (String, Table)| {
            println!("[LUA] Formatting template: {}", template);
            // In a real implementation, would format the string with values
            Ok(format!("Formatted: {}", template))
        })?;
        
        // Register the functions in Lua global namespace - FIXED: No generic annotations
        let globals = lua.globals();
        globals.set("log", log_func)?;
        globals.set("open_file", open_func)?;
        globals.set("read_file", read_func)?;
        globals.set("get_version", version_func)?;
        globals.set("format_string", format_func)?;
        
        Ok(())
    }
}

impl Default for LuaFrontend {
    fn default() -> Self {
        Self::new()
    }
}

// Public API function
pub fn parse_program(source: &str) -> Result<Program, Vec<ParseError>> {
    let frontend = LuaFrontend::new();
    match frontend.parse_program(source) {
        Ok(program) => Ok(program),
        Err(err) => Err(vec![err]),
    }
}

// Helper functions for backward compatibility
pub fn create_semicolon_error(span: Span) -> ParseError {
    ParseError::syntax_error(
        "You forgot semicolon here",
        span
    ).with_help("Try adding semicolon here")
}

pub fn create_undefined_variable_error(name: &str, span: Span) -> ParseError {
    ParseError::syntax_error(
        format!("Undefined variable '{}'", name),
        span
    ).with_help("Declare the variable with 'var' or check spelling")
}

pub fn create_type_mismatch_error(expected: &str, found: &str, span: Span) -> ParseError {
    ParseError::syntax_error(
        format!("Type mismatch: expected {}, found {}", expected, found),
        span
    ).with_help("Check the types of your expression")
}

pub fn format_parse_errors(errors: &[ParseError], source: &str) -> String {
    use colored::*;
    
    if errors.is_empty() {
        return String::new();
    }
    
    let mut output = String::new();
    output.push_str(&format!("{} Found {} error{}\n", 
        "✗".red().bold(),
        errors.len(),
        if errors.len() == 1 { "" } else { "s" }
    ));
    
    for (i, error) in errors.iter().enumerate() {
        output.push_str(&format!("\n{} ", format!("{}.", i + 1).bold()));
        output.push_str(&error.format_error(source));
    }
    
    output
}