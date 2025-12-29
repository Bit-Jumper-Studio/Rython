use std::collections::HashMap;
use serde::{Serialize, Deserialize};

// ==================== POSITION TRACKING ====================

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

// ==================== ERROR HANDLING ====================

#[derive(Debug, Clone)]
pub enum ParseError {
    SyntaxError {
        message: String,
        span: Span,
        help: Option<String>,
        context: Option<String>,
    },
    LexError {
        message: String,
        span: Span,
        help: Option<String>,
    },
}

impl ParseError {
    pub fn syntax_error(message: impl Into<String>, span: Span) -> Self {
        ParseError::SyntaxError {
            message: message.into(),
            span,
            help: None,
            context: None,
        }
    }
    
    pub fn lex_error(message: impl Into<String>, span: Span) -> Self {
        ParseError::LexError {
            message: message.into(),
            span,
            help: None,
        }
    }
    
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        match &mut self {
            ParseError::SyntaxError { help: h, .. } => *h = Some(help.into()),
            ParseError::LexError { help: h, .. } => *h = Some(help.into()),
        }
        self
    }
    
    pub fn with_context(mut self, context: impl Into<String>) -> Self {
        match &mut self {
            ParseError::SyntaxError { context: c, .. } => *c = Some(context.into()),
            ParseError::LexError { .. } => {} // Lex errors don't have context
        }
        self
    }
    
    pub fn span(&self) -> Span {
        match self {
            ParseError::SyntaxError { span, .. } => *span,
            ParseError::LexError { span, .. } => *span,
        }
    }
    
    pub fn format_error(&self, source: &str) -> String {
        match self {
            ParseError::SyntaxError { message, span, help, context } => {
                self.format_detailed("syntax error", message, span, help.as_deref(), context.as_deref(), source)
            }
            ParseError::LexError { message, span, help } => {
                self.format_detailed("lexical error", message, span, help.as_deref(), None, source)
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
            ParseError::SyntaxError { message, span, .. } => {
                write!(f, "syntax error at {}: {}", span, message)
            }
            ParseError::LexError { message, span, .. } => {
                write!(f, "lexical error at {}: {}", span, message)
            }
        }
    }
}

impl std::error::Error for ParseError {}

// ==================== AST DEFINITIONS ====================

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

// ==================== TOKENS WITH POSITION ====================

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub lexeme: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Var, If, Elif, Else, While, For, In, Return, Def,
    And, Or, Not, Pass, Break, Continue,
    True, False, None,
    
    // Literals
    Number(i64),
    Float(f64),
    String(String),
    Identifier(String),
    
    // Operators
    Plus, Minus, Star, Slash, Percent, Power,
    Eq, Ne, Lt, Le, Gt, Ge, Assign,
    PlusAssign, MinusAssign, StarAssign, SlashAssign,
    
    // Punctuation
    LParen, RParen, LBrace, RBrace, Comma, Colon, Semicolon,
    
    // Special
    Newline, EOF,
}

impl TokenKind {
    pub fn to_string(&self) -> String {
        match self {
            TokenKind::Var => "var".to_string(),
            TokenKind::If => "if".to_string(),
            TokenKind::Elif => "elif".to_string(),
            TokenKind::Else => "else".to_string(),
            TokenKind::While => "while".to_string(),
            TokenKind::For => "for".to_string(),
            TokenKind::In => "in".to_string(),
            TokenKind::Return => "return".to_string(),
            TokenKind::Def => "def".to_string(),
            TokenKind::And => "and".to_string(),
            TokenKind::Or => "or".to_string(),
            TokenKind::Not => "not".to_string(),
            TokenKind::Pass => "pass".to_string(),
            TokenKind::Break => "break".to_string(),
            TokenKind::Continue => "continue".to_string(),
            TokenKind::True => "True".to_string(),
            TokenKind::False => "False".to_string(),
            TokenKind::None => "None".to_string(),
            TokenKind::Number(n) => n.to_string(),
            TokenKind::Float(f) => f.to_string(),
            TokenKind::String(s) => format!("\"{}\"", s),
            TokenKind::Identifier(s) => s.clone(),
            TokenKind::Plus => "+".to_string(),
            TokenKind::Minus => "-".to_string(),
            TokenKind::Star => "*".to_string(),
            TokenKind::Slash => "/".to_string(),
            TokenKind::Percent => "%".to_string(),
            TokenKind::Power => "**".to_string(),
            TokenKind::Eq => "==".to_string(),
            TokenKind::Ne => "!=".to_string(),
            TokenKind::Lt => "<".to_string(),
            TokenKind::Le => "<=".to_string(),
            TokenKind::Gt => ">".to_string(),
            TokenKind::Ge => ">=".to_string(),
            TokenKind::Assign => "=".to_string(),
            TokenKind::PlusAssign => "+=".to_string(),
            TokenKind::MinusAssign => "-=".to_string(),
            TokenKind::StarAssign => "*=".to_string(),
            TokenKind::SlashAssign => "/=".to_string(),
            TokenKind::LParen => "(".to_string(),
            TokenKind::RParen => ")".to_string(),
            TokenKind::LBrace => "{".to_string(),
            TokenKind::RBrace => "}".to_string(),
            TokenKind::Comma => ",".to_string(),
            TokenKind::Colon => ":".to_string(),
            TokenKind::Semicolon => ";".to_string(),
            TokenKind::Newline => "newline".to_string(),
            TokenKind::EOF => "end of file".to_string(),
        }
    }
}

pub fn lex(input: &str) -> Result<Vec<Token>, ParseError> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    let mut pos = Position::start();
    
    while let Some(&c) = chars.peek() {
        let start_pos = pos;
        
        match c {
            ' ' | '\t' => {
                pos.advance(c);
                chars.next();
            }
            '\n' => {
                let span = Span::single(pos);
                tokens.push(Token {
                    kind: TokenKind::Newline,
                    span,
                    lexeme: c.to_string(),
                });
                pos.advance(c);
                chars.next();
            }
            '#' => {
                while chars.next_if(|&c| c != '\n').is_some() {
                    pos.advance(c);
                }
            }
            '"' | '\'' => {
    let quote = chars.next().unwrap();
    pos.advance(quote);
    let mut s = String::new();
    let mut escape = false;
    let mut closed = false;
    
    while let Some(ch) = chars.next() {
        if escape {
            s.push(match ch {
                'n' => '\n', 't' => '\t', 'r' => '\r',
                '\\' => '\\', '\'' => '\'', '"' => '"',
                _ => {
                    return Err(ParseError::lex_error(
                        format!("Invalid escape sequence '\\{}'", ch),
                        Span::single(pos)
                    ).with_help("Valid escape sequences: \\n, \\t, \\r, \\\\, \\', \\\""));
                }
            });
            escape = false;
        } else if ch == '\\' {
            escape = true;
        } else if ch == quote {
            closed = true;
            break;
        } else {
            s.push(ch);
        }
        pos.advance(ch);
    }
    
    if !closed {
        return Err(ParseError::lex_error(
            format!("Unclosed string literal starting with '{}'", quote),
            Span::new(start_pos, pos)
        ).with_help("Close the string with matching quote"));
    }
    
    let span = Span::new(start_pos, pos);
    let lexeme = input[start_pos.offset..pos.offset].to_string();
    tokens.push(Token {
        kind: TokenKind::String(s),
        span,
        lexeme,
    });
}
            '0'..='9' => {
                let mut num = String::new();
                let mut is_float = false;
                
                while let Some(&d) = chars.peek() {
                    if d.is_numeric() {
                        num.push(chars.next().unwrap());
                        pos.advance(d);
                    } else if d == '.' && chars.clone().skip(1).next().map_or(false, |c| c.is_numeric()) {
                        is_float = true;
                        num.push(chars.next().unwrap());
                        pos.advance(d);
                    } else {
                        break;
                    }
                }
                
                let span = Span::new(start_pos, pos);
                let lexeme = input[start_pos.offset..pos.offset].to_string();
                
                if is_float {
                    match num.parse::<f64>() {
                        Ok(f) => tokens.push(Token {
                            kind: TokenKind::Float(f),
                            span,
                            lexeme,
                        }),
                        Err(_) => {
                            return Err(ParseError::lex_error(
                                format!("Invalid floating point number '{}'", num),
                                span
                            ).with_help("Ensure the number is in valid format (e.g., 3.14, 0.5)"));
                        }
                    }
                } else {
                    match num.parse::<i64>() {
                        Ok(n) => tokens.push(Token {
                            kind: TokenKind::Number(n),
                            span,
                            lexeme,
                        }),
                        Err(_) => {
                            return Err(ParseError::lex_error(
                                format!("Number '{}' is too large", num),
                                span
                            ).with_help("Use a smaller integer value"));
                        }
                    }
                }
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut ident = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        ident.push(chars.next().unwrap());
                        pos.advance(ch);
                    } else {
                        break;
                    }
                }
                
                let span = Span::new(start_pos, pos);
                let lexeme = input[start_pos.offset..pos.offset].to_string();
                let kind = match ident.as_str() {
                    "var" => TokenKind::Var,
                    "if" => TokenKind::If,
                    "elif" => TokenKind::Elif,
                    "else" => TokenKind::Else,
                    "while" => TokenKind::While,
                    "for" => TokenKind::For,
                    "in" => TokenKind::In,
                    "return" => TokenKind::Return,
                    "def" => TokenKind::Def,
                    "and" => TokenKind::And,
                    "or" => TokenKind::Or,
                    "not" => TokenKind::Not,
                    "pass" => TokenKind::Pass,
                    "break" => TokenKind::Break,
                    "continue" => TokenKind::Continue,
                    "True" => TokenKind::True,
                    "False" => TokenKind::False,
                    "None" => TokenKind::None,
                    _ => TokenKind::Identifier(ident),
                };
                
                tokens.push(Token {
                    kind,
                    span,
                    lexeme,
                });
            }
            '+' => {
                chars.next();
                pos.advance('+');
                if chars.next_if_eq(&'=').is_some() {
                    pos.advance('=');
                    let span = Span::new(start_pos, pos);
                    let lexeme = input[start_pos.offset..pos.offset].to_string();
                    tokens.push(Token {
                        kind: TokenKind::PlusAssign,
                        span,
                        lexeme,
                    });
                } else {
                    let span = Span::single(start_pos);
                    tokens.push(Token {
                        kind: TokenKind::Plus,
                        span,
                        lexeme: "+".to_string(),
                    });
                }
            }
            '-' => {
                chars.next();
                pos.advance('-');
                if chars.next_if_eq(&'=').is_some() {
                    pos.advance('=');
                    let span = Span::new(start_pos, pos);
                    let lexeme = input[start_pos.offset..pos.offset].to_string();
                    tokens.push(Token {
                        kind: TokenKind::MinusAssign,
                        span,
                        lexeme,
                    });
                } else {
                    let span = Span::single(start_pos);
                    tokens.push(Token {
                        kind: TokenKind::Minus,
                        span,
                        lexeme: "-".to_string(),
                    });
                }
            }
            '*' => {
                chars.next();
                pos.advance('*');
                if chars.next_if_eq(&'*').is_some() {
                    pos.advance('*');
                    let span = Span::new(start_pos, pos);
                    let lexeme = input[start_pos.offset..pos.offset].to_string();
                    tokens.push(Token {
                        kind: TokenKind::Power,
                        span,
                        lexeme,
                    });
                } else if chars.next_if_eq(&'=').is_some() {
                    pos.advance('=');
                    let span = Span::new(start_pos, pos);
                    let lexeme = input[start_pos.offset..pos.offset].to_string();
                    tokens.push(Token {
                        kind: TokenKind::StarAssign,
                        span,
                        lexeme,
                    });
                } else {
                    let span = Span::single(start_pos);
                    tokens.push(Token {
                        kind: TokenKind::Star,
                        span,
                        lexeme: "*".to_string(),
                    });
                }
            }
            '/' => {
                chars.next();
                pos.advance('/');
                if chars.next_if_eq(&'=').is_some() {
                    pos.advance('=');
                    let span = Span::new(start_pos, pos);
                    let lexeme = input[start_pos.offset..pos.offset].to_string();
                    tokens.push(Token {
                        kind: TokenKind::SlashAssign,
                        span,
                        lexeme,
                    });
                } else {
                    let span = Span::single(start_pos);
                    tokens.push(Token {
                        kind: TokenKind::Slash,
                        span,
                        lexeme: "/".to_string(),
                    });
                }
            }
            '%' => {
                chars.next();
                pos.advance('%');
                let span = Span::single(start_pos);
                tokens.push(Token {
                    kind: TokenKind::Percent,
                    span,
                    lexeme: "%".to_string(),
                });
            }
            '=' => {
                chars.next();
                pos.advance('=');
                if chars.next_if_eq(&'=').is_some() {
                    pos.advance('=');
                    let span = Span::new(start_pos, pos);
                    let lexeme = input[start_pos.offset..pos.offset].to_string();
                    tokens.push(Token {
                        kind: TokenKind::Eq,
                        span,
                        lexeme,
                    });
                } else {
                    let span = Span::single(start_pos);
                    tokens.push(Token {
                        kind: TokenKind::Assign,
                        span,
                        lexeme: "=".to_string(),
                    });
                }
            }
            '!' => {
                chars.next();
                pos.advance('!');
                if chars.next_if_eq(&'=').is_some() {
                    pos.advance('=');
                    let span = Span::new(start_pos, pos);
                    let lexeme = input[start_pos.offset..pos.offset].to_string();
                    tokens.push(Token {
                        kind: TokenKind::Ne,
                        span,
                        lexeme,
                    });
                } else {
                    return Err(ParseError::lex_error(
                        "Unexpected '!'".to_string(),
                        Span::single(start_pos)
                    ).with_help("Did you mean '!=' for not equal?"));
                }
            }
            '<' => {
                chars.next();
                pos.advance('<');
                if chars.next_if_eq(&'=').is_some() {
                    pos.advance('=');
                    let span = Span::new(start_pos, pos);
                    let lexeme = input[start_pos.offset..pos.offset].to_string();
                    tokens.push(Token {
                        kind: TokenKind::Le,
                        span,
                        lexeme,
                    });
                } else {
                    let span = Span::single(start_pos);
                    tokens.push(Token {
                        kind: TokenKind::Lt,
                        span,
                        lexeme: "<".to_string(),
                    });
                }
            }
            '>' => {
                chars.next();
                pos.advance('>');
                if chars.next_if_eq(&'=').is_some() {
                    pos.advance('=');
                    let span = Span::new(start_pos, pos);
                    let lexeme = input[start_pos.offset..pos.offset].to_string();
                    tokens.push(Token {
                        kind: TokenKind::Ge,
                        span,
                        lexeme,
                    });
                } else {
                    let span = Span::single(start_pos);
                    tokens.push(Token {
                        kind: TokenKind::Gt,
                        span,
                        lexeme: ">".to_string(),
                    });
                }
            }
            '(' => {
                chars.next();
                pos.advance('(');
                let span = Span::single(start_pos);
                tokens.push(Token {
                    kind: TokenKind::LParen,
                    span,
                    lexeme: "(".to_string(),
                });
            }
            ')' => {
                chars.next();
                pos.advance(')');
                let span = Span::single(start_pos);
                tokens.push(Token {
                    kind: TokenKind::RParen,
                    span,
                    lexeme: ")".to_string(),
                });
            }
            '{' => {
                chars.next();
                pos.advance('{');
                let span = Span::single(start_pos);
                tokens.push(Token {
                    kind: TokenKind::LBrace,
                    span,
                    lexeme: "{".to_string(),
                });
            }
            '}' => {
                chars.next();
                pos.advance('}');
                let span = Span::single(start_pos);
                tokens.push(Token {
                    kind: TokenKind::RBrace,
                    span,
                    lexeme: "}".to_string(),
                });
            }
            ',' => {
                chars.next();
                pos.advance(',');
                let span = Span::single(start_pos);
                tokens.push(Token {
                    kind: TokenKind::Comma,
                    span,
                    lexeme: ",".to_string(),
                });
            }
            ':' => {
                chars.next();
                pos.advance(':');
                let span = Span::single(start_pos);
                tokens.push(Token {
                    kind: TokenKind::Colon,
                    span,
                    lexeme: ":".to_string(),
                });
            }
            ';' => {
                chars.next();
                pos.advance(';');
                let span = Span::single(start_pos);
                tokens.push(Token {
                    kind: TokenKind::Semicolon,
                    span,
                    lexeme: ";".to_string(),
                });
            }
            _ => {
                return Err(ParseError::lex_error(
                    format!("Unexpected character '{}'", c),
                    Span::single(pos)
                ).with_help("Remove or replace this character"));
            }
        }
    }
    
    // Add EOF token
    tokens.push(Token {
        kind: TokenKind::EOF,
        span: Span::single(pos),
        lexeme: "".to_string(),
    });
    
    Ok(tokens)
}

// ==================== PARSER WITH ERROR RECOVERY ====================

pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token], _source: &'a str) -> Self {
        Self {
            tokens,
            pos: 0,
            errors: Vec::new(),
        }
    }
    
    pub fn parse_program(&mut self) -> Result<Program, Vec<ParseError>> {
        let start_pos = self.current().span.start;
        let mut statements = Vec::new();
        self.skip_newlines();
        
        while !self.is_at_end() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    self.errors.push(err);
                    self.recover();
                }
            }
            self.skip_newlines();
        }
        
        let end_pos = self.current().span.end;
        
        if !self.errors.is_empty() {
            Err(self.errors.clone())
        } else {
            Ok(Program {
                body: statements,
                span: Span::new(start_pos, end_pos),
            })
        }
    }
    
    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.current().span;
        
        match &self.current().kind {
            TokenKind::Var => self.parse_var_decl(start),
            TokenKind::If => self.parse_if_statement(start),
            TokenKind::While => self.parse_while_statement(start),
            TokenKind::Return => self.parse_return_statement(start),
            TokenKind::Def => self.parse_function_def(start),
            TokenKind::Pass => {
                self.consume(TokenKind::Pass)?;
                self.skip_newlines();
                Ok(Statement::Pass)
            }
            TokenKind::Break => {
                self.consume(TokenKind::Break)?;
                self.skip_newlines();
                Ok(Statement::Break)
            }
            TokenKind::Continue => {
                self.consume(TokenKind::Continue)?;
                self.skip_newlines();
                Ok(Statement::Continue)
            }
            TokenKind::Identifier(name) => {
                let name = name.clone();
                let name_span = self.current().span;
                self.advance();
                
                if matches!(self.current().kind, TokenKind::Assign) {
                    self.consume(TokenKind::Assign)?;
                    let value = self.parse_expression()?;
                    let value_span = value.span(); // Get span before moving
                    self.skip_newlines();
                    Ok(Statement::Assign { 
                        target: name, 
                        value, 
                        span: Span::new(name_span.start, value_span.end) 
                    })
                } else if matches!(self.current().kind, TokenKind::PlusAssign | TokenKind::MinusAssign | 
                                 TokenKind::StarAssign | TokenKind::SlashAssign) {
                    let op = match self.current().kind {
                        TokenKind::PlusAssign => Op::Add,
                        TokenKind::MinusAssign => Op::Sub,
                        TokenKind::StarAssign => Op::Mul,
                        TokenKind::SlashAssign => Op::Div,
                        _ => unreachable!(),
                    };
                    self.advance();
                    let value = self.parse_expression()?;
                    let value_span = value.span(); // Get span before moving
                    self.skip_newlines();
                    Ok(Statement::AugAssign { 
                        target: name, 
                        op, 
                        value, 
                        span: Span::new(name_span.start, value_span.end) 
                    })
                } else {
                    self.pos -= 1; // Go back to identifier
                    let expr = self.parse_expression()?;
                    self.skip_newlines();
                    Ok(Statement::Expr(expr))
                }
            }
            _ => {
                let expr = self.parse_expression()?;
                self.skip_newlines();
                Ok(Statement::Expr(expr))
            }
        }
    }
    
    fn parse_var_decl(&mut self, start: Span) -> Result<Statement, ParseError> {
        self.consume(TokenKind::Var)?;
        
        let name = match &self.current().kind {
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.advance();
                name
            }
            _ => {
                return Err(ParseError::syntax_error(
                    "Expected identifier after 'var'",
                    self.current().span
                ).with_context("Variable declaration must have a name")
                 .with_help("Example: var x = 42"));
            }
        };
        
        let type_hint = if matches!(self.current().kind, TokenKind::Colon) {
            self.advance();
            match &self.current().kind {
                TokenKind::Identifier(t) => {
                    let t = t.clone();
                    self.advance();
                    Some(t)
                }
                _ => {
                    return Err(ParseError::syntax_error(
                        "Expected type name after ':'",
                        self.current().span
                    ).with_help("Example: var x: int = 42"));
                }
            }
        } else {
            None
        };
        
        self.consume(TokenKind::Assign)?;
        let value = self.parse_expression()?;
        let value_span = value.span(); // Get span before moving
        self.skip_newlines();
        
        Ok(Statement::VarDecl { 
            name, 
            value, 
            type_hint, 
            span: Span::new(start.start, value_span.end) 
        })
    }
    
    fn parse_if_statement(&mut self, start: Span) -> Result<Statement, ParseError> {
        self.consume(TokenKind::If)?;
        let condition = self.parse_expression()?;
        
        self.consume(TokenKind::Colon)?;
        self.skip_newlines();
        let then_block = self.parse_block()?;
        
        let mut elif_blocks = Vec::new();
        while matches!(self.current().kind, TokenKind::Elif) {
            self.advance();
            let elif_cond = self.parse_expression()?;
            self.consume(TokenKind::Colon)?;
            self.skip_newlines();
            let elif_body = self.parse_block()?;
            elif_blocks.push((elif_cond, elif_body));
        }
        
        let else_block = if matches!(self.current().kind, TokenKind::Else) {
            self.advance();
            self.consume(TokenKind::Colon)?;
            self.skip_newlines();
            Some(self.parse_block()?)
        } else {
            None
        };
        
        Ok(Statement::If { 
            condition, 
            then_block, 
            elif_blocks, 
            else_block,
            span: start 
        })
    }
    
    fn parse_while_statement(&mut self, start: Span) -> Result<Statement, ParseError> {
        self.consume(TokenKind::While)?;
        let condition = self.parse_expression()?;
        
        self.consume(TokenKind::Colon)?;
        self.skip_newlines();
        let body = self.parse_block()?;
        
        Ok(Statement::While { 
            condition, 
            body, 
            orelse: None,
            span: start 
        })
    }
    
    fn parse_return_statement(&mut self, _start: Span) -> Result<Statement, ParseError> {
        self.consume(TokenKind::Return)?;
        let expr = if matches!(self.current().kind, TokenKind::Newline | TokenKind::EOF) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.skip_newlines();
        Ok(Statement::Return(expr))
    }
    
    fn parse_function_def(&mut self, start: Span) -> Result<Statement, ParseError> {
        self.consume(TokenKind::Def)?;
        let name = match &self.current().kind {
            TokenKind::Identifier(n) => {
                let n = n.clone();
                self.advance();
                n
            }
            _ => {
                return Err(ParseError::syntax_error(
                    "Expected function name",
                    self.current().span
                ).with_help("Example: def my_function():"));
            }
        };
        
        self.consume(TokenKind::LParen)?;
        let mut args = Vec::new();
        
        while !matches!(self.current().kind, TokenKind::RParen) {
            match &self.current().kind {
                TokenKind::Identifier(arg) => {
                    args.push(arg.clone());
                    self.advance();
                }
                _ => {
                    return Err(ParseError::syntax_error(
                        "Expected argument name",
                        self.current().span
                    ).with_help("Example: def add(a, b):"));
                }
            }
            
            if matches!(self.current().kind, TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }
        
        self.consume(TokenKind::RParen)?;
        self.consume(TokenKind::Colon)?;
        self.skip_newlines();
        let body = self.parse_block()?;
        
        Ok(Statement::FunctionDef { 
            name, 
            args, 
            body,
            span: start 
        })
    }
    
    fn parse_block(&mut self) -> Result<Vec<Statement>, ParseError> {
        let _start = self.current().span;
        
        if matches!(self.current().kind, TokenKind::LBrace) {
            self.advance();
            let mut statements = Vec::new();
            
            while !matches!(self.current().kind, TokenKind::RBrace) && !self.is_at_end() {
                match self.parse_statement() {
                    Ok(stmt) => statements.push(stmt),
                    Err(err) => {
                        self.errors.push(err);
                        self.recover();
                    }
                }
                self.skip_newlines();
            }
            
            if self.is_at_end() {
                return Err(ParseError::syntax_error(
                    "Unclosed block starting here",
                    _start
                ).with_help("Add '}' to close the block"));
            }
            
            self.consume(TokenKind::RBrace)?;
            Ok(statements)
        } else {
            // Single statement block
            Ok(vec![self.parse_statement()?])
        }
    }
    
    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_or()
    }
    
    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_and()?;
        let start_span = left.span();
        
        while matches!(self.current().kind, TokenKind::Or) {
            self.advance();
            let right = self.parse_and()?;
            let right_span = right.span(); // Get span before moving
            left = Expr::BoolOp { 
                op: BoolOp::Or, 
                values: vec![left, right], 
                span: Span::new(start_span.start, right_span.end) 
            };
        }
        
        Ok(left)
    }
    
    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_not()?;
        let start_span = left.span();
        
        while matches!(self.current().kind, TokenKind::And) {
            self.advance();
            let right = self.parse_not()?;
            let right_span = right.span(); // Get span before moving
            left = Expr::BoolOp { 
                op: BoolOp::And, 
                values: vec![left, right], 
                span: Span::new(start_span.start, right_span.end) 
            };
        }
        
        Ok(left)
    }
    
    fn parse_not(&mut self) -> Result<Expr, ParseError> {
        if matches!(self.current().kind, TokenKind::Not) {
            let start = self.current().span;
            self.advance();
            let operand = self.parse_not()?;
            let operand_span = operand.span(); // Get span before moving
            Ok(Expr::UnaryOp { 
                op: UnaryOp::Not, 
                operand: Box::new(operand), 
                span: Span::new(start.start, operand_span.end) 
            })
        } else {
            self.parse_comparison()
        }
    }
    
    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let left = self.parse_addition()?;
        let mut ops = Vec::new();
        let mut comparators = Vec::new();
        let start_span = left.span();
        let mut _end_span = left.span();
        
        while matches!(self.current().kind, TokenKind::Eq | TokenKind::Ne | 
                     TokenKind::Lt | TokenKind::Le | TokenKind::Gt | TokenKind::Ge) {
            let op = match self.current().kind {
                TokenKind::Eq => CompareOp::Eq,
                TokenKind::Ne => CompareOp::Ne,
                TokenKind::Lt => CompareOp::Lt,
                TokenKind::Le => CompareOp::Le,
                TokenKind::Gt => CompareOp::Gt,
                TokenKind::Ge => CompareOp::Ge,
                _ => unreachable!(),
            };
            self.advance();
            let comparator = self.parse_addition()?;
            _end_span = comparator.span();
            ops.push(op);
            comparators.push(comparator);
        }
        
        if ops.is_empty() {
            Ok(left)
        } else {
            Ok(Expr::Compare { 
                left: Box::new(left), 
                ops, 
                comparators, 
                span: Span::new(start_span.start, _end_span.end) 
            })
        }
    }
    
    fn parse_addition(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_multiplication()?;
        let start_span = left.span();
        
        while matches!(self.current().kind, TokenKind::Plus | TokenKind::Minus) {
            let op = match self.current().kind {
                TokenKind::Plus => Op::Add,
                TokenKind::Minus => Op::Sub,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_multiplication()?;
            let right_span = right.span(); // Get span before moving
            left = Expr::BinOp { 
                left: Box::new(left), 
                op, 
                right: Box::new(right), 
                span: Span::new(start_span.start, right_span.end) 
            };
        }
        
        Ok(left)
    }
    
    fn parse_multiplication(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary()?;
        let start_span = left.span();
        
        while matches!(self.current().kind, TokenKind::Star | TokenKind::Slash | TokenKind::Percent) {
            let op = match self.current().kind {
                TokenKind::Star => Op::Mul,
                TokenKind::Slash => Op::Div,
                TokenKind::Percent => Op::Mod,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_unary()?;
            let right_span = right.span(); // Get span before moving
            left = Expr::BinOp { 
                left: Box::new(left), 
                op, 
                right: Box::new(right), 
                span: Span::new(start_span.start, right_span.end) 
            };
        }
        
        Ok(left)
    }
    
    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if matches!(self.current().kind, TokenKind::Minus | TokenKind::Plus) {
            let start = self.current().span;
            let op = match self.current().kind {
                TokenKind::Minus => UnaryOp::Minus,
                TokenKind::Plus => UnaryOp::Plus,
                _ => unreachable!(),
            };
            self.advance();
            let operand = self.parse_unary()?;
            let operand_span = operand.span(); // Get span before moving
            Ok(Expr::UnaryOp { 
                op, 
                operand: Box::new(operand), 
                span: Span::new(start.start, operand_span.end) 
            })
        } else {
            self.parse_power()
        }
    }
    
    fn parse_power(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_primary()?;
        let start_span = left.span();
        
        if matches!(self.current().kind, TokenKind::Power) {
            self.advance();
            let right = self.parse_unary()?;
            let right_span = right.span(); // Get span before moving
            left = Expr::BinOp { 
                left: Box::new(left), 
                op: Op::Pow, 
                right: Box::new(right), 
                span: Span::new(start_span.start, right_span.end) 
            };
        }
        
        Ok(left)
    }
    
    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.current().clone();
        
        match &token.kind {
            TokenKind::Number(n) => {
                self.advance();
                Ok(Expr::Number(*n, token.span))
            }
            TokenKind::Float(f) => {
                self.advance();
                Ok(Expr::Float(*f, token.span))
            }
            TokenKind::True => {
                self.advance();
                Ok(Expr::Boolean(true, token.span))
            }
            TokenKind::False => {
                self.advance();
                Ok(Expr::Boolean(false, token.span))
            }
            TokenKind::None => {
                self.advance();
                Ok(Expr::None(token.span))
            }
            TokenKind::String(s) => {
                self.advance();
                Ok(Expr::String(s.clone(), token.span))
            }
            TokenKind::Identifier(name) => {
                self.advance();
                
                if matches!(self.current().kind, TokenKind::LParen) {
                    let start_span = token.span;
                    self.advance();
                    let mut args = Vec::new();
                    
                    while !matches!(self.current().kind, TokenKind::RParen) && !self.is_at_end() {
                        args.push(self.parse_expression()?);
                        
                        if matches!(self.current().kind, TokenKind::Comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    
                    if self.is_at_end() {
                        return Err(ParseError::syntax_error(
                            "Unclosed function call",
                            start_span
                        ).with_help("Add ')' to close the function call"));
                    }
                    
                    self.consume(TokenKind::RParen)?;
                    let end_span = self.previous().span;
                    Ok(Expr::Call { 
                        func: name.clone(), 
                        args, 
                        kwargs: HashMap::new(), 
                        span: Span::new(start_span.start, end_span.end) 
                    })
                } else {
                    Ok(Expr::Var(name.clone(), token.span))
                }
            }
            TokenKind::LParen => {
                let start_span = token.span;
                self.advance();
                let expr = self.parse_expression()?;
                
                if self.is_at_end() {
                    return Err(ParseError::syntax_error(
                        "Unclosed parentheses",
                        start_span
                    ).with_help("Add ')' to close the expression"));
                }
                
                self.consume(TokenKind::RParen)?;
                let _end_span = self.previous().span;
                Ok(expr)
            }
            _ => Err(ParseError::syntax_error(
                format!("Expected expression, found '{}'", token.lexeme),
                token.span
            ).with_help("Expected a value, variable, or function call"))
        }
    }
    
    // Helper methods
    
    fn current(&self) -> &Token {
        &self.tokens[self.pos]
    }
    
    fn previous(&self) -> &Token {
        &self.tokens[self.pos - 1]
    }
    
    fn advance(&mut self) {
        if !self.is_at_end() {
            self.pos += 1;
        }
    }
    
    fn consume(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        if self.current().kind == expected {
            self.advance();
            Ok(())
        } else {
            let current = self.current().clone();
            let expected_name = match expected {
                TokenKind::Var => "'var'",
                TokenKind::If => "'if'",
                TokenKind::Else => "'else'",
                TokenKind::While => "'while'",
                TokenKind::Return => "'return'",
                TokenKind::Def => "'def'",
                TokenKind::Colon => "':'",
                TokenKind::LParen => "'('",
                TokenKind::RParen => "')'",
                TokenKind::LBrace => "'{'",
                TokenKind::RBrace => "'}'",
                TokenKind::Comma => "','",
                TokenKind::Semicolon => "';'",
                TokenKind::Assign => "'='",
                TokenKind::Eq => "'=='",
                TokenKind::Ne => "'!='",
                TokenKind::Lt => "'<'",
                TokenKind::Le => "'<='",
                TokenKind::Gt => "'>'",
                TokenKind::Ge => "'>='",
                TokenKind::Plus => "'+'",
                TokenKind::Minus => "'-'",
                TokenKind::Star => "'*'",
                TokenKind::Slash => "'/'",
                TokenKind::Percent => "'%'",
                TokenKind::Power => "'**'",
                TokenKind::And => "'and'",
                TokenKind::Or => "'or'",
                TokenKind::Not => "'not'",
                TokenKind::Newline => "newline",
                TokenKind::EOF => "end of file",
                _ => "token",
            };
            
            Err(ParseError::syntax_error(
                format!("Expected {}, found '{}'", expected_name, current.lexeme),
                current.span
            ))
        }
    }
    
    fn skip_newlines(&mut self) {
        while matches!(self.current().kind, TokenKind::Newline | TokenKind::Semicolon) {
            self.advance();
        }
    }
    
    fn is_at_end(&self) -> bool {
        matches!(self.current().kind, TokenKind::EOF)
    }
    
    fn recover(&mut self) {
        // Skip tokens until we find a statement boundary
        while !self.is_at_end() {
            match self.current().kind {
                TokenKind::Newline | TokenKind::Semicolon | 
                TokenKind::EOF | TokenKind::RBrace => {
                    break;
                }
                _ => {
                    self.advance();
                }
            }
        }
        
        // Skip any newlines after recovery
        self.skip_newlines();
    }
}

pub fn parse_program(code: &str) -> Result<Program, Vec<ParseError>> {
    let tokens = match lex(code) {
        Ok(tokens) => tokens,
        Err(err) => return Err(vec![err]),
    };
    
    let mut parser = Parser::new(&tokens, code);
    parser.parse_program()
}

// ==================== ERROR FORMATTING HELPERS ====================

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

// ==================== COMMON ERROR PATTERNS ====================

pub fn create_missing_semicolon_error(span: Span) -> ParseError {
    ParseError::syntax_error(
        "Missing ';' at end of statement",
        span
    ).with_help("Try adding ';' here")
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