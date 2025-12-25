use std::collections::HashMap;
use serde::{Serialize, Deserialize};

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
    Number(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Var(String),
    None,
    BinOp { left: Box<Expr>, op: Op, right: Box<Expr> },
    UnaryOp { op: UnaryOp, operand: Box<Expr> },
    BoolOp { op: BoolOp, values: Vec<Expr> },
    Compare { left: Box<Expr>, ops: Vec<CompareOp>, comparators: Vec<Expr> },
    Call { func: String, args: Vec<Expr>, kwargs: HashMap<String, Expr> },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    VarDecl { name: String, value: Expr, type_hint: Option<String> },
    Assign { target: String, value: Expr },
    AugAssign { target: String, op: Op, value: Expr },
    Expr(Expr),
    Return(Option<Expr>),
    If { condition: Expr, then_block: Vec<Statement>, elif_blocks: Vec<(Expr, Vec<Statement>)>, else_block: Option<Vec<Statement>> },
    While { condition: Expr, body: Vec<Statement>, orelse: Option<Vec<Statement>> },
    FunctionDef { name: String, args: Vec<String>, body: Vec<Statement> },
    Pass,
    Break,
    Continue,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    pub body: Vec<Statement>,
}

// ==================== TOKENS ====================

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Var, If, Elif, Else, While, For, In, Return, Def,
    And, Or, Not, Pass, Break, Continue,
    True, False, None,
    Number(i64), Float(f64), String(String),
    Identifier(String),
    Plus, Minus, Star, Slash, Percent, Power,
    Eq, Ne, Lt, Le, Gt, Ge, Assign,
    PlusAssign, MinusAssign, StarAssign, SlashAssign,
    LParen, RParen, LBrace, RBrace, Comma, Colon, Semicolon,
    Newline, EOF,
}

// ==================== LEXER ====================

pub fn lex(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    
    while let Some(&c) = chars.peek() {
        match c {
            ' ' | '\t' => { chars.next(); }
            '\n' => {
                tokens.push(Token::Newline);
                chars.next();
            }
            '#' => {
                while chars.next_if(|&c| c != '\n').is_some() {}
            }
            '"' | '\'' => {
                let quote = chars.next().unwrap();
                let mut s = String::new();
                while let Some(ch) = chars.next() {
                    if ch == quote { break; }
                    if ch == '\\' {
                        if let Some(escaped) = chars.next() {
                            s.push(match escaped {
                                'n' => '\n', 't' => '\t', 'r' => '\r',
                                '\\' => '\\', '\'' => '\'', '"' => '"',
                                _ => escaped,
                            });
                        }
                    } else {
                        s.push(ch);
                    }
                }
                tokens.push(Token::String(s));
            }
            '0'..='9' => {
                let mut num = String::new();
                while let Some(&d) = chars.peek() {
                    if d.is_numeric() || d == '.' {
                        num.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                if num.contains('.') {
                    tokens.push(Token::Float(num.parse().unwrap()));
                } else {
                    tokens.push(Token::Number(num.parse().unwrap()));
                }
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut ident = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        ident.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                tokens.push(match ident.as_str() {
                    "var" => Token::Var, "if" => Token::If, "elif" => Token::Elif,
                    "else" => Token::Else, "while" => Token::While, "for" => Token::For,
                    "in" => Token::In, "return" => Token::Return, "def" => Token::Def,
                    "and" => Token::And, "or" => Token::Or, "not" => Token::Not,
                    "pass" => Token::Pass, "break" => Token::Break, "continue" => Token::Continue,
                    "True" => Token::True, "False" => Token::False, "None" => Token::None,
                    _ => Token::Identifier(ident),
                });
            }
            '+' => {
                chars.next();
                if chars.next_if_eq(&'=').is_some() {
                    tokens.push(Token::PlusAssign);
                } else {
                    tokens.push(Token::Plus);
                }
            }
            '-' => {
                chars.next();
                if chars.next_if_eq(&'=').is_some() {
                    tokens.push(Token::MinusAssign);
                } else {
                    tokens.push(Token::Minus);
                }
            }
            '*' => {
                chars.next();
                if chars.next_if_eq(&'*').is_some() {
                    tokens.push(Token::Power);
                } else if chars.next_if_eq(&'=').is_some() {
                    tokens.push(Token::StarAssign);
                } else {
                    tokens.push(Token::Star);
                }
            }
            '/' => {
                chars.next();
                if chars.next_if_eq(&'=').is_some() {
                    tokens.push(Token::SlashAssign);
                } else {
                    tokens.push(Token::Slash);
                }
            }
            '%' => { chars.next(); tokens.push(Token::Percent); }
            '=' => {
                chars.next();
                if chars.next_if_eq(&'=').is_some() {
                    tokens.push(Token::Eq);
                } else {
                    tokens.push(Token::Assign);
                }
            }
            '!' => {
                chars.next();
                if chars.next_if_eq(&'=').is_some() {
                    tokens.push(Token::Ne);
                }
            }
            '<' => {
                chars.next();
                if chars.next_if_eq(&'=').is_some() {
                    tokens.push(Token::Le);
                } else {
                    tokens.push(Token::Lt);
                }
            }
            '>' => {
                chars.next();
                if chars.next_if_eq(&'=').is_some() {
                    tokens.push(Token::Ge);
                } else {
                    tokens.push(Token::Gt);
                }
            }
            '(' => { chars.next(); tokens.push(Token::LParen); }
            ')' => { chars.next(); tokens.push(Token::RParen); }
            '{' => { chars.next(); tokens.push(Token::LBrace); }
            '}' => { chars.next(); tokens.push(Token::RBrace); }
            ',' => { chars.next(); tokens.push(Token::Comma); }
            ':' => { chars.next(); tokens.push(Token::Colon); }
            ';' => { chars.next(); tokens.push(Token::Semicolon); }
            _ => { chars.next(); }
        }
    }
    
    tokens.push(Token::EOF);
    tokens
}

// ==================== PARSER ====================

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }
    
    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut statements = Vec::new();
        self.skip_newlines();
        
        while !self.is_at_end() {
            statements.push(self.parse_statement()?);
            self.skip_newlines();
        }
        
        Ok(Program { body: statements })
    }
    
    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.peek() {
            Token::Var => self.parse_var_decl(),
            Token::If => self.parse_if_statement(),
            Token::While => self.parse_while_statement(),
            Token::Return => self.parse_return_statement(),
            Token::Def => self.parse_function_def(),
            Token::Pass => { self.next(); self.skip_newlines(); Ok(Statement::Pass) }
            Token::Break => { self.next(); self.skip_newlines(); Ok(Statement::Break) }
            Token::Continue => { self.next(); self.skip_newlines(); Ok(Statement::Continue) }
            Token::Identifier(name) => {
                let name = name.clone();
                self.next();
                if matches!(self.peek(), Token::Assign) {
                    self.next();
                    let value = self.parse_expression()?;
                    self.skip_newlines();
                    Ok(Statement::Assign { target: name, value })
                } else if matches!(self.peek(), Token::PlusAssign | Token::MinusAssign | Token::StarAssign | Token::SlashAssign) {
                    let op = match self.next() {
                        Token::PlusAssign => Op::Add,
                        Token::MinusAssign => Op::Sub,
                        Token::StarAssign => Op::Mul,
                        Token::SlashAssign => Op::Div,
                        _ => unreachable!(),
                    };
                    let value = self.parse_expression()?;
                    self.skip_newlines();
                    Ok(Statement::AugAssign { target: name, op, value })
                } else {
                    self.pos -= 1;
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
    
    fn parse_var_decl(&mut self) -> Result<Statement, String> {
        self.consume(Token::Var)?;
        let name = match self.next() {
            Token::Identifier(n) => n,
            _ => return Err("Expected identifier".to_string()),
        };
        let type_hint = if matches!(self.peek(), Token::Colon) {
            self.next();
            match self.next() {
                Token::Identifier(t) => Some(t),
                _ => None,
            }
        } else {
            None
        };
        self.consume(Token::Assign)?;
        let value = self.parse_expression()?;
        self.skip_newlines();
        Ok(Statement::VarDecl { name, value, type_hint })
    }
    
    fn parse_if_statement(&mut self) -> Result<Statement, String> {
        self.consume(Token::If)?;
        let condition = self.parse_expression()?;
        self.consume(Token::Colon)?;
        self.skip_newlines();
        let then_block = self.parse_block()?;
        
        let mut elif_blocks = Vec::new();
        while matches!(self.peek(), Token::Elif) {
            self.next();
            let elif_cond = self.parse_expression()?;
            self.consume(Token::Colon)?;
            self.skip_newlines();
            let elif_body = self.parse_block()?;
            elif_blocks.push((elif_cond, elif_body));
        }
        
        let else_block = if matches!(self.peek(), Token::Else) {
            self.next();
            self.consume(Token::Colon)?;
            self.skip_newlines();
            Some(self.parse_block()?)
        } else {
            None
        };
        
        Ok(Statement::If { condition, then_block, elif_blocks, else_block })
    }
    
    fn parse_while_statement(&mut self) -> Result<Statement, String> {
        self.consume(Token::While)?;
        let condition = self.parse_expression()?;
        self.consume(Token::Colon)?;
        self.skip_newlines();
        let body = self.parse_block()?;
        Ok(Statement::While { condition, body, orelse: None })
    }
    
    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.consume(Token::Return)?;
        let expr = if matches!(self.peek(), Token::Newline | Token::EOF) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.skip_newlines();
        Ok(Statement::Return(expr))
    }
    
    fn parse_function_def(&mut self) -> Result<Statement, String> {
        self.consume(Token::Def)?;
        let name = match self.next() {
            Token::Identifier(n) => n,
            _ => return Err("Expected function name".to_string()),
        };
        self.consume(Token::LParen)?;
        let mut args = Vec::new();
        while !matches!(self.peek(), Token::RParen) {
            match self.next() {
                Token::Identifier(arg) => args.push(arg),
                _ => return Err("Expected argument name".to_string()),
            }
            if matches!(self.peek(), Token::Comma) {
                self.next();
            }
        }
        self.consume(Token::RParen)?;
        self.consume(Token::Colon)?;
        self.skip_newlines();
        let body = self.parse_block()?;
        Ok(Statement::FunctionDef { name, args, body })
    }
    
    fn parse_block(&mut self) -> Result<Vec<Statement>, String> {
        if matches!(self.peek(), Token::LBrace) {
            self.next();
            let mut statements = Vec::new();
            while !matches!(self.peek(), Token::RBrace) {
                statements.push(self.parse_statement()?);
                self.skip_newlines();
            }
            self.next();
            Ok(statements)
        } else {
            Ok(vec![self.parse_statement()?])
        }
    }
    
    fn parse_expression(&mut self) -> Result<Expr, String> {
        self.parse_or()
    }
    
    fn parse_or(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_and()?;
        while matches!(self.peek(), Token::Or) {
            self.next();
            let right = self.parse_and()?;
            left = Expr::BoolOp { op: BoolOp::Or, values: vec![left, right] };
        }
        Ok(left)
    }
    
    fn parse_and(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_not()?;
        while matches!(self.peek(), Token::And) {
            self.next();
            let right = self.parse_not()?;
            left = Expr::BoolOp { op: BoolOp::And, values: vec![left, right] };
        }
        Ok(left)
    }
    
    fn parse_not(&mut self) -> Result<Expr, String> {
        if matches!(self.peek(), Token::Not) {
            self.next();
            let operand = self.parse_not()?;
            Ok(Expr::UnaryOp { op: UnaryOp::Not, operand: Box::new(operand) })
        } else {
            self.parse_comparison()
        }
    }
    
    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let left = self.parse_addition()?;
        let mut ops = Vec::new();
        let mut comparators = Vec::new();
        
        while matches!(self.peek(), Token::Eq | Token::Ne | Token::Lt | Token::Le | Token::Gt | Token::Ge) {
            let op = match self.next() {
                Token::Eq => CompareOp::Eq,
                Token::Ne => CompareOp::Ne,
                Token::Lt => CompareOp::Lt,
                Token::Le => CompareOp::Le,
                Token::Gt => CompareOp::Gt,
                Token::Ge => CompareOp::Ge,
                _ => unreachable!(),
            };
            ops.push(op);
            comparators.push(self.parse_addition()?);
        }
        
        if ops.is_empty() {
            Ok(left)
        } else {
            Ok(Expr::Compare { left: Box::new(left), ops, comparators })
        }
    }
    
    fn parse_addition(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_multiplication()?;
        while matches!(self.peek(), Token::Plus | Token::Minus) {
            let op = match self.next() {
                Token::Plus => Op::Add,
                Token::Minus => Op::Sub,
                _ => unreachable!(),
            };
            let right = self.parse_multiplication()?;
            left = Expr::BinOp { left: Box::new(left), op, right: Box::new(right) };
        }
        Ok(left)
    }
    
    fn parse_multiplication(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_unary()?;
        while matches!(self.peek(), Token::Star | Token::Slash | Token::Percent) {
            let op = match self.next() {
                Token::Star => Op::Mul,
                Token::Slash => Op::Div,
                Token::Percent => Op::Mod,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            left = Expr::BinOp { left: Box::new(left), op, right: Box::new(right) };
        }
        Ok(left)
    }
    
    fn parse_unary(&mut self) -> Result<Expr, String> {
        if matches!(self.peek(), Token::Minus | Token::Plus) {
            let op = match self.next() {
                Token::Minus => UnaryOp::Minus,
                Token::Plus => UnaryOp::Plus,
                _ => unreachable!(),
            };
            let operand = self.parse_unary()?;
            Ok(Expr::UnaryOp { op, operand: Box::new(operand) })
        } else {
            self.parse_power()
        }
    }
    
    fn parse_power(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_primary()?;
        if matches!(self.peek(), Token::Power) {
            self.next();
            let right = self.parse_unary()?;
            left = Expr::BinOp { left: Box::new(left), op: Op::Pow, right: Box::new(right) };
        }
        Ok(left)
    }
    
    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.next() {
            Token::Number(n) => Ok(Expr::Number(n)),
            Token::Float(f) => Ok(Expr::Float(f)),
            Token::True => Ok(Expr::Boolean(true)),
            Token::False => Ok(Expr::Boolean(false)),
            Token::None => Ok(Expr::None),
            Token::String(s) => Ok(Expr::String(s)),
            Token::Identifier(name) => {
                if matches!(self.peek(), Token::LParen) {
                    self.next();
                    let mut args = Vec::new();
                    while !matches!(self.peek(), Token::RParen) {
                        args.push(self.parse_expression()?);
                        if matches!(self.peek(), Token::Comma) {
                            self.next();
                        }
                    }
                    self.consume(Token::RParen)?;
                    Ok(Expr::Call { func: name, args, kwargs: HashMap::new() })
                } else {
                    Ok(Expr::Var(name))
                }
            }
            Token::LParen => {
                let expr = self.parse_expression()?;
                self.consume(Token::RParen)?;
                Ok(expr)
            }
            _ => Err("Expected expression".to_string()),
        }
    }
    
    fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }
    
    fn next(&mut self) -> Token {
        let token = self.tokens[self.pos].clone();
        self.pos += 1;
        token
    }
    
    fn consume(&mut self, expected: Token) -> Result<(), String> {
        if self.peek() == &expected {
            self.next();
            Ok(())
        } else {
            Err(format!("Expected {:?}, got {:?}", expected, self.peek()))
        }
    }
    
    fn skip_newlines(&mut self) {
        while matches!(self.peek(), Token::Newline | Token::Semicolon) {
            self.next();
        }
    }
    
    fn is_at_end(&self) -> bool {
        matches!(self.peek(), Token::EOF)
    }
}

pub fn parse_program(code: &str) -> Result<Program, String> {
    let tokens = lex(code);
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}