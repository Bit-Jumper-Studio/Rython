// lib.rs - Main library module
pub mod backend;
pub mod compiler;
pub mod disk_cache;
pub mod dsl;
pub mod emitter;
pub mod lua_frontend;
pub mod lua_pool;
pub mod rcl_compiler;
pub mod rcl_integration;
pub mod ssd_injector;
pub mod cli;

// Re-export main components
pub use backend::{Backend, BackendRegistry, Target, Capability};
pub use compiler::{EarthngCompiler, CompilerConfig, compile, compile_with_hardware};
pub use lua_frontend::{parse_program, LuaFrontend};
pub use rcl_compiler::RclCompiler;
pub use rcl_integration::EarthngCompilerWithRcl;

// Type aliases for backward compatibility
pub mod parser {
    pub use crate::lua_frontend::{
        Program, Statement, Expr, Position, Span, Op,
        parse_program, ParseError,
        CompareOp, BoolOp, UnaryOp
    };
}