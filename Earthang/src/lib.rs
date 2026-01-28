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
pub mod backend;
pub mod compiler;
pub mod disk_cache;
pub mod dsl;
pub mod emitter;
pub mod extension;
pub mod lua_frontend;
pub mod lua_pool;
pub mod cli;

pub use backend::{Backend, BackendRegistry, Target, Capability};
pub use compiler::{EarthangCompiler, CompilerConfig, compile, compile_with_hardware};
pub use lua_frontend::{parse_program, LuaFrontend};
pub use extension::{EarthngModule, AssemblyEmitter, BasicAssemblyEmitter, ExtensionRegistry, MathModule, StringModule, SystemModule};  // NEW

pub mod parser {
    pub use crate::lua_frontend::{
        Program, Statement, Expr, Position, Span, Op,
        parse_program, ParseError,
        CompareOp, BoolOp, UnaryOp
    };
}