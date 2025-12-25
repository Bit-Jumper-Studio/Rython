pub mod parser;
pub mod emitter;
pub mod compiler;
pub mod linker;
pub mod utils;
pub mod bios;
pub mod cli;
pub mod backend; 
pub mod rcl_compiler;      
pub mod rcl_integration;   

// Re-export commonly used items
pub use compiler::*;
pub use emitter::*;
pub use parser::*;
pub use cli::*;
pub use backend::*; 
pub use rcl_compiler::*;      
pub use rcl_integration::*;