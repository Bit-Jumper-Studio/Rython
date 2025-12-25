use std::process;

use rython::cli;

fn main() {
    if let Err(e) = cli::run() {
        eprintln!("ERROR: {}", e);
        process::exit(1);
    }
}