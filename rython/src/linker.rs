use std::process::Command;
use std::path::Path;

pub fn manual_link(object_file: &str, output_name: &str) -> Result<(), String> {
    println!("Attempting manual linking...");
    
    // Use ld.exe directly instead of gcc
    let ld_binary = "bin/ld.exe";
    if !Path::new(ld_binary).exists() {
        return Err("ld.exe not found in bin/".to_string());
    }
    
    // Try to link with ld directly
    let ld_output = Command::new(ld_binary)
        .args(&[
            "-o", output_name,
            object_file,
            "-L./lib",
            "-lrython_runtime",
            "-lmsvcrt",
            "--entry", "main",
            "--subsystem", "console",
        ])
        .output()
        .map_err(|e| format!("Failed to run ld: {}", e))?;
        
    if !ld_output.status.success() {
        let error_msg = String::from_utf8_lossy(&ld_output.stderr);
        println!("LD linking failed: {}", error_msg);
        
        // Try with different entry point
        let ld_output = Command::new(ld_binary)
            .args(&[
                "-o", output_name,
                object_file,
                "-L./lib",
                "-lrython_runtime",
                "--entry", "_main",
                "--subsystem", "console",
            ])
            .output()
            .map_err(|e| format!("Failed to run ld (alternative): {}", e))?;
            
        if !ld_output.status.success() {
            let error_msg = String::from_utf8_lossy(&ld_output.stderr);
            return Err(format!("LD linking failed: {}", error_msg));
        }
    }
    
    println!("Manual linking successful!");
    Ok(())
}