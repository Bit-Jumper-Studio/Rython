use std::path::Path;

/// Find the NASM executable, checking local bin folder first
pub fn find_nasm() -> String {
    // Check local bin folder first
    if cfg!(target_os = "windows") {
        if Path::new("bin/nasm.exe").exists() {
            return "bin/nasm.exe".to_string();
        }
        "nasm.exe".to_string()
    } else {
        if Path::new("bin/nasm").exists() {
            return "bin/nasm".to_string();
        }
        "nasm".to_string()
    }
}

/// Find the linker executable (ld or link)
pub fn find_linker() -> String {
    if cfg!(target_os = "windows") {
        if Path::new("bin/link.exe").exists() {
            return "bin/link.exe".to_string();
        }
        if Path::new("bin/ld.exe").exists() {
            return "bin/ld.exe".to_string();
        }
        "link.exe".to_string()
    } else {
        if Path::new("bin/ld").exists() {
            return "bin/ld".to_string();
        }
        "ld".to_string()
    }
}

/// Check if a command exists in PATH or local bin folder
pub fn command_exists(cmd: &str) -> bool {
    if cfg!(target_os = "windows") {
        if Path::new(&format!("bin/{}.exe", cmd)).exists() {
            return true;
        }
        if Path::new(&format!("bin/{}", cmd)).exists() {
            return true;
        }
    } else {
        if Path::new(&format!("bin/{}", cmd)).exists() {
            return true;
        }
    }
    
    // Check in PATH
    std::process::Command::new(cmd)
        .arg("--version")
        .output()
        .is_ok()
}