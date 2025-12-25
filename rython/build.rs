fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    
    // Get the target triple
    let target = std::env::var("TARGET").unwrap();
    
    // Handle MinGW-specific linking (Windows)
    if target.contains("windows-gnu") {
        println!("cargo:rustc-cfg=mingw_build");
        
        // Set linker flags for MinGW
        println!("cargo:rustc-link-arg=-static");
        println!("cargo:rustc-link-arg=-lmsvcrt");
        println!("cargo:rustc-link-arg=-lmingwex");
        println!("cargo:rustc-link-arg=-lmingw32");
        println!("cargo:rustc-link-arg=-lgcc");
        println!("cargo:rustc-link-arg=-Wl,-Bstatic");
        println!("cargo:rustc-link-arg=-Wl,--gc-sections");
        println!("cargo:rustc-link-arg=-Wl,--allow-multiple-definition");
    }
    // Handle Linux-specific linking
    else if target.contains("linux") {
        println!("cargo:rustc-cfg=linux_build");
        
        // Set linker flags for Linux
        println!("cargo:rustc-link-arg=-Wl,--gc-sections");
        
        // Optional: Add runtime library if needed
        // println!("cargo:rustc-link-arg=-lrt");
        // println!("cargo:rustc-link-arg=-ldl");
    }
    // Handle macOS-specific linking
    else if target.contains("darwin") || target.contains("apple") {
        println!("cargo:rustc-cfg=macos_build");
        
        // macOS linker flags
        println!("cargo:rustc-link-arg=-Wl,-dead_strip");
    }
    // Handle other Unix-like systems
    else if target.contains("bsd") || target.contains("solaris") {
        println!("cargo:rustc-cfg=unix_build");
        
        // Generic Unix linker flags
        println!("cargo:rustc-link-arg=-Wl,--gc-sections");
    }
    // Handle unknown targets with generic settings
    else {
        println!("cargo:warning=Building for unknown target: {}", target);
        println!("cargo:rustc-cfg=generic_build");
    }
}