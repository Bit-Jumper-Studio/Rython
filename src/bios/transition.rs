/// Compact 512-byte bootloader
#[derive(Debug, Clone)]
pub struct CompactBootloader {
    code: Vec<u8>,
}

impl CompactBootloader {
    /// Create a new compact bootloader builder
    pub fn new() -> Self {
        Self {
            code: Vec::with_capacity(512),
        }
    }
    
    /// Build the bootloader
    pub fn create(&mut self) -> Result<Vec<u8>, String> {
        // 16-bit real mode bootloader
        let mut code = Vec::new();
        
        // BIOS parameter block (BPB)
        code.extend_from_slice(&[
            0xEB, 0x3C, 0x90,                         // Jump to boot code
            0x52, 0x79, 0x74, 0x68, 0x6F, 0x6E,       // "Earthang" OEM
            0x20, 0x20, 0x20,
            0x00, 0x02,                               // Bytes per sector
            0x01,                                     // Sectors per cluster
            0x01, 0x00,                               // Reserved sectors
            0x01,                                     // FAT copies
            0x00, 0x00,                               // Root entries
            0x00, 0x00,                               // Total sectors (small)
            0xF8,                                     // Media descriptor
            0x00, 0x00,                               // Sectors per FAT
            0x3F, 0x00,                               // Sectors per track
            0xFF, 0x00,                               // Heads
            0x00, 0x00, 0x00, 0x00,                   // Hidden sectors
            0x00, 0x00, 0x00, 0x00,                   // Total sectors (large)
        ]);
        
        // Extended BPB
        code.extend_from_slice(&[0x00; 32]);
        
        // Boot code
        code.extend_from_slice(&[
            0xFA,                                     // cli
            0x31, 0xC0,                               // xor ax, ax
            0x8E, 0xD8,                               // mov ds, ax
            0x8E, 0xC0,                               // mov es, ax
            0x8E, 0xD0,                               // mov ss, ax
            0xBC, 0x00, 0x7C,                         // mov sp, 0x7C00
            0xFB,                                     // sti
            0xFC,                                     // cld
            
            // Clear screen
            0xB8, 0x03, 0x00,                         // mov ax, 0x0003
            0xCD, 0x10,                               // int 0x10
            
            // Print message
            0xBE, 0x7A, 0x7C,                         // mov si, msg
            0xE8, 0x10, 0x00,                         // call print_string
            
            // Hang
            0xFA,                                     // cli
            0xF4,                                     // hlt
            0xEB, 0xFD,                               // jmp $
            
            // Print string subroutine
            0x60,                                     // pusha
            0xB4, 0x0E,                               // mov ah, 0x0E
            0xAC,                                     // .loop: lodsb
            0x08, 0xC0,                               // test al, al
            0x74, 0x04,                               // jz .done
            0xCD, 0x10,                               // int 0x10
            0xEB, 0xF8,                               // jmp .loop
            0x61,                                     // .done: popa
            0xC3,                                     // ret
            
            // Message
            0x52, 0x79, 0x74, 0x68, 0x6F, 0x6E, 0x20, 0x42, 0x6F, 0x6F,
            0x74, 0x6C, 0x6F, 0x61, 0x64, 0x65, 0x72, 0x00, // "Earthang Bootloader"
        ]);
        
        // Pad to 510 bytes
        while code.len() < 510 {
            code.push(0x00);
        }
        
        // Boot signature
        code.push(0x55);
        code.push(0xAA);
        
        self.code = code;
        Ok(self.code.clone())
    }
}

/// Micro 256-byte bootloader
#[derive(Debug, Clone)]
pub struct MicroBootloader {
    code: Vec<u8>,
}

impl MicroBootloader {
    /// Create a new micro bootloader builder
    pub fn new() -> Self {
        Self {
            code: Vec::with_capacity(256),
        }
    }
    
    /// Build the micro bootloader
    pub fn create(&mut self) -> Vec<u8> {
        let mut code = Vec::new();
        
        // Minimal bootloader that just prints a character
        code.extend_from_slice(&[
            0xFA,                                     // cli
            0x31, 0xC0,                               // xor ax, ax
            0x8E, 0xD8,                               // mov ds, ax
            0x8E, 0xC0,                               // mov es, ax
            0x8E, 0xD0,                               // mov ss, ax
            0xBC, 0x00, 0x7C,                         // mov sp, 0x7C00
            0xFB,                                     // sti
            0xFC,                                     // cld
            
            // Print 'R' in top-left corner
            0xB8, 0x00, 0xB8,                         // mov ax, 0xB800
            0x8E, 0xC0,                               // mov es, ax
            0x26, 0xC6, 0x06, 0x00, 0x00, 0x52,       // mov byte [es:0x0000], 'R'
            0x26, 0xC6, 0x06, 0x01, 0x00, 0x0F,       // mov byte [es:0x0001], 0x0F
            
            // Hang
            0xFA,                                     // cli
            0xF4,                                     // hlt
            0xEB, 0xFD,                               // jmp $
        ]);
        
        // Pad to 256 bytes
        while code.len() < 256 {
            code.push(0x00);
        }
        
        self.code = code;
        self.code.clone()
    }
}

/// Mode transition emitter for bit jumping
#[derive(Debug, Clone)]
pub struct ModeTransitionEmitter {
    current_mode: Mode,
    target_mode: Mode,
    features: Vec<Feature>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Mode {
    Real16,
    Protected32,
    Long64,
}

#[derive(Debug, Clone, PartialEq)]  // Added PartialEq here
pub enum Feature {
    SSE,
    AVX,
    AVX512,
    Paging,
    Graphics,
}

impl ModeTransitionEmitter {
    /// Create a new mode transition emitter
    pub fn new() -> Self {
        Self {
            current_mode: Mode::Real16,
            target_mode: Mode::Long64,
            features: vec![Feature::Paging],
        }
    }
    
    /// Set target mode
    pub fn set_target(&mut self, target: Mode) {
        self.target_mode = target;
    }
    
    /// Add feature to enable
    pub fn add_feature(&mut self, feature: Feature) {
        if !self.features.contains(&feature) {
            self.features.push(feature);
        }
    }
    
    /// Build bootloader with transitions - FIXED VERSION
    pub fn create_bootloader(&mut self) -> Result<Vec<u8>, String> {
        let mut code = Vec::new();
        
        match (self.current_mode.clone(), self.target_mode.clone()) {
            (Mode::Real16, Mode::Long64) => {
                // Start in 16-bit mode
                code.extend_from_slice(&self.create_16bit_header());
                code.extend_from_slice(&self.transition_to_32bit());
                code.extend_from_slice(&self.transition_to_64bit());
                
                // Enable features
                for feature in &self.features {
                    match feature {
                        Feature::SSE => code.extend_from_slice(&self.enable_sse()),
                        Feature::AVX => code.extend_from_slice(&self.enable_avx()),
                        Feature::AVX512 => code.extend_from_slice(&self.enable_avx512()),
                        Feature::Paging => code.extend_from_slice(&self.setup_paging()),
                        Feature::Graphics => code.extend_from_slice(&self.init_graphics()),
                    }
                }
                
                // CRITICAL FIX: Add infinite loop BEFORE any data
                code.extend_from_slice(&[
                    0xFA,  // cli
                    0xF4,  // hlt
                    0xEB, 0xFD,  // jmp $
                ]);
            }
            (Mode::Real16, Mode::Protected32) => {
                code.extend_from_slice(&self.create_16bit_header());
                code.extend_from_slice(&self.transition_to_32bit());
                
                // CRITICAL FIX: Add infinite loop BEFORE any data
                code.extend_from_slice(&[
                    0xFA,  // cli
                    0xF4,  // hlt
                    0xEB, 0xFD,  // jmp $
                ]);
            }
            _ => {
                return Err("Unsupported mode transition".to_string());
            }
        }
        
        // Pad to 510 bytes
        while code.len() < 510 {
            code.push(0x00);
        }
        
        // Boot signature
        code.push(0x55);
        code.push(0xAA);
        
        // Add string data AFTER boot signature (at 0x7E00)
        let string_data = b"Earthang Bootloader\0";
        code.extend_from_slice(string_data);
        
        Ok(code)
    }
    
    fn create_16bit_header(&self) -> Vec<u8> {
        vec![
            // Start at 0x7C00
            0xFA,                                     // cli
            0x31, 0xC0,                               // xor ax, ax
            0x8E, 0xD8,                               // mov ds, ax
            0x8E, 0xC0,                               // mov es, ax
            0x8E, 0xD0,                               // mov ss, ax
            0xBC, 0x00, 0x7C,                         // mov sp, 0x7C00
            0xFB,                                     // sti
            0xFC,                                     // cld
        ]
    }
    
    fn transition_to_32bit(&self) -> Vec<u8> {
        vec![
            // Enable A20 line
            0xE4, 0x92,                               // in al, 0x92
            0x0C, 0x02,                               // or al, 2
            0xE6, 0x92,                               // out 0x92, al
            
            // Load GDT
            0x0F, 0x01, 0x16, 0x70, 0x7C,             // lgdt [gdt32_desc]
            
            // Enter protected mode
            0x0F, 0x20, 0xC0,                         // mov eax, cr0
            0x66, 0x83, 0xC8, 0x01,                   // or eax, 1
            0x0F, 0x22, 0xC0,                         // mov cr0, eax
            
            // Far jump to 32-bit code
            0xEA, 0x78, 0x7C, 0x08, 0x00,             // jmp 0x08:0x7C78
        ]
    }
    
    fn transition_to_64bit(&self) -> Vec<u8> {
        vec![
            // 32-bit code starts here (after the far jump)
            // bits 32
            0x66, 0xB8, 0x10, 0x00,                   // mov ax, 0x10
            0x8E, 0xD8,                               // mov ds, ax
            0x8E, 0xC0,                               // mov es, ax
            0x8E, 0xE0,                               // mov fs, ax
            0x8E, 0xE8,                               // mov gs, ax
            0x8E, 0xD0,                               // mov ss, ax
            0xBC, 0x00, 0x00, 0x09, 0x00,             // mov esp, 0x90000
            
            // Setup PAE paging
            0xBF, 0x00, 0x10, 0x00, 0x00,             // mov edi, 0x1000
            0x0F, 0x22, 0xDF,                         // mov cr3, edi
            
            // Enable PAE
            0x0F, 0x20, 0xE0,                         // mov eax, cr4
            0x66, 0x0D, 0x20, 0x00,                   // or eax, 0x20
            0x0F, 0x22, 0xE0,                         // mov cr4, eax
            
            // Enable long mode
            0xB9, 0x80, 0x00, 0x00, 0xC0,             // mov ecx, 0xC0000080
            0x0F, 0x32,                               // rdmsr
            0x66, 0x0D, 0x00, 0x01, 0x00, 0x00,       // or eax, 0x100
            0x0F, 0x30,                               // wrmsr
            
            // Enable paging
            0x0F, 0x20, 0xC0,                         // mov eax, cr0
            0x66, 0x0D, 0x00, 0x00, 0x00, 0x80,       // or eax, 0x80000000
            0x0F, 0x22, 0xC0,                         // mov cr0, eax
            
            // Load 64-bit GDT
            0x0F, 0x01, 0x16, 0x90, 0x7C,             // lgdt [gdt64_desc]
            
            // Far jump to 64-bit code
            0xEA, 0xA0, 0x7C, 0x08, 0x00,             // jmp 0x08:0x7CA0
        ]
    }
    
    fn enable_sse(&self) -> Vec<u8> {
        vec![
            // 64-bit code
            0x0F, 0x20, 0xE0,                         // mov rax, cr4
            0x48, 0x0D, 0x00, 0x00, 0x02, 0x00,       // or rax, 0x200
            0x0F, 0x22, 0xE0,                         // mov cr4, rax
            0x0F, 0xAE, 0xF8,                         // stmxcsr [rax]
        ]
    }
    
    fn enable_avx(&self) -> Vec<u8> {
        vec![
            0x0F, 0x01, 0xD0,                         // xgetbv
            0x48, 0x0D, 0x00, 0x00, 0x00, 0x06,       // or rax, 0x600
            0x0F, 0x01, 0xD1,                         // xsetbv
            0xC4, 0xE1, 0xF9, 0x7E, 0xC0,             // vmovq xmm0, rax
        ]
    }
    
    fn enable_avx512(&self) -> Vec<u8> {
        vec![
            0x0F, 0x01, 0xD0,                         // xgetbv
            0x48, 0x0D, 0x00, 0x00, 0x00, 0xE0,       // or rax, 0xE0
            0x0F, 0x01, 0xD1,                         // xsetbv
        ]
    }
    
    fn setup_paging(&self) -> Vec<u8> {
        vec![
            // Simple identity paging setup
            0x48, 0xC7, 0xC0, 0x00, 0x10, 0x00, 0x00, // mov rax, 0x1000
            0x48, 0x89, 0xC7,                         // mov rdi, rax
            0x48, 0xC7, 0xC0, 0x03, 0x00, 0x00, 0x00, // mov rax, 0x0000000000000003
            0x48, 0xC7, 0xC1, 0x00, 0x01, 0x00, 0x00, // mov rcx, 0x100
            0xF3, 0x48, 0xAB,                         // rep stosq
        ]
    }
    
    fn init_graphics(&self) -> Vec<u8> {
        vec![
            // Set VGA mode 13h (320x200, 256 colors)
            0xB8, 0x13, 0x00,                         // mov ax, 0x13
            0xCD, 0x10,                               // int 0x10
        ]
    }
}

/// Graphics kernel for visual output
#[derive(Debug, Clone)]
pub struct GraphicsKernel {
    pub entry_point: u64,
    graphics_code: Vec<u8>,
    #[allow(dead_code)]
    vram_address: u64,
}

impl GraphicsKernel {
    /// Create a new graphics kernel
    pub fn new() -> Self {
        Self {
            entry_point: 0x10000,
            graphics_code: Vec::new(),
            vram_address: 0xA0000, // VGA memory
        }
    }
    
    /// Load graphics code
    pub fn load_graphics_code(&mut self, code: &[u8]) {
        self.graphics_code = code.to_vec();
    }
    
    /// Create a disk image with the kernel
    pub fn create_disk_image(&self) -> Vec<u8> {
        let mut disk = Vec::new();
        
        // Boot sector (512 bytes)
        let mut bootloader = CompactBootloader::new();
        if let Ok(boot) = bootloader.create() {
            disk.extend_from_slice(&boot);
        } else {
            // Default boot sector
            for _ in 0..512 {
                disk.push(0x00);
            }
            disk[510] = 0x55;
            disk[511] = 0xAA;
        }
        
        // Pad to cylinder boundary (63 sectors * 512 bytes = 32256 bytes)
        while disk.len() < 32256 {
            disk.push(0x00);
        }
        
        // Load graphics kernel at sector 63 (0x7E00 in memory)
        let kernel_offset = self.entry_point as usize - 0x7C00;
        while disk.len() < kernel_offset {
            disk.push(0x00);
        }
        
        // Add graphics code
        disk.extend_from_slice(&self.graphics_code);
        
        // Pad to 1.44MB floppy size (1474560 bytes)
        while disk.len() < 1474560 {
            disk.push(0x00);
        }
        
        disk
    }
    
    /// Simple graphics example: Draw gradient
    pub fn create_gradient_example(&mut self) {
        let mut code = Vec::new();
        
        // 64-bit graphics code
        code.extend_from_slice(&[
            // Switch to mode 13h
            0xB8, 0x13, 0x00,                         // mov ax, 0x0013
            0xCD, 0x10,                               // int 0x10
            
            // Setup VGA memory pointer
            0x48, 0xC7, 0xC0, 0x00, 0xA0, 0x00, 0x00, // mov rax, 0xA000
            0x48, 0xC1, 0xE0, 0x04,                   // shl rax, 4
            0x48, 0x89, 0xC7,                         // mov rdi, rax
            
            // Draw gradient
            0x48, 0xC7, 0xC1, 0x40, 0x1F, 0x00, 0x00, // mov rcx, 0x1F40 (320*200/8)
            0x31, 0xD2,                               // xor edx, edx
            0x31, 0xDB,                               // xor ebx, ebx
            
            0x88, 0xD8,                               // .loop: mov al, bl
            0xAA,                                     // stosb
            0xFE, 0xC3,                               // inc bl
            0x48, 0xFF, 0xC2,                         // inc rdx
            0x48, 0x39, 0xD1,                         // cmp rcx, rdx
            0x75, 0xF5,                               // jne .loop
            
            // Hang
            0xFA,                                     // cli
            0xF4,                                     // hlt
            0xEB, 0xFD,                               // jmp $
        ]);
        
        self.graphics_code = code;
    }
}

/// Create a simple hello world bootloader
pub fn create_hello_bootloader() -> Vec<u8> {
    let mut code = Vec::new();
    
    code.extend_from_slice(&[
        0xFA,                                     // cli
        0x31, 0xC0,                               // xor ax, ax
        0x8E, 0xD8,                               // mov ds, ax
        0x8E, 0xC0,                               // mov es, ax
        0x8E, 0xD0,                               // mov ss, ax
        0xBC, 0x00, 0x7C,                         // mov sp, 0x7C00
        0xFB,                                     // sti
        0xFC,                                     // cld
        
        // Clear screen
        0xB8, 0x03, 0x00,                         // mov ax, 0x0003
        0xCD, 0x10,                               // int 0x10
        
        // Print message
        0xB8, 0x00, 0xB8,                         // mov ax, 0xB800
        0x8E, 0xC0,                               // mov es, ax
        0xBF, 0x00, 0x00,                         // mov di, 0x0000
        
        // "Hello, Earthang!"
        0xB0, 0x48, 0xAB,                         // mov al, 'H'; stosw
        0xB0, 0x65, 0xAB,                         // mov al, 'e'; stosw
        0xB0, 0x6C, 0xAB,                         // mov al, 'l'; stosw
        0xB0, 0x6C, 0xAB,                         // mov al, 'l'; stosw
        0xB0, 0x6F, 0xAB,                         // mov al, 'o'; stosw
        0xB0, 0x2C, 0xAB,                         // mov al, ','; stosw
        0xB0, 0x20, 0xAB,                         // mov al, ' '; stosw
        0xB0, 0x52, 0xAB,                         // mov al, 'R'; stosw
        0xB0, 0x79, 0xAB,                         // mov al, 'y'; stosw
        0xB0, 0x74, 0xAB,                         // mov al, 't'; stosw
        0xB0, 0x68, 0xAB,                         // mov al, 'h'; stosw
        0xB0, 0x6F, 0xAB,                         // mov al, 'o'; stosw
        0xB0, 0x6E, 0xAB,                         // mov al, 'n'; stosw
        0xB0, 0x21, 0xAB,                         // mov al, '!'; stosw
        
        // Hang
        0xFA,                                     // cli
        0xF4,                                     // hlt
        0xEB, 0xFD,                               // jmp $
    ]);
    
    // Pad to 510 bytes
    while code.len() < 510 {
        code.push(0x00);
    }
    
    // Boot signature
    code.push(0x55);
    code.push(0xAA);
    
    code
}

// Public API
pub use Mode::*;
pub use Feature::*;

// Default implementations
impl Default for CompactBootloader {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for MicroBootloader {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for ModeTransitionEmitter {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for GraphicsKernel {
    fn default() -> Self {
        Self::new()
    }
}