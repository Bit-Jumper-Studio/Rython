use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct HardwareDSL {
    pub device_registry: HashMap<String, HardwareDevice>,
    pub assembly_cache: Vec<String>,
    pub current_device: Option<String>,
}

#[derive(Debug, Clone)]
pub struct HardwareDevice {
    pub name: String,
    pub device_type: DeviceType,
    pub registers: HashMap<String, u64>,
    pub interrupts: Vec<InterruptHandler>,
    pub dma_channels: Vec<DmaChannel>,
    pub special_instructions: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeviceType {
    GPU,
    NetworkCard,
    StorageController,
    SoundCard,
    USBCard,
    PCIeDevice,
    Custom(String),
}

#[derive(Debug, Clone)]
pub struct InterruptHandler {
    pub irq: u8,
    pub handler_address: u64,
    pub assembly_stub: String,
}

#[derive(Debug, Clone)]
pub struct DmaChannel {
    pub channel: u8,
    pub base_address: u64,
    pub page_address: u64,
    pub count_register: u64,
    pub mode_register: u64,
}

impl HardwareDSL {
    pub fn new() -> Self {
        let mut dsl = Self {
            device_registry: HashMap::new(),
            assembly_cache: Vec::new(),
            current_device: None,
        };
        
        // Register common hardware devices
        dsl.register_default_devices();
        dsl
    }
    
    fn register_default_devices(&mut self) {
        // GPU (VGA/Graphics)
        let mut gpu = HardwareDevice::new("gpu", DeviceType::GPU);
        gpu.registers.insert("CRTC_INDEX".to_string(), 0x3D4);
        gpu.registers.insert("CRTC_DATA".to_string(), 0x3D5);
        gpu.registers.insert("INPUT_STATUS".to_string(), 0x3DA);
        gpu.registers.insert("VGA_ENABLE".to_string(), 0x3C2);
        gpu.registers.insert("MEMORY_MODE".to_string(), 0x3C4);
        gpu.registers.insert("COLOR_PLANE".to_string(), 0x3C5);
        gpu.special_instructions.push("gpu_flush_fifo".to_string());
        gpu.special_instructions.push("gpu_wait_vsync".to_string());
        self.device_registry.insert("gpu".to_string(), gpu);
        
        // Network Card (NE2000 compatible)
        let mut net = HardwareDevice::new("network_card", DeviceType::NetworkCard);
        net.registers.insert("COMMAND".to_string(), 0x300);
        net.registers.insert("PAGE_START".to_string(), 0x301);
        net.registers.insert("PAGE_STOP".to_string(), 0x302);
        net.registers.insert("BOUNDARY".to_string(), 0x303);
        net.registers.insert("TRANSMIT_STATUS".to_string(), 0x304);
        net.registers.insert("TRANSMIT_COMMAND".to_string(), 0x305);
        net.registers.insert("INTERRUPT_STATUS".to_string(), 0x307);
        net.registers.insert("DMA_CONFIG".to_string(), 0x30A);
        
        // Add DMA channels
        net.dma_channels.push(DmaChannel {
            channel: 1,
            base_address: 0x4000,
            page_address: 0x87,
            count_register: 0x0B,
            mode_register: 0x0C,
        });
        
        self.device_registry.insert("network_card".to_string(), net);
        
        // Storage Controller (ATA/IDE)
        let mut storage = HardwareDevice::new("storage", DeviceType::StorageController);
        storage.registers.insert("DATA".to_string(), 0x1F0);
        storage.registers.insert("ERROR".to_string(), 0x1F1);
        storage.registers.insert("SECTOR_COUNT".to_string(), 0x1F2);
        storage.registers.insert("LBA_LOW".to_string(), 0x1F3);
        storage.registers.insert("LBA_MID".to_string(), 0x1F4);
        storage.registers.insert("LBA_HIGH".to_string(), 0x1F5);
        storage.registers.insert("DRIVE_HEAD".to_string(), 0x1F6);
        storage.registers.insert("COMMAND".to_string(), 0x1F7);
        storage.registers.insert("STATUS".to_string(), 0x1F7);
        self.device_registry.insert("storage".to_string(), storage);
        
        // Sound Card (Sound Blaster 16)
        let mut sound = HardwareDevice::new("sound", DeviceType::SoundCard);
        sound.registers.insert("RESET".to_string(), 0x226);
        sound.registers.insert("READ_DATA".to_string(), 0x22A);
        sound.registers.insert("WRITE_DATA".to_string(), 0x22C);
        sound.registers.insert("WRITE_STATUS".to_string(), 0x22C);
        sound.registers.insert("MIXER_INDEX".to_string(), 0x224);
        sound.registers.insert("MIXER_DATA".to_string(), 0x225);
        sound.registers.insert("IRQ_ACK".to_string(), 0x22F);
        self.device_registry.insert("sound".to_string(), sound);
    }
    
    /// Parse hardware DSL syntax
    pub fn parse_hardware_statement(&mut self, stmt: &str) -> Result<Vec<String>, String> {
        let trimmed = stmt.trim();
        
        // Check for device decorator
        if trimmed.starts_with('@') {
            return self.parse_device_decorator(trimmed);
        }
        
        // Check for hardware intrinsic
        if trimmed.starts_with("write_register") || trimmed.starts_with("read_register") ||
           trimmed.starts_with("dma_transfer") || trimmed.starts_with("port_in") ||
           trimmed.starts_with("port_out") {
            return self.parse_hardware_intrinsic(trimmed);
        }
        
        Err("Not a hardware DSL statement".to_string())
    }
    
    fn parse_device_decorator(&mut self, decorator: &str) -> Result<Vec<String>, String> {
        let parts: Vec<&str> = decorator.split('.').collect();
        if parts.len() < 2 {
            return Err("Invalid device decorator format".to_string());
        }
        
        let device_name = parts[0].trim_start_matches('@').trim();
        let operation = parts[1].trim();
        
        if !self.device_registry.contains_key(device_name) {
            return Err(format!("Unknown device: {}", device_name));
        }
        
        self.current_device = Some(device_name.to_string());
        
        // Generate device-specific assembly header
        let mut asm = Vec::new();
        asm.push(format!("; Device function for {}", device_name));
        
        match operation {
            "def" => {
                // Function definition for device
                asm.push("    ; Device function stub".to_string());
            }
            _ => {
                return Err(format!("Unknown device operation: {}", operation));
            }
        }
        
        Ok(asm)
    }
    
    fn parse_hardware_intrinsic(&mut self, intrinsic: &str) -> Result<Vec<String>, String> {
        // Parse function-like intrinsics
        if let Some((func_name, args)) = intrinsic.split_once('(') {
            let func_name = func_name.trim();
            let args = args.trim_end_matches(')');
            let arg_list: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
            
            match func_name {
                "write_register" => {
                    if arg_list.len() != 2 {
                        return Err("write_register requires 2 arguments: register, value".to_string());
                    }
                    return self.generate_write_register(&arg_list[0], &arg_list[1]);
                }
                "read_register" => {
                    if arg_list.len() != 1 {
                        return Err("read_register requires 1 argument: register".to_string());
                    }
                    return self.generate_read_register(&arg_list[0]);
                }
                "dma_transfer" => {
                    if arg_list.len() != 2 {
                        return Err("dma_transfer requires 2 arguments: address, data".to_string());
                    }
                    return self.generate_dma_transfer(&arg_list[0], &arg_list[1]);
                }
                "port_in" => {
                    if arg_list.len() != 1 {
                        return Err("port_in requires 1 argument: port".to_string());
                    }
                    return self.generate_port_in(&arg_list[0]);
                }
                "port_out" => {
                    if arg_list.len() != 2 {
                        return Err("port_out requires 2 arguments: port, value".to_string());
                    }
                    return self.generate_port_out(&arg_list[0], &arg_list[1]);
                }
                _ => {
                    return Err(format!("Unknown hardware intrinsic: {}", func_name));
                }
            }
        }
        
        Err("Invalid hardware intrinsic syntax".to_string())
    }
    
    fn generate_write_register(&self, register: &str, value: &str) -> Result<Vec<String>, String> {
        let mut asm = Vec::new();
        
        // Try to parse as hex
        let reg_val = if register.starts_with("0x") {
            u64::from_str_radix(&register[2..], 16)
                .map_err(|e| format!("Invalid hex register: {}", e))?
        } else {
            register.parse::<u64>()
                .map_err(|e| format!("Invalid register number: {}", e))?
        };
        
        let data_val = if value.starts_with("0x") {
            u64::from_str_radix(&value[2..], 16)
                .map_err(|e| format!("Invalid hex value: {}", e))?
        } else {
            value.parse::<u64>()
                .map_err(|e| format!("Invalid value: {}", e))?
        };
        
        // Generate appropriate assembly based on register size
        if reg_val <= 0xFF {
            // 8-bit port
            asm.push(format!("    ; write_register({}, {})", register, value));
            asm.push("    mov dx, ax".to_string()); // Save value
            asm.push(format!("    mov ax, {:#x}", reg_val));
            asm.push("    mov dx, ax".to_string());
            asm.push(format!("    mov al, {:#x}", data_val & 0xFF));
            asm.push("    out dx, al".to_string());
        } else if reg_val <= 0xFFFF {
            // 16-bit port
            asm.push(format!("    ; write_register({}, {})", register, value));
            asm.push(format!("    mov dx, {:#x}", reg_val));
            asm.push(format!("    mov ax, {:#x}", data_val & 0xFFFF));
            asm.push("    out dx, ax".to_string());
        } else {
            // Memory-mapped I/O
            asm.push(format!("    ; MMIO write_register({}, {})", register, value));
            asm.push(format!("    mov rax, {:#x}", reg_val));
            asm.push("    mov rdi, rax".to_string());
            asm.push(format!("    mov rax, {:#x}", data_val));
            asm.push("    mov [rdi], rax".to_string());
        }
        
        Ok(asm)
    }
    
    fn generate_read_register(&self, register: &str) -> Result<Vec<String>, String> {
        let mut asm = Vec::new();
        
        let reg_val = if register.starts_with("0x") {
            u64::from_str_radix(&register[2..], 16)
                .map_err(|e| format!("Invalid hex register: {}", e))?
        } else {
            register.parse::<u64>()
                .map_err(|e| format!("Invalid register number: {}", e))?
        };
        
        // Generate appropriate assembly
        if reg_val <= 0xFF {
            // 8-bit port
            asm.push(format!("    ; read_register({})", register));
            asm.push(format!("    mov dx, {:#x}", reg_val));
            asm.push("    in al, dx".to_string());
            asm.push("    movzx rax, al".to_string());
        } else if reg_val <= 0xFFFF {
            // 16-bit port
            asm.push(format!("    ; read_register({})", register));
            asm.push(format!("    mov dx, {:#x}", reg_val));
            asm.push("    in ax, dx".to_string());
            asm.push("    movzx rax, ax".to_string());
        } else {
            // Memory-mapped I/O
            asm.push(format!("    ; MMIO read_register({})", register));
            asm.push(format!("    mov rax, {:#x}", reg_val));
            asm.push("    mov rdi, rax".to_string());
            asm.push("    mov rax, [rdi]".to_string());
        }
        
        Ok(asm)
    }
    
    fn generate_dma_transfer(&self, address: &str, data: &str) -> Result<Vec<String>, String> {
        let mut asm = Vec::new();
        
        // Parse address
        let addr_val = if address.starts_with("0x") {
            u64::from_str_radix(&address[2..], 16)
                .map_err(|e| format!("Invalid hex address: {}", e))?
        } else {
            address.parse::<u64>()
                .map_err(|e| format!("Invalid address: {}", e))?
        };
        
        // Handle different data types
        asm.push(format!("    ; dma_transfer({}, {})", address, data));
        
        if data.starts_with("buffer_addr") || data.contains("data") {
            // Variable or function argument
            asm.push("    ; Setting up DMA transfer".to_string());
            asm.push("    cli  ; Disable interrupts during DMA".to_string());
            
            // Program DMA controller (assuming channel 1)
            asm.push("    ; Program DMA channel 1".to_string());
            asm.push("    mov al, 0x04  ; Mask channel 1".to_string());
            asm.push("    out 0x0A, al".to_string());
            
            // Set transfer mode
            asm.push("    mov al, 0x45  ; Single transfer, auto-init, read".to_string());
            asm.push("    out 0x0B, al".to_string());
            
            // Set address
            asm.push(format!("    mov ax, {:#x}  ; Low word of address", addr_val & 0xFFFF));
            asm.push("    out 0x02, al".to_string());
            asm.push("    mov al, ah".to_string());
            asm.push("    out 0x02, al".to_string());
            
            // Set page
            asm.push(format!("    mov al, {:#x}  ; Page register", (addr_val >> 16) & 0xFF));
            asm.push("    out 0x83, al".to_string());
            
            // Set count (assume 512 bytes for now)
            asm.push("    mov ax, 511  ; Count - 1 (512 bytes)".to_string());
            asm.push("    out 0x03, al".to_string());
            asm.push("    mov al, ah".to_string());
            asm.push("    out 0x03, al".to_string());
            
            // Unmask channel
            asm.push("    mov al, 0x01  ; Unmask channel 1".to_string());
            asm.push("    out 0x0A, al".to_string());
            
            asm.push("    sti  ; Re-enable interrupts".to_string());
        } else {
            // Direct data
            let data_val = if data.starts_with("0x") {
                u64::from_str_radix(&data[2..], 16)
                    .map_err(|e| format!("Invalid hex data: {}", e))?
            } else {
                data.parse::<u64>()
                    .map_err(|e| format!("Invalid data: {}", e))?
            };
            
            asm.push(format!("    mov rax, {:#x}", data_val));
            asm.push(format!("    mov [0x{:x}], rax", addr_val));
        }
        
        Ok(asm)
    }
    
    fn generate_port_in(&self, port: &str) -> Result<Vec<String>, String> {
        let mut asm = Vec::new();
        
        let port_val = if port.starts_with("0x") {
            u16::from_str_radix(&port[2..], 16)
                .map_err(|e| format!("Invalid hex port: {}", e))?
        } else {
            port.parse::<u16>()
                .map_err(|e| format!("Invalid port: {}", e))?
        };
        
        asm.push(format!("    ; port_in({})", port));
        asm.push(format!("    mov dx, {:#x}", port_val));
        
        // Choose appropriate instruction based on expected data size
        if port_val <= 0xFF {
            asm.push("    in al, dx".to_string());
            asm.push("    movzx rax, al".to_string());
        } else {
            asm.push("    in ax, dx".to_string());
            asm.push("    movzx rax, ax".to_string());
        }
        
        Ok(asm)
    }
    
    fn generate_port_out(&self, port: &str, value: &str) -> Result<Vec<String>, String> {
        let mut asm = Vec::new();
        
        let port_val = if port.starts_with("0x") {
            u16::from_str_radix(&port[2..], 16)
                .map_err(|e| format!("Invalid hex port: {}", e))?
        } else {
            port.parse::<u16>()
                .map_err(|e| format!("Invalid port: {}", e))?
        };
        
        let data_val = if value.starts_with("0x") {
            u16::from_str_radix(&value[2..], 16)
                .map_err(|e| format!("Invalid hex value: {}", e))?
        } else {
            value.parse::<u16>()
                .map_err(|e| format!("Invalid value: {}", e))?
        };
        
        asm.push(format!("    ; port_out({}, {})", port, value));
        asm.push(format!("    mov dx, {:#x}", port_val));
        
        if port_val <= 0xFF {
            asm.push(format!("    mov al, {:#x}", data_val & 0xFF));
            asm.push("    out dx, al".to_string());
        } else {
            asm.push(format!("    mov ax, {:#x}", data_val));
            asm.push("    out dx, ax".to_string());
        }
        
        Ok(asm)
    }
    
    /// Generate device-specific function prologue
    pub fn generate_device_function_prologue(&self, device_name: &str, func_name: &str) -> Result<String, String> {
        let device = self.device_registry.get(device_name)
            .ok_or_else(|| format!("Unknown device: {}", device_name))?;
        
        let mut asm = String::new();
        
        asm.push_str(&format!("; {} function: {}\n", device_name, func_name));
        asm.push_str("    ; Save registers\n");
        asm.push_str("    push rbp\n");
        asm.push_str("    mov rbp, rsp\n");
        asm.push_str("    push rbx\n");
        asm.push_str("    push rcx\n");
        asm.push_str("    push rdx\n");
        asm.push_str("    push rsi\n");
        asm.push_str("    push rdi\n\n");
        
        // Device-specific setup
        match device.device_type {
            DeviceType::GPU => {
                asm.push_str("    ; GPU setup\n");
                asm.push_str("    ; Save current video mode\n");
                asm.push_str("    mov ah, 0x0F\n");
                asm.push_str("    int 0x10\n");
                asm.push_str("    push ax\n\n");
            }
            DeviceType::NetworkCard => {
                asm.push_str("    ; Network card setup\n");
                asm.push_str("    ; Initialize network card\n");
                asm.push_str("    mov dx, 0x300\n");
                asm.push_str("    mov al, 0x21  ; Stop, DMA abort\n");
                asm.push_str("    out dx, al\n");
                asm.push_str("    ; Wait for reset\n");
                asm.push_str("    call delay_ms\n\n");
            }
            DeviceType::StorageController => {
                asm.push_str("    ; Storage controller setup\n");
                asm.push_str("    ; Select drive 0 (master)\n");
                asm.push_str("    mov dx, 0x1F6\n");
                asm.push_str("    mov al, 0xA0\n");
                asm.push_str("    out dx, al\n\n");
            }
            _ => {
                asm.push_str("    ; Generic device setup\n");
            }
        }
        
        Ok(asm)
    }
    
    /// Generate device-specific function epilogue
    pub fn generate_device_function_epilogue(&self, device_name: &str) -> Result<String, String> {
        let device = self.device_registry.get(device_name)
            .ok_or_else(|| format!("Unknown device: {}", device_name))?;
        
        let mut asm = String::new();
        
        // Device-specific cleanup
        match device.device_type {
            DeviceType::GPU => {
                asm.push_str("    ; GPU cleanup\n");
                asm.push_str("    ; Restore video mode\n");
                asm.push_str("    pop ax\n");
                asm.push_str("    mov ah, 0x00\n");
                asm.push_str("    int 0x10\n\n");
            }
            DeviceType::NetworkCard => {
                asm.push_str("    ; Network card cleanup\n");
                asm.push_str("    ; Disable network card interrupts\n");
                asm.push_str("    mov dx, 0x300\n");
                asm.push_str("    mov al, 0x21\n");
                asm.push_str("    out dx, al\n\n");
            }
            _ => {}
        }
        
        asm.push_str("    ; Restore registers\n");
        asm.push_str("    pop rdi\n");
        asm.push_str("    pop rsi\n");
        asm.push_str("    pop rdx\n");
        asm.push_str("    pop rcx\n");
        asm.push_str("    pop rbx\n");
        asm.push_str("    mov rsp, rbp\n");
        asm.push_str("    pop rbp\n");
        asm.push_str("    ret\n");
        
        Ok(asm)
    }
    
    /// Register a custom hardware device
    pub fn register_device(&mut self, name: &str, device_type: DeviceType) -> &mut HardwareDevice {
        let device = HardwareDevice::new(name, device_type);
        self.device_registry.insert(name.to_string(), device);
        self.device_registry.get_mut(name).unwrap()
    }
    
    /// Generate complete hardware access library
    pub fn generate_hardware_library(&self) -> String {
        let mut lib = String::new();
        
        lib.push_str("; ========== HARDWARE DIRECT ACCESS LIBRARY ==========\n\n");
        
        // Generic hardware functions
        lib.push_str("; Generic port I/O functions\n");
        lib.push_str("port_in_8:\n");
        lib.push_str("    ; Input from 8-bit port\n");
        lib.push_str("    ; Input: DX = port\n");
        lib.push_str("    ; Output: AL = value\n");
        lib.push_str("    in al, dx\n");
        lib.push_str("    ret\n\n");
        
        lib.push_str("port_out_8:\n");
        lib.push_str("    ; Output to 8-bit port\n");
        lib.push_str("    ; Input: DX = port, AL = value\n");
        lib.push_str("    out dx, al\n");
        lib.push_str("    ret\n\n");
        
        lib.push_str("port_in_16:\n");
        lib.push_str("    ; Input from 16-bit port\n");
        lib.push_str("    ; Input: DX = port\n");
        lib.push_str("    ; Output: AX = value\n");
        lib.push_str("    in ax, dx\n");
        lib.push_str("    ret\n\n");
        
        lib.push_str("port_out_16:\n");
        lib.push_str("    ; Output to 16-bit port\n");
        lib.push_str("    ; Input: DX = port, AX = value\n");
        lib.push_str("    out dx, ax\n");
        lib.push_str("    ret\n\n");
        
        // DMA functions
        lib.push_str("; DMA transfer functions\n");
        lib.push_str("setup_dma:\n");
        lib.push_str("    ; Setup DMA transfer\n");
        lib.push_str("    ; Input: AX = address low, BH = page, CX = count\n");
        lib.push_str("    push ax\n");
        lib.push_str("    cli\n");
        lib.push_str("    mov al, 0x04  ; Mask channel 1\n");
        lib.push_str("    out 0x0A, al\n");
        lib.push_str("    pop ax\n");
        lib.push_str("    out 0x02, al  ; Address low\n");
        lib.push_str("    mov al, ah\n");
        lib.push_str("    out 0x02, al  ; Address high\n");
        lib.push_str("    mov al, bh\n");
        lib.push_str("    out 0x83, al  ; Page register\n");
        lib.push_str("    mov al, cl\n");
        lib.push_str("    out 0x03, al  ; Count low\n");
        lib.push_str("    mov al, ch\n");
        lib.push_str("    out 0x03, al  ; Count high\n");
        lib.push_str("    mov al, 0x01  ; Unmask channel 1\n");
        lib.push_str("    out 0x0A, al\n");
        lib.push_str("    sti\n");
        lib.push_str("    ret\n\n");
        
        // GPU-specific functions
        lib.push_str("; GPU functions\n");
        lib.push_str("gpu_wait_vsync:\n");
        lib.push_str("    ; Wait for vertical retrace\n");
        lib.push_str("    mov dx, 0x3DA\n");
        lib.push_str(".wait1:\n");
        lib.push_str("    in al, dx\n");
        lib.push_str("    test al, 8\n");
        lib.push_str("    jz .wait1\n");
        lib.push_str(".wait2:\n");
        lib.push_str("    in al, dx\n");
        lib.push_str("    test al, 8\n");
        lib.push_str("    jnz .wait2\n");
        lib.push_str("    ret\n\n");
        
        lib.push_str("gpu_set_mode:\n");
        lib.push_str("    ; Set VGA mode\n");
        lib.push_str("    ; Input: AL = mode number\n");
        lib.push_str("    mov ah, 0x00\n");
        lib.push_str("    int 0x10\n");
        lib.push_str("    ret\n\n");
        
        // Network card functions
        lib.push_str("; Network card functions\n");
        lib.push_str("net_send_packet:\n");
        lib.push_str("    ; Send network packet\n");
        lib.push_str("    ; Input: RSI = packet data, CX = length\n");
        lib.push_str("    push ax\n");
        lib.push_str("    push dx\n");
        lib.push_str("    push si\n");
        lib.push_str("    push cx\n");
        lib.push_str("    \n");
        lib.push_str("    ; Program DMA for packet\n");
        lib.push_str("    mov ax, si\n");
        lib.push_str("    mov bh, 0x00  ; Page 0\n");
        lib.push_str("    call setup_dma\n");
        lib.push_str("    \n");
        lib.push_str("    ; Send packet command\n");
        lib.push_str("    mov dx, 0x300\n");
        lib.push_str("    mov al, 0x22  ; Transmit\n");
        lib.push_str("    out dx, al\n");
        lib.push_str("    \n");
        lib.push_str("    pop cx\n");
        lib.push_str("    pop si\n");
        lib.push_str("    pop dx\n");
        lib.push_str("    pop ax\n");
        lib.push_str("    ret\n\n");
        
        // Utility functions
        lib.push_str("; Utility functions\n");
        lib.push_str("delay_ms:\n");
        lib.push_str("    ; Delay milliseconds\n");
        lib.push_str("    ; Input: CX = milliseconds\n");
        lib.push_str("    push ax\n");
        lib.push_str("    push cx\n");
        lib.push_str("    push dx\n");
        lib.push_str("    \n");
        lib.push_str("    mov dx, 0x3DA  ; VGA status port\n");
        lib.push_str(".delay_loop:\n");
        lib.push_str("    in al, dx\n");
        lib.push_str("    and al, 0x08\n");
        lib.push_str("    cmp al, 0x08\n");
        lib.push_str("    je .delay_loop\n");
        lib.push_str("    loop .delay_loop\n");
        lib.push_str("    \n");
        lib.push_str("    pop dx\n");
        lib.push_str("    pop cx\n");
        lib.push_str("    pop ax\n");
        lib.push_str("    ret\n");
        
        lib
    }
    
    /// Generate example hardware program
    pub fn generate_example(&self) -> String {
        let mut example = String::new();
        
        example.push_str("; Example Hardware DSL Program\n");
        example.push_str("; This shows how to use the hardware direct access DSL\n\n");
        
        example.push_str("@gpu.def render_frame():\n");
        example.push_str("    ; Direct VGA register access\n");
        example.push_str("    write_register(0x3D4, 0x0A)  ; Disable cursor\n");
        example.push_str("    write_register(0x3D5, 0x20)\n");
        example.push_str("    write_register(0x3D4, 0x0B)\n");
        example.push_str("    write_register(0x3D5, 0x00)\n");
        example.push_str("    \n");
        example.push_str("    ; Draw something to screen\n");
        example.push_str("    mov edi, 0xA0000  ; VGA memory\n");
        example.push_str("    mov ecx, 320*200\n");
        example.push_str("    mov al, 0x0F  ; White pixel\n");
        example.push_str("    rep stosb\n");
        example.push_str("    \n");
        example.push_str("    ; Wait for vertical sync\n");
        example.push_str("    call gpu_wait_vsync\n");
        example.push_str("    ret\n\n");
        
        example.push_str("@network_card.def send_packet(data):\n");
        example.push_str("    ; Send network packet using DMA\n");
        example.push_str("    ; data contains packet buffer\n");
        example.push_str("    \n");
        example.push_str("    ; Setup DMA transfer\n");
        example.push_str("    dma_transfer(0x4000, data)  ; Transfer to NIC buffer\n");
        example.push_str("    \n");
        example.push_str("    ; Start transmission\n");
        example.push_str("    write_register(0x300, 0x22)  ; Transmit command\n");
        example.push_str("    \n");
        example.push_str("    ; Wait for completion\n");
        example.push_str("    mov cx, 10\n");
        example.push_str("    call delay_ms\n");
        example.push_str("    ret\n\n");
        
        example.push_str("@sound.def play_tone(frequency, duration):\n");
        example.push_str("    ; Play a tone on sound card\n");
        example.push_str("    write_register(0x224, 0x01)  ; Mixer: FM music\n");
        example.push_str("    write_register(0x225, 0x0F)\n");
        example.push_str("    \n");
        example.push_str("    ; Calculate timer value\n");
        example.push_str("    mov ax, 1193180  ; PIT frequency\n");
        example.push_str("    xor dx, dx\n");
        example.push_str("    div frequency\n");
        example.push_str("    \n");
        example.push_str("    ; Program timer\n");
        example.push_str("    out 0x43, 0xB6  ; Channel 2, square wave\n");
        example.push_str("    out 0x42, al    ; Low byte\n");
        example.push_str("    mov al, ah\n");
        example.push_str("    out 0x42, al    ; High byte\n");
        example.push_str("    \n");
        example.push_str("    ; Turn on speaker\n");
        example.push_str("    in al, 0x61\n");
        example.push_str("    or al, 3\n");
        example.push_str("    out 0x61, al\n");
        example.push_str("    \n");
        example.push_str("    ; Wait for duration\n");
        example.push_str("    mov cx, duration\n");
        example.push_str("    call delay_ms\n");
        example.push_str("    \n");
        example.push_str("    ; Turn off speaker\n");
        example.push_str("    in al, 0x61\n");
        example.push_str("    and al, 0xFC\n");
        example.push_str("    out 0x61, al\n");
        example.push_str("    ret\n");
        
        example
    }
}

impl HardwareDevice {
    pub fn new(name: &str, device_type: DeviceType) -> Self {
        Self {
            name: name.to_string(),
            device_type,
            registers: HashMap::new(),
            interrupts: Vec::new(),
            dma_channels: Vec::new(),
            special_instructions: Vec::new(),
        }
    }
    
    pub fn add_register(&mut self, name: &str, address: u64) -> &mut Self {
        self.registers.insert(name.to_string(), address);
        self
    }
    
    pub fn add_interrupt(&mut self, irq: u8, handler: &str) -> &mut Self {
        self.interrupts.push(InterruptHandler {
            irq,
            handler_address: 0,
            assembly_stub: handler.to_string(),
        });
        self
    }
    
    pub fn add_dma_channel(&mut self, channel: u8, base: u64, page: u64) -> &mut Self {
        self.dma_channels.push(DmaChannel {
            channel,
            base_address: base,
            page_address: page,
            count_register: 0x0B,
            mode_register: 0x0C,
        });
        self
    }
}

// ========== PARSER EXTENSIONS ==========

// Extend the parser to support hardware DSL syntax
pub fn parse_hardware_dsl(source: &str) -> Result<Vec<String>, String> {
    let mut dsl = HardwareDSL::new();
    let mut assembly = Vec::new();
    
    for line in source.lines() {
        let trimmed = line.trim();
        
        if trimmed.is_empty() || trimmed.starts_with(';') {
            continue;
        }
        
        // Try to parse as hardware DSL
        match dsl.parse_hardware_statement(trimmed) {
            Ok(mut asm_lines) => {
                assembly.append(&mut asm_lines);
            }
            Err(_) => {
                // Not hardware DSL, pass through as regular assembly
                if !trimmed.starts_with('@') {
                    assembly.push(trimmed.to_string());
                }
            }
        }
    }
    
    Ok(assembly)
}

// Extend the backend to support hardware intrinsics
pub fn extend_backend_with_hardware(backend_asm: &str, hardware_dsl: &HardwareDSL) -> String {
    let mut final_asm = backend_asm.to_string();
    
    // Add hardware library
    final_asm.push_str("\n\n; ========== HARDWARE SUPPORT ==========\n");
    final_asm.push_str(&hardware_dsl.generate_hardware_library());
    
    // Add cached assembly
    if !hardware_dsl.assembly_cache.is_empty() {
        final_asm.push_str("\n\n; ========== HARDWARE FUNCTIONS ==========\n");
        for asm in &hardware_dsl.assembly_cache {
            final_asm.push_str(asm);
            final_asm.push('\n');
        }
    }
    
    final_asm
}

// ========== EXAMPLE USAGE ==========

pub fn create_hardware_example() -> String {
    let dsl = HardwareDSL::new();
    dsl.generate_example()
}

// ========== INTEGRATION WITH EXISTING BACKEND ==========

pub fn integrate_hardware_with_backend(backend: &dyn crate::backend::Backend, dsl: &HardwareDSL) {
    // Hardware DSL integration with backend
    println!("Hardware DSL integration: {}", backend.name());
    println!("Devices registered: {}", dsl.device_registry.len());
}

// ========== MAIN API ==========

pub fn init_hardware_dsl() -> HardwareDSL {
    HardwareDSL::new()
}

pub fn compile_with_hardware(source: &str, target: crate::backend::Target) -> Result<String, String> {
    // Parse regular Earthang code
    let program = crate::parser::parse_program(source)
        .map_err(|e| format!("Parse error: {:?}", e))?;
    
    // Create hardware DSL
    let dsl = HardwareDSL::new();
    
    // Create appropriate backend
    let mut backend: Box<dyn crate::backend::Backend> = match target {
        crate::backend::Target::Bios16 => Box::new(crate::backend::Bios16Backend::new()),
        crate::backend::Target::Bios32 => Box::new(crate::backend::Bios32Backend::new()),
        crate::backend::Target::Bios64 => Box::new(crate::backend::Bios64Backend::new()),
        _ => return Err("Hardware DSL only supports BIOS targets".to_string()),
    };
    
    // Compile program
    let asm = backend.compile_program(&program)?;
    
    // Extend with hardware support
    let final_asm = extend_backend_with_hardware(&asm, &dsl);
    
    Ok(final_asm)
}

// ========== TEST FUNCTIONS ==========

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_hardware_dsl_parsing() {
        let mut dsl = HardwareDSL::new();
        
        // Test write_register
        let asm = dsl.parse_hardware_statement("write_register(0x3D4, 0x0A)").unwrap();
        assert!(asm[0].contains("write_register"));
        assert!(asm.join("").contains("out dx, al"));
        
        // Test dma_transfer
        let asm = dsl.parse_hardware_statement("dma_transfer(0x4000, buffer_addr)").unwrap();
        assert!(asm[0].contains("dma_transfer"));
        assert!(asm.join("").contains("DMA"));
        
        // Test device decorator
        let asm = dsl.parse_hardware_statement("@gpu.def render_frame():").unwrap();
        assert!(asm[0].contains("Device function"));
        
        println!("Hardware DSL tests passed!");
    }
    
    #[test]
    fn test_hardware_library_generation() {
        let dsl = HardwareDSL::new();
        let lib = dsl.generate_hardware_library();
        
        assert!(lib.contains("port_in_8"));
        assert!(lib.contains("port_out_8"));
        assert!(lib.contains("setup_dma"));
        assert!(lib.contains("gpu_wait_vsync"));
        assert!(lib.contains("net_send_packet"));
        
        println!("Hardware library generation test passed!");
    }
}

// Make the DSL available as a module
pub mod hardware {
    pub use super::{
        HardwareDSL, HardwareDevice, DeviceType, DmaChannel, InterruptHandler,
        init_hardware_dsl, compile_with_hardware, create_hardware_example,
        parse_hardware_dsl, extend_backend_with_hardware
    };
}

// Default implementation
impl Default for HardwareDSL {
    fn default() -> Self {
        Self::new()
    }
}