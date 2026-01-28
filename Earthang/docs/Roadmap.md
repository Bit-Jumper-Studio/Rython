```mermaid
flowchart TD
    A[Earthang Language System] --> B[Frontend]
    A --> C[Backend Architecture]
    A --> D[Hardware DSL]
    A --> E[Booting System]
    A --> F[Cache System]
    
    subgraph B[Frontend]
        B1[Parser]
        B2[AST<br/>Program/Statement/Expr]
        B3[Type System]
        B1 --> B2 --> B3
    end
    
    subgraph C[Backend Architecture]
        C1[Target Platforms]
        C2[Backend Registry]
        C3[Capability System]
        
        subgraph C1[Target Platforms]
            C1A[BIOS16<br/>Real Mode]
            C1B[BIOS32<br/>Protected Mode]
            C1C[BIOS64<br/>Long Mode]
            C1D[Linux64<br/>Userspace]
        end
        
        subgraph C3[Capability System]
            C3A[RealMode16<br/>ProtectedMode32<br/>LongMode64]
            C3B[SSE/AVX/AVX512<br/>Extensions]
            C3C[Paging<br/>VirtualMemory]
            C3D[BIOS/Linux<br/>Environment]
            C3E[NoHeap/NoFilesystem<br/>Constraints]
        end
        
        C2 --> C1
        C2 --> C3
    end
    
    subgraph D[Hardware DSL]
        D1[Device Registry]
        D2[Hardware Intrinsics]
        D3[Direct Hardware Access]
        
        subgraph D1[Device Registry]
            D1A[GPU<br/>VGA/Graphics]
            D1B[Network Card]
            D1C[Storage Controller]
            D1D[Sound Card]
            D1E[Custom Devices]
        end
        
        D2 --> D1
        D2 --> D3[write_register/read_register<br/>dma_transfer/port_in/port_out]
    end
    
    subgraph E[Booting System]
        E1[Compact Bootloader<br/>512-byte]
        E2[Micro Bootloader<br/>256-byte]
        E3[Mode Transition<br/>Real16→Long64]
        E4[Graphics Kernel<br/>VGA Output]
    end
    
    subgraph F[Cache System]
        F1[Disk Cache]
        F2[Script Caching]
        F3[Bytecode Storage]
        F4[Function Cache]
    end
    
    B --> C2
    D --> C
    E --> C1A & C1B & C1C
    
    style A fill:#e1f5fe
    style B fill:#f3e5f5
    style C fill:#e8f5e8
    style D fill:#fff3e0
    style E fill:#fce4ec
    style F fill:#f1f8e9