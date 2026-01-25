```mermaid
flowchart TD
    Start[Earthang Source Code] --> Parser
    
    subgraph ParsingPhase
        Parser --> AST[Abstract Syntax Tree]
        AST --> TypeChecker[Type Checking]
        TypeChecker --> SemanticAnalysis[Semantic Analysis]
    end
    
    subgraph BackendSelection
        SemanticAnalysis --> AnalyzeCaps[Analyze Capabilities]
        AnalyzeCaps --> SelectBackend{Select Backend}
        
        SelectBackend --> |BIOS16| BIOS16Backend
        SelectBackend --> |BIOS32| BIOS32Backend
        SelectBackend --> |BIOS64| Bios64Backend
        SelectBackend --> |Linux64| Linux64Backend
        
        subgraph BIOS64Backend
            direction LR
            B641[Memory Manager]
            B642[String Literals]
            B643[Stack Management]
            B644[Hardware DSL]
        end
        
        subgraph Linux64Backend
            direction LR
            L641[System V ABI]
            L642[Syscall Interface]
            L643[Stack Alignment]
        end
    end
    
    subgraph CodeGeneration
        BIOS16Backend --> Gen16[Generate 16-bit ASM]
        BIOS32Backend --> Gen32[Generate 32-bit ASM]
        Bios64Backend --> Gen64[Generate 64-bit ASM]
        Linux64Backend --> GenLinux[Generate Linux ASM]
        
        Gen16 --> Assemble16[Assemble to Binary]
        Gen32 --> Assemble32[Assemble to Binary]
        Gen64 --> Assemble64[Assemble to Binary]
        GenLinux --> AssembleLinux[Assemble to ELF]
    end
    
    subgraph BootProcess[For BIOS Targets]
        Assemble16 --> Boot16[16-bit Bootloader]
        Assemble32 --> Boot32[32-bit Bootloader]
        Assemble64 --> Boot64[64-bit Bootloader]
        
        Boot16 --> Loader16[BIOS Loads at 0x7C00]
        Boot32 --> Transition32[Real→Protected Mode]
        Boot64 --> Transition64[Real→Protected→Long Mode]
        
        Transition64 --> Final64[64-bit Kernel]
    end
    
    subgraph HardwareIntegration
        HardwareDSL[Hardware DSL] --> DeviceSpecific[Device-Specific Code]
        DeviceSpecific --> MergeWithBackend[Merge with Generated Code]
    end
    
    subgraph CachingSystem
        OriginalSource --> Hash[Calculate Hash]
        Hash --> CheckCache{Check Cache}
        CheckCache --> |Hit| LoadCache[Load Cached Bytecode]
        CheckCache --> |Miss| GenerateNew[Generate New]
        GenerateNew --> StoreCache[Store in Cache]
    end
    
    LoadCache --> Assemble16 & Assemble32 & Assemble64 & AssembleLinux
    StoreCache --> Assemble16 & Assemble32 & Assemble64 & AssembleLinux
    
    MergeWithBackend --> FinalOutput[Final Binary]
    
    style Start fill:#bbdefb
    style FinalOutput fill:#c8e6c9
    style HardwareIntegration fill:#ffecb3
    style CachingSystem fill:#dcedc8