# Earthang

**Earthang** is an experimental programming language and compiler designed to generate **small, native, low-level binaries** using a simple Python-like syntax.

The project is inspired by the original ideas behind *Rython*, but implements its **own lexer, parser, AST, and x86_64 assembly backend**, without relying on the C standard library or external runtimes.

Earthang currently targets **Linux ELF64**, with future goals including **UEFI and BIOS** code generation.

---

## Design Goals

- Minimal runtime (no C stdlib, no libc startup)
- Small binary size
- Direct Linux syscalls
- Predictable control flow
- Python-like syntax for low-level code
- Suitable for bare-metal and firmware-oriented environments

Earthang is **not** intended to replace C or C++.  
It exists to explore **low-level programming with less ceremony and less binary bloat**.

---

## Example

### Earthang (Fibonacci)

```lua
var n: int = 100
var a: long = 0
var b: long = 1

while a < n:
    print(a)

    var temp: long = a
    a = b
    b = temp + b
end

```