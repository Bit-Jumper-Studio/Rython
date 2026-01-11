; Rython Linux 64-bit Backend
; Generated from Rython AST

    bits 64
    default rel

    section .text
    global _start

_start:
    ; Set up stack
    mov rbp, rsp
    and rsp, -16         ; Align stack to 16 bytes
    call main
    ; Exit after main returns
    mov rax, 60         ; sys_exit
    xor rdi, rdi        ; exit code 0
    syscall

main:
    push rbp
    mov rbp, rsp
    and rsp, -16        ; Align stack to 16 bytes
    ; String: 'hi'
    lea rdi, [str_0]
    call print_string
    call print_newline
    ; Return from main
    mov rax, 0          ; Return 0
    mov rsp, rbp
    pop rbp
    ret

; Simple print string function
print_string:
    ; Input: rdi = string address
    push rax
    push rdi
    push rsi
    push rdx
    
    ; Calculate length
    mov rsi, rdi          ; String address
    xor rdx, rdx          ; Length counter
.count_loop:
    cmp byte [rsi + rdx], 0
    je .count_done
    inc rdx
    jmp .count_loop
.count_done:
    
    ; Write to stdout
    mov rax, 1           ; sys_write
    mov rdi, 1           ; stdout
    ; rsi already has string address
    ; rdx already has length
    syscall
    
    ; Restore registers
    pop rdx
    pop rsi
    pop rdi
    pop rax
    ret

; Print decimal number
print_decimal:
    ; Input: rax = integer
    push rbp
    mov rbp, rsp
    sub rsp, 32          ; Buffer space
    
    ; Save registers
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    
    ; Save the number
    mov [rbp - 8], rax   ; Save at [rbp-8]
    
    ; Point to buffer end
    lea rdi, [rsp + 31]  ; Last byte of buffer
    mov byte [rdi], 0    ; Null terminator
    
    ; Handle negative numbers
    mov rax, [rbp - 8]
    test rax, rax
    jns .positive
    neg rax
    
.positive:
    mov rbx, 10
    
.convert_loop:
    xor rdx, rdx
    div rbx              ; rax = quotient, rdx = remainder
    add dl, '0'
    dec rdi
    mov [rdi], dl
    test rax, rax
    jnz .convert_loop
    
    ; Add minus sign if needed
    mov rax, [rbp - 8]
    test rax, rax
    jns .print_it
    dec rdi
    mov byte [rdi], '-'
    
.print_it:
    ; Calculate length
    lea rsi, [rsp + 32]  ; End of buffer + 1
    sub rsi, rdi         ; rsi = length
    
    ; Print the number
    mov rax, 1           ; sys_write
    mov rdx, rsi         ; length
    mov rsi, rdi         ; string
    mov rdi, 1           ; stdout
    syscall
    
    ; Print newline
    mov rax, 1
    mov rdi, 1
    lea rsi, [newline]
    mov rdx, 1
    syscall
    
    ; Restore registers
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    
    mov rsp, rbp
    pop rbp
    ret

; Print newline
print_newline:
    push rax
    push rdi
    push rsi
    push rdx
    
    mov rax, 1
    mov rdi, 1
    lea rsi, [newline]
    mov rdx, 1
    syscall
    
    pop rdx
    pop rsi
    pop rdi
    pop rax
    ret
    section .data
newline:
    db 10, 0

; String literals
str_0:
    db 'hi', 0
