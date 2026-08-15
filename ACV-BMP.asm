; ============================================================================
; ACV-BMP.ASM - BMP Image Viewer for ACV-1030 / V6355D
; Target: 12 MHz 286 with ACV-1030 ISA video card, DOS COM
; Port of PC1-BMP.ASM; hidden 160x200x16 graphics mode
;
; ACV-1030 port assumptions:
; - ACV uses the full V6355D ports 3D8/3D9/3DD/3DE; all OUTs use DX.
; - BIOS mode 4 is selected first to establish the CGA CRTC timing baseline.
; - Register 67h is 18h (8-bit bus, no 64 KB page mode).
; - Register 65h is 01h (200 lines, NTSC, color, CRT).
; - 80h is written to 3DDh before 4Ah is sent to 3D8h.
; - The ACV color/graphics window is B800h. CGA interlace remains: even
;   rows start at offset 0000h and odd rows at offset 2000h.
; - Composite palette programming opens with 40h, writes 32 bytes to 3DEh
;   with delays, and closes with 80h. CGA/TTL output remains fixed IRGB.
; - Normal composite mode loads the BMP's 16 palette entries directly; this
;   viewer does not perform global best-16 palette selection.
; - Optional /CGA mode skips programmable palette loading and remaps each BMP
;   palette entry to the nearest fixed CGA/TTL RGB color at runtime. The BMP
;   file is never modified.
; - Only 8086/286 instructions are used; no V40/80186-only instructions.
; ============================================================================

[BITS 16]
[CPU 286]
[ORG 0x100]

VIDEO_SEG       equ 0xB800
PORT_REG_ADDR   equ 0x3DD
PORT_REG_DATA   equ 0x3DE
PORT_MODE       equ 0x3D8
PORT_COLOR      equ 0x3D9
SCREEN_WIDTH    equ 160
SCREEN_HEIGHT   equ 200

BMP_SIGNATURE   equ 0
BMP_DATA_OFFSET equ 10
BMP_WIDTH       equ 18
BMP_HEIGHT      equ 22
BMP_BPP         equ 28
BMP_COMPRESSION equ 30

main:
    push cs
    pop ds
    cld
    xor bx, bx
    cmp byte [bx + 0x80], 0
    jne .has_arguments
    jmp .show_usage
.has_arguments:
    mov si, 0x81
.skip_spaces:
    lodsb
    cmp al, ' '
    je .skip_spaces
    cmp al, 0x0D
    jne .not_empty_tail
    jmp .show_usage
.not_empty_tail:
    cmp al, '/'
    jne .not_help
    lodsb
    cmp al, '?'
    jne .not_question_help
    jmp .show_usage
.not_question_help:
    cmp al, 'h'
    jne .not_lower_help
    jmp .show_usage
.not_lower_help:
    cmp al, 'H'
    jne .not_upper_help
    jmp .show_usage
.not_upper_help:
    cmp al, 'c'
    je .cga_option
    cmp al, 'C'
    jne .not_cga_option
.cga_option:
    lodsb
    cmp al, 'g'
    je .cga_got_g
    cmp al, 'G'
    jne .not_cga_option
.cga_got_g:
    lodsb
    cmp al, 'a'
    je .cga_got_a
    cmp al, 'A'
    jne .not_cga_option
.cga_got_a:
    mov byte [cga_mode], 1
    jmp .skip_option_spaces
.not_cga_option:
    dec si
    dec si
    jmp .save_filename
.not_help:
    dec si
.save_filename:
    mov [filename_ptr], si
.skip_option_spaces:
.skip_option_space_loop:
    lodsb
    cmp al, ' '
    je .skip_option_space_loop
    cmp al, 0x0D
    je .show_usage
    dec si
    mov [filename_ptr], si
.find_end:
    lodsb
    cmp al, ' '
    je .found_end
    cmp al, 0x0D
    jne .find_end
.found_end:
    dec si
    mov byte [si], 0

    mov dx, [filename_ptr]
    mov ax, 0x3D00
    int 0x21
    jnc .file_opened
    jmp .file_error
.file_opened:
    mov [file_handle], ax
    mov bx, ax
    mov dx, bmp_header
    mov cx, 118
    mov ah, 0x3F
    int 0x21
    jnc .header_read
    jmp .file_error
.header_read:
    cmp ax, 118
    jb .file_error
    cmp word [bmp_header + BMP_SIGNATURE], 0x4D42
    jne .not_bmp
    cmp word [bmp_header + BMP_BPP], 4
    jne .wrong_format
    cmp word [bmp_header + BMP_COMPRESSION], 0
    jne .wrong_format
    cmp word [bmp_header + BMP_COMPRESSION + 2], 0
    jne .wrong_format

    cmp byte [cga_mode], 0
    je .palette_ready
    call set_cga_remap
.palette_ready:

    mov bx, [file_handle]
    mov dx, [bmp_header + BMP_DATA_OFFSET]
    mov cx, [bmp_header + BMP_DATA_OFFSET + 2]
    mov ax, 0x4200
    int 0x21
    jc .file_error

    call enable_graphics_mode
    mov al, 0x42
    call out_mode
    call clear_screen
    cmp byte [cga_mode], 0
    jne .skip_bmp_palette
    call set_bmp_palette
.skip_bmp_palette:
    call decode_bmp
    xor al, al
    call out_color
    mov al, 0x4A
    call out_mode

    mov bx, [file_handle]
    mov ah, 0x3E
    int 0x21
    xor ah, ah
    int 0x16
    call set_cga_palette
    call disable_graphics_mode
    mov ax, 0x0003
    int 0x10
    mov ax, 0x4C00
    int 0x21

.show_usage:
    mov dx, msg_info
    mov ah, 0x09
    int 0x21
    mov ax, 0x4C00
    int 0x21
.file_error:
    mov dx, msg_file_err
    jmp .print_exit
.not_bmp:
    mov dx, msg_not_bmp
    jmp .print_exit
.wrong_format:
    mov dx, msg_format
.print_exit:
    mov ah, 0x09
    int 0x21
    mov ax, 0x4C01
    int 0x21

; --- Full-port ACV output helpers. The caller's DX is preserved. ---
out_mode:
    push dx
    mov dx, PORT_MODE
    out dx, al
    pop dx
    ret
out_color:
    push dx
    mov dx, PORT_COLOR
    out dx, al
    pop dx
    ret
out_reg_addr:
    push dx
    mov dx, PORT_REG_ADDR
    out dx, al
    pop dx
    ret
out_reg_data:
    push dx
    mov dx, PORT_REG_DATA
    out dx, al
    pop dx
    ret

; --- BIOS mode 4 first, then ACV-1030 hidden-mode setup. ---
enable_graphics_mode:
    push ax
    push dx
    mov ax, 0x0004
    int 0x10

    mov al, 0x67
    call out_reg_addr
    jmp short $+2
    mov al, 0x18
    call out_reg_data
    jmp short $+2

    mov al, 0x65
    call out_reg_addr
    jmp short $+2
    mov al, 0x01
    call out_reg_data
    jmp short $+2

    mov al, 0x80
    call out_reg_addr
    jmp short $+2
    mov al, 0x4A
    call out_mode
    jmp short $+2
    jmp short $+2
    xor al, al
    call out_color
    jmp short $+2
    jmp short $+2
    pop dx
    pop ax
    ret

disable_graphics_mode:
    push ax
    push dx
    mov al, 0x80
    call out_reg_addr
    jmp short $+2
    mov al, 0x28
    call out_mode
    jmp short $+2
    mov al, 0x65
    call out_reg_addr
    jmp short $+2
    mov al, 0x01
    call out_reg_data
    jmp short $+2
    pop dx
    pop ax
    ret

; --- BMP BGRA palette to V6355D's 32-byte composite palette format. ---
; The ACV composite DAC uses these values; CGA/TTL remains fixed IRGB.
set_bmp_palette:
    push ax
    push bx
    push cx
    push dx
    push si
    cli
    mov al, 0x40
    call out_reg_addr
    jmp short $+2
    mov si, bmp_header + 54
    mov cx, 16
.palette_loop:
    lodsb
    mov bl, al
    lodsb
    mov bh, al
    lodsb
    shr al, 5
    call out_reg_data
    jmp short $+2
    mov al, bh
    and al, 0xE0
    shr al, 1
    mov ah, al
    mov al, bl
    shr al, 5
    or al, ah
    call out_reg_data
    jmp short $+2
    lodsb
    loop .palette_loop
    mov al, 0x80
    call out_reg_addr
    sti
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; --- Build BMP-index -> fixed CGA/TTL index map for /CGA mode. ---
; Uses RGB888 Manhattan distance against the 16 fixed CGA colors.
set_cga_remap:
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    mov si, bmp_header + 54
    xor di, di
    mov cx, 16
.load_source:
    lodsb
    mov [cga_source_b + di], al
    lodsb
    mov [cga_source_g + di], al
    lodsb
    mov [cga_source_r + di], al
    lodsb
    inc di
    loop .load_source

    xor di, di
.source_loop:
    mov byte [cga_best_index], 0
    mov word [cga_best_distance], 0xFFFF
    xor bx, bx
.candidate_loop:
    call cga_distance
    cmp ax, [cga_best_distance]
    jae .candidate_next
    mov [cga_best_distance], ax
    mov [cga_best_index], bl
.candidate_next:
    inc bx
    cmp bx, 16
    jb .candidate_loop
    mov al, [cga_best_index]
    mov [cga_remap + di], al
    inc di
    cmp di, 16
    jb .source_loop

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; Input BX = fixed CGA color index; output AX = RGB888 Manhattan distance.
cga_distance:
    xor dx, dx
    mov al, [cga_source_r + di]
    sub al, [cga_fixed_r + bx]
    jnc .red_done
    neg al
.red_done:
    xor ah, ah
    add dx, ax
    mov al, [cga_source_g + di]
    sub al, [cga_fixed_g + bx]
    jnc .green_done
    neg al
.green_done:
    xor ah, ah
    add dx, ax
    mov al, [cga_source_b + di]
    sub al, [cga_fixed_b + bx]
    jnc .blue_done
    neg al
.blue_done:
    xor ah, ah
    add dx, ax
    mov ax, dx
    ret

map_cga_index:
    xor bh, bh
    mov bx, cga_remap
    xlat
    ret

map_cga_packed_byte:
    push dx
    mov ah, al
    shr al, 4
    call map_cga_index
    shl al, 4
    mov dl, al
    mov al, ah
    and al, 0x0F
    call map_cga_index
    or al, dl
    pop dx
    ret

set_cga_palette:
    push ax
    push cx
    push dx
    push si
    cli
    mov al, 0x40
    call out_reg_addr
    jmp short $+2
    mov si, cga_colors
    mov cx, 32
.palette_loop:
    lodsb
    call out_reg_data
    jmp short $+2
    loop .palette_loop
    mov al, 0x80
    call out_reg_addr
    sti
    pop si
    pop dx
    pop cx
    pop ax
    ret

clear_screen:
    push ax
    push cx
    push di
    push es
    mov ax, VIDEO_SEG
    mov es, ax
    xor di, di
    mov cx, 8192
    xor ax, ax
    cld
    rep stosw
    pop es
    pop di
    pop cx
    pop ax
    ret

; --- Decode bottom-up 4-bit BMP rows into the unchanged packed format. ---
decode_bmp:
    push ax
    push bx
    push cx
    push dx
    push di
    push si
    push es
    mov ax, VIDEO_SEG
    mov es, ax
    mov ax, [bmp_header + BMP_WIDTH]
    mov [image_width], ax
    cmp ax, 160
    jbe .width_ok
    cmp ax, 320
    ja .width_too_large
    mov byte [downsample_flag], 1
    jmp .width_done
.width_too_large:
    mov ax, 320
    mov [image_width], ax
    mov byte [downsample_flag], 1
    jmp .width_done
.width_ok:
    mov byte [downsample_flag], 0
.width_done:
    mov ax, [bmp_header + BMP_HEIGHT]
    cmp word [bmp_header + BMP_HEIGHT + 2], 0
    je .height_positive
    neg ax
.height_positive:
    cmp ax, 200
    jbe .height_ok
    mov ax, 200
.height_ok:
    mov [image_height], ax
    mov ax, [image_width]
    inc ax
    shr ax, 1
    add ax, 3
    and ax, 0xFFFC
    cmp ax, 164
    jbe .bpr_ok
    mov ax, 164
.bpr_ok:
    mov [bytes_per_row], ax
    mov ax, [image_height]
    dec ax
    mov [current_row], ax
.row_loop:
    mov ax, [current_row]
    push ax
    shr ax, 1
    mov bx, 80
    mul bx
    mov di, ax
    pop ax
    test al, 1
    jz .even_row
    add di, 0x2000
.even_row:
    mov bx, [file_handle]
    mov dx, row_buffer
    mov cx, [bytes_per_row]
    mov ah, 0x3F
    int 0x21
    jc .decode_done
    or ax, ax
    jz .decode_done
    mov al, [border_ctr]
    call out_color
    inc byte [border_ctr]
    and byte [border_ctr], 0x0F
    cmp byte [downsample_flag], 0
    je .copy_row
    mov si, row_buffer
    mov cx, 80
.downsample_loop:
    lodsb
    push ax
    and al, 0xF0
    mov ah, al
    cmp byte [cga_mode], 0
    je .downsample_first_source
    shr al, 4
    call map_cga_index
    shl al, 4
    mov ah, al
.downsample_first_source:
    lodsb
    shr al, 4
    cmp byte [cga_mode], 0
    je .downsample_source_pixel
    call map_cga_index
.downsample_source_pixel:
    or al, ah
    mov [es:di], al
    inc di
    pop ax
    push ax
    mov ax, cx
    and ax, 7
    jnz .no_border_ds
    mov al, [border_ctr]
    call out_color
    inc byte [border_ctr]
    and byte [border_ctr], 0x0F
.no_border_ds:
    pop ax
    loop .downsample_loop
    jmp .row_done
.copy_row:
    mov si, row_buffer
    mov cx, 80
.copy_loop:
    lodsb
    cmp byte [cga_mode], 0
    je .copy_source_byte
    call map_cga_packed_byte
.copy_source_byte:
    stosb
    push ax
    mov ax, cx
    and ax, 7
    jnz .no_border_copy
    mov al, [border_ctr]
    call out_color
    inc byte [border_ctr]
    and byte [border_ctr], 0x0F
.no_border_copy:
    pop ax
    loop .copy_loop
.row_done:
    mov ax, [current_row]
    or ax, ax
    jz .decode_done
    dec ax
    mov [current_row], ax
    jmp .row_loop
.decode_done:
    pop es
    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret

msg_info db 'ACV-BMP - ACV-1030 160x200x16 BMP viewer',0x0D,0x0A
         db 'Supports 4-bit uncompressed 160/320-wide BMP files.',0x0D,0x0A
         db 'Usage: ACV-BMP [/CGA] filename.bmp',0x0D,0x0A
         db '/CGA remaps pixels to the nearest fixed CGA/TTL color.',0x0D,0x0A,'$'
msg_file_err db 'Error: Cannot open or read file',0x0D,0x0A,'$'
msg_not_bmp db 'Error: Not a valid BMP file',0x0D,0x0A,'$'
msg_format db 'Error: BMP must be 4-bit uncompressed',0x0D,0x0A,'$'

filename_ptr dw 0
file_handle dw 0
image_width dw 0
image_height dw 0
bytes_per_row dw 0
current_row dw 0
downsample_flag db 0
border_ctr db 0
cga_mode db 0
cga_best_index db 0
cga_best_distance dw 0
cga_remap times 16 db 0
cga_source_r times 16 db 0
cga_source_g times 16 db 0
cga_source_b times 16 db 0

cga_fixed_r:
    ; Fixed CGA/TTL RGB888 palette, indexes 0-15.
    db 0,0,0,0,170,170,170,170,85,85,85,85,255,255,255,255
cga_fixed_g:
    db 0,0,170,170,0,0,85,170,85,85,255,255,85,85,255,255
cga_fixed_b:
    db 0,170,0,170,0,170,0,170,85,255,85,255,85,255,85,255

cga_colors:
    db 0x00,0x00, 0x00,0x05, 0x00,0x50, 0x00,0x55
    db 0x05,0x00, 0x05,0x05, 0x05,0x20, 0x05,0x55
    db 0x02,0x22, 0x02,0x27, 0x02,0x72, 0x02,0x77
    db 0x07,0x22, 0x07,0x27, 0x07,0x70, 0x07,0x77

bmp_header: times 128 db 0
row_buffer: times 164 db 0
