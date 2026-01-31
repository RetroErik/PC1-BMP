; ============================================================================
; LOADBMP.ASM (v 1.0) - BMP Image Viewer for Olivetti Prodest PC1
; Hidden 160x200x16 Graphics Mode
; Written for NASM - NEC V40 (80186 compatible)
; By Retro Erik - 2026 using VS Code with Co-Pilot
;
; Supports: 4-bit (16-color) BMP files, uncompressed (BI_RGB)
; Usage: LOADBMP filename.bmp
;        Press any key to exit
; ============================================================================

[BITS 16]
[ORG 0x100]

; ============================================================================
; Constants
; ============================================================================
VIDEO_SEG       equ 0xB000      ; PC1 video RAM segment

; 6355 I/O Ports
PORT_REG_ADDR   equ 0xDD        ; 6355 Register Bank Address Port
PORT_REG_DATA   equ 0xDE        ; 6355 Register Bank Data Port  
PORT_MODE       equ 0xD8        ; CGA Mode Control Port
PORT_COLOR      equ 0xD9        ; CGA Color Select Port

; BMP File Header offsets (14 bytes)
BMP_SIGNATURE   equ 0           ; 'BM' signature (2 bytes)
BMP_FILESIZE    equ 2           ; File size (dword)
BMP_RESERVED    equ 6           ; Reserved (dword)
BMP_DATA_OFFSET equ 10          ; Offset to pixel data (dword)

; BMP Info Header offsets (40 bytes for BITMAPINFOHEADER)
BMP_HEADER_SIZE equ 14          ; Offset to info header size field (dword, value=40)
BMP_WIDTH       equ 18          ; Image width (dword)
BMP_HEIGHT      equ 22          ; Image height (dword)
BMP_PLANES      equ 26          ; Planes (word, should be 1)
BMP_BPP         equ 28          ; Bits per pixel (word)
BMP_COMPRESSION equ 30          ; Compression (dword, 0=none)
BMP_IMAGE_SIZE  equ 34          ; Image size (dword)
BMP_PALETTE_OFFSET equ 54       ; Palette starts here (for 4-bit BMP)

SCREEN_WIDTH    equ 160
SCREEN_HEIGHT   equ 200

; ============================================================================
; Main Program Entry Point
; ============================================================================
main:
    ; Parse command line for filename (simple version)
    mov si, 0x81            ; Command line starts at PSP:0081
    
    ; Skip leading spaces
.skip_spaces:
    lodsb
    cmp al, ' '
    je .skip_spaces
    cmp al, 0x0D            ; End of command line?
    je .show_usage
    
    ; Check for /? or /h or /H
    cmp al, '/'
    jne .not_help
    lodsb                   ; Get next character
    cmp al, '?'
    je .show_usage
    cmp al, 'h'
    je .show_usage
    cmp al, 'H'
    je .show_usage
    ; Not a help flag, treat as filename starting with /
    dec si
    dec si
    jmp .save_filename
    
.not_help:
    ; SI now points one past first char, back up
    dec si
    
.save_filename:
    mov [filename_ptr], si
    
    ; Find end of filename (space or CR)
.find_end:
    lodsb
    cmp al, ' '
    je .found_end
    cmp al, 0x0D
    jne .find_end
    
.found_end:
    dec si
    mov byte [si], 0        ; Null-terminate filename
    jmp .open_file

.show_usage:
    mov dx, msg_info
    mov ah, 0x09
    int 0x21
    mov ax, 0x4C00
    int 0x21

.open_file:
    ; Open the BMP file
    mov dx, [filename_ptr]
    mov ax, 0x3D00          ; DOS Open File (read-only)
    int 0x21
    jc .file_error
    mov [file_handle], ax
    
    ; Read entire BMP header + palette in one go
    ; 14 (file header) + 40 (info header) + 64 (palette) = 118 bytes
    mov bx, ax
    mov dx, bmp_header
    mov cx, 118
    mov ah, 0x3F            ; DOS Read File
    int 0x21
    jc .file_error
    cmp ax, 118             ; Check we got enough bytes
    jb .file_error
    
    ; Verify BMP signature ('BM')
    cmp word [bmp_header + BMP_SIGNATURE], 0x4D42
    jne .not_bmp
    
    ; Check bits per pixel (should be 4)
    cmp word [bmp_header + BMP_BPP], 4
    jne .wrong_format
    
    ; Check compression (should be 0 = uncompressed)
    cmp word [bmp_header + BMP_COMPRESSION], 0
    jne .wrong_format
    cmp word [bmp_header + BMP_COMPRESSION + 2], 0
    jne .wrong_format
    
    ; Seek to pixel data (data offset is at offset 10, dword value)
    mov bx, [file_handle]
    mov dx, [bmp_header + BMP_DATA_OFFSET]      ; Low word of offset
    mov cx, [bmp_header + BMP_DATA_OFFSET + 2]  ; High word of offset
    mov ax, 0x4200          ; Seek from beginning (AL=0, AH=0x42)
    int 0x21
    jc .file_error
    
    ; Enable graphics mode (starts with video blanked)
    call enable_graphics_mode
    
    ; Blank video output during load
    mov al, 0x42            ; Graphics mode, video OFF
    out PORT_MODE, al
    
    ; Clear screen first
    call clear_screen
    
    ; Set palette from BMP
    call set_bmp_palette
    
    ; Display BMP image
    call decode_bmp
    
    ; Enable video output - image appears instantly!
    mov al, 0x4A            ; Graphics mode, video ON
    out PORT_MODE, al
    
    ; Close file
    mov bx, [file_handle]
    mov ah, 0x3E            ; DOS Close File
    int 0x21
    
    ; Wait for keypress
    xor ah, ah
    int 0x16
    
    ; Reset palette to CGA defaults before exiting
    call set_cga_palette
    
    ; Disable graphics mode
    call disable_graphics_mode
    
    ; Restore text mode
    mov ax, 0x0003
    int 0x10
    
    ; Exit to DOS
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
    ; Fall through

.print_exit:
    mov ah, 0x09
    int 0x21
    mov ax, 0x4C01
    int 0x21

; ============================================================================
; enable_graphics_mode - Olivetti Prodest PC1 hidden 160x200x16 graphics mode
; ============================================================================
enable_graphics_mode:
    push ax
    push dx
    
    ; BIOS Mode 4: CGA 320x200 graphics
    mov ax, 0x0004
    int 0x10
    
    ; Configure V6355D register 0x67
    mov al, 0x67
    out PORT_REG_ADDR, al
    jmp short $+2
    mov al, 0x18            ; 8-bit bus mode
    out PORT_REG_DATA, al
    jmp short $+2
    
    ; Set monitor control register 0x65
    mov al, 0x65
    out PORT_REG_ADDR, al
    jmp short $+2
    mov al, 0x09            ; 200 lines, PAL, color
    out PORT_REG_DATA, al
    jmp short $+2
    
    ; Unlock 16-color mode
    mov al, 0x4A
    out PORT_MODE, al
    jmp short $+2
    jmp short $+2
    
    ; Set border color = black
    xor al, al
    out PORT_COLOR, al
    jmp short $+2
    jmp short $+2
    
    pop dx
    pop ax
    ret

; ============================================================================
; disable_graphics_mode - Reset V6355 registers
; ============================================================================
disable_graphics_mode:
    push ax
    push dx
    
    mov al, 0x67
    out PORT_REG_ADDR, al
    jmp short $+2
    mov al, 0x00
    out PORT_REG_DATA, al
    jmp short $+2
    
    mov al, 0x65
    out PORT_REG_ADDR, al
    jmp short $+2
    mov al, 0x09
    out PORT_REG_DATA, al
    jmp short $+2
    
    mov al, 0x28
    out PORT_MODE, al
    jmp short $+2
    
    pop dx
    pop ax
    ret

; ============================================================================
; set_bmp_palette - Set palette from BMP file
; BMP palette: 64 bytes (16 colors × 4 bytes BGRA, where A is unused)
; 6355 palette: 32 bytes (16 colors × 2 bytes)
;   Byte 1: Red (bits 0-2, values 0-7)
;   Byte 2: Green (bits 4-6) | Blue (bits 0-2)
; ============================================================================
set_bmp_palette:
    push ax
    push bx
    push cx
    push dx
    push si
    
    cli                     ; Disable interrupts
    
    ; Enable palette write
    mov al, 0x40
    out PORT_REG_ADDR, al
    jmp short $+2
    
    ; Convert 16 colors from BMP format (BGRA) to 6355 format
    ; Palette starts at offset 54 in bmp_header
    mov si, bmp_header + 54
    mov cx, 16
    
.palette_loop:
    ; BMP stores as: Blue, Green, Red, Alpha (we ignore alpha)
    lodsb                   ; Blue (0-255)
    mov bl, al              ; Save blue in BL
    
    lodsb                   ; Green (0-255)
    mov bh, al              ; Save green in BH
    
    lodsb                   ; Red (0-255)
    shr al, 5               ; Convert to 3-bit (0-7)
    out PORT_REG_DATA, al   ; Write Red to even register
    jmp short $+2
    
    ; Combine Green and Blue
    mov al, bh              ; Green
    and al, 0xE0            ; Keep upper 3 bits
    shr al, 1               ; Shift to bits 4-6
    mov ah, al              ; Save in AH
    
    mov al, bl              ; Blue
    shr al, 5               ; Convert to 3-bit
    or al, ah               ; Combine: Green (4-6) | Blue (0-2)
    out PORT_REG_DATA, al   ; Write GB to odd register
    jmp short $+2
    
    lodsb                   ; Skip alpha byte
    
    loop .palette_loop
    
    ; Disable palette write
    mov al, 0x80
    out PORT_REG_ADDR, al
    
    sti                     ; Re-enable interrupts
    
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; set_cga_palette - Reset palette to standard CGA text mode colors
; 6355 palette: 32 bytes (16 colors × 2 bytes)
;   Byte 1: Red (bits 0-2, values 0-7)
;   Byte 2: Green (bits 4-6) | Blue (bits 0-2)
; ============================================================================
set_cga_palette:
    push ax
    push cx
    push si
    
    cli                     ; Disable interrupts
    
    ; Enable palette write
    mov al, 0x40
    out PORT_REG_ADDR, al
    jmp short $+2
    
    ; Write 32 bytes of CGA palette data
    mov si, cga_colors
    mov cx, 32              ; 16 colors × 2 bytes
    
.pal_write_loop:
    lodsb                   ; Load byte from DS:SI into AL, inc SI
    out PORT_REG_DATA, al   ; Write to port 0xDE
    jmp short $+2           ; I/O delay
    loop .pal_write_loop
    
    ; Disable palette write
    mov al, 0x80
    out PORT_REG_ADDR, al
    
    sti                     ; Re-enable interrupts
    
    pop si
    pop cx
    pop ax
    ret

; ============================================================================
; clear_screen - Fill video memory with black
; ============================================================================
clear_screen:
    push ax
    push cx
    push di
    push es
    
    mov ax, VIDEO_SEG
    mov es, ax
    xor di, di
    mov cx, 8192            ; 16KB = 8192 words
    xor ax, ax
    cld
    rep stosw
    
    pop es
    pop di
    pop cx
    pop ax
    ret

; ============================================================================
; decode_bmp - Read BMP pixel data to video memory
; BMP format: 4-bit pixels, 2 pixels per byte (packed nibbles)
; BMP stores bottom-up (last row first), so we reverse it
; Rows are padded to dword (4-byte) boundaries
; ============================================================================
decode_bmp:
    push ax
    push bx
    push cx
    push dx
    push di
    push si
    push es
    
    ; Set ES to video memory
    mov ax, VIDEO_SEG
    mov es, ax
    
    ; Get image dimensions (stored as dwords in BMP, we only use low words)
    ; Width at offset 18 (dword)
    mov ax, [bmp_header + 18]       ; Low word of width
    mov [image_width], ax           ; Store original width
    
    ; Check if we need downscaling
    cmp ax, 160
    jbe .width_ok
    cmp ax, 320
    ja .width_too_large
    
    ; Width is 161-320, we'll downsample
    mov byte [downsample_flag], 1
    jmp .width_done
    
.width_too_large:
    mov ax, 320                     ; Clamp to 320
    mov [image_width], ax
    mov byte [downsample_flag], 1
    jmp .width_done
    
.width_ok:
    mov byte [downsample_flag], 0
    
.width_done:
    
    ; Height at offset 22 (dword) - may be negative for top-down BMPs!
    mov ax, [bmp_header + 22]       ; Low word of height
    cmp word [bmp_header + 24], 0   ; Check high word/sign
    je .height_positive
    
    ; Negative height means top-down (rare)
    neg ax
    
.height_positive:
    cmp ax, 200
    jbe .height_ok
    mov ax, 200                     ; Clamp to screen height
.height_ok:
    mov [image_height], ax
    
    ; Calculate bytes per row in file (with padding)
    ; For 4-bit: bytes = (width + 1) / 2, then round up to multiple of 4
    mov ax, [image_width]
    inc ax
    shr ax, 1               ; bytes = (width + 1) / 2
    add ax, 3               ; Round up to multiple of 4
    and ax, 0xFFFC
    
    ; Safety check - max 164 for 320 pixels + padding
    cmp ax, 164
    jbe .bpr_ok
    mov ax, 164
.bpr_ok:
    mov [bytes_per_row], ax
    
    ; BMP stores bottom-up, start from last row
    mov ax, [image_height]
    dec ax
    mov [current_row], ax
    
.row_loop:
    ; Calculate video memory offset for this row
    ; Even rows: offset = (row/2) * 80
    ; Odd rows:  offset = 0x2000 + (row/2) * 80
    mov ax, [current_row]
    push ax                 ; Save for odd/even test
    shr ax, 1               ; AX = row / 2
    mov bx, 80
    mul bx                  ; AX = (row/2) * 80
    mov di, ax              ; DI = base offset
    
    pop ax                  ; Restore row number
    test al, 1              ; Check if odd row
    jz .is_even_row
    add di, 0x2000          ; Add 8KB offset for odd rows
.is_even_row:
    
    ; Read one scanline from file
    mov bx, [file_handle]
    mov dx, row_buffer
    mov cx, [bytes_per_row]
    mov ah, 0x3F            ; DOS Read
    int 0x21
    jc .decode_done         ; Error reading
    or ax, ax               ; Check if we read any bytes
    jz .decode_done         ; EOF reached
    
    ; Check if we need to downsample
    cmp byte [downsample_flag], 0
    je .no_downsample
    
    ; Downsample 320 -> 160
    ; Read 2 bytes (4 pixels), output 1 byte (2 pixels)
    ; Input:  [pixel0][pixel1] [pixel2][pixel3]
    ; Output: [pixel0][pixel2]
    
    mov si, row_buffer
    mov cx, 80              ; Output 80 bytes (160 pixels from 320)
    
.downsample_loop:
    lodsb                   ; AL = [pixel0][pixel1]
    and al, 0xF0            ; AL = [pixel0][0]
    mov ah, al              ; Save pixel0 in AH
    
    lodsb                   ; AL = [pixel2][pixel3]
    shr al, 4               ; AL = [0][pixel2]
    or al, ah               ; AL = [pixel0][pixel2]
    
    mov [es:di], al         ; Write directly to video memory
    inc di
    
    loop .downsample_loop
    jmp .row_done
    
.no_downsample:
    ; Copy first 80 bytes for 160-wide images
    mov si, row_buffer
    mov cx, 80
    cld
    rep movsb
    
.row_done:
    
    ; Move to previous row (BMP is bottom-up)
    mov ax, [current_row]
    or ax, ax               ; Check if row 0
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

; ============================================================================
; Data Section
; ============================================================================

msg_info    db 'LOADBMP v1.0 - BMP Image Viewer for Olivetti Prodest PC1', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'Displays BMP images in 160x200 16-color mode.', 0x0D, 0x0A
            db 'Supports 160x200 and 320x200 BMP images (16 colors only).', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'Usage: LOADBMP filename.bmp', 0x0D, 0x0A
            db '       LOADBMP /? or /h for this help', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'RetroErik 2026', 0x0D, 0x0A, '$'
msg_file_err db 'Error: Cannot open file', 0x0D, 0x0A, '$'
msg_not_bmp db 'Error: Not a valid BMP file', 0x0D, 0x0A, '$'
msg_format  db 'Error: BMP must be 4-bit uncompressed', 0x0D, 0x0A, '$'

filename_ptr    dw 0
file_handle     dw 0
image_width     dw 0
image_height    dw 0
bytes_per_row   dw 0
current_row     dw 0
downsample_flag db 0            ; 1 if we need to downsample 320->160

; ============================================================================
; Standard CGA text mode palette (16 colors)
; Format: Byte 1 = Red (bits 0-2), Byte 2 = Green (bits 4-6) | Blue (bits 0-2)
; ============================================================================
cga_colors:
    db 0x00, 0x00    ; 0:  Black
    db 0x00, 0x05    ; 1:  Blue
    db 0x00, 0x50    ; 2:  Green
    db 0x00, 0x55    ; 3:  Cyan
    db 0x05, 0x00    ; 4:  Red
    db 0x05, 0x05    ; 5:  Magenta
    db 0x05, 0x20    ; 6:  Brown (dark yellow-orange)
    db 0x05, 0x55    ; 7:  Light Gray
    db 0x02, 0x22    ; 8:  Dark Gray
    db 0x02, 0x27    ; 9:  Light Blue
    db 0x02, 0x72    ; 10: Light Green
    db 0x02, 0x77    ; 11: Light Cyan
    db 0x07, 0x22    ; 12: Light Red
    db 0x07, 0x27    ; 13: Light Magenta
    db 0x07, 0x70    ; 14: Yellow
    db 0x07, 0x77    ; 15: White

; Reserve space for BMP header (includes palette)
bmp_header:     times 128 db 0  ; File header + Info header + Palette
row_buffer:     times 164 db 0  ; Max row size for 320 pixels + padding
row_buffer_out: times 84 db 0   ; Output buffer for downsampled row
