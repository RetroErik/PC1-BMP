; ============================================================================
; PC1-BMP2.ASM (v 1.0) - BMP Viewer with CGA Palette Flip
; ============================================================================
;
; Displays 320x200 4-bit BMP images on the Olivetti Prodest PC1 using
; CGA 320x200x4 mode with per-scanline V6355D palette reprogramming.
;
; THE TECHNIQUE:
;   CGA mode 4: 2 bits per pixel, values 0-3.
;   Pixel 0 = entry 0 (black/border), fixed.
;   Pixel 1 = entry 2 (even lines) / entry 3 (odd lines)
;   Pixel 2 = entry 4 (even) / entry 5 (odd)
;   Pixel 3 = entry 6 (even) / entry 7 (odd)
;
;   Per scanline during the visible area, ALL 8 palette entries are
;   reprogrammed with the 3 chosen colors for that scanline.
;   PAL_EVEN/PAL_ODD is flipped via port 0xD9 bit 5 each HBLANK.
;
;   This allows displaying images with up to 3 unique non-black
;   colors per scanline (plus black). When a scanline has more than
;   3 non-black colors, the 3 most frequent are kept and remaining
;   pixels are remapped to the nearest of {black, color1, color2,
;   color3} using RGB Manhattan distance.
;
; ALGORITHM:
;   1. Load and validate 320x200 4-bit BMP
;   2. Convert BMP 16-color BGRA palette to RGB arrays + V6355D format
;   3. PASS 1 (Analysis): For each scanline, count pixel color frequency,
;      pick top 3 most frequent colors, build palette stream table
;   4. PASS 2 (Render): Re-read BMP, remap each pixel to CGA value 0-3
;      using nearest-color matching, pack into 2bpp CGA VRAM
;   5. Enter display loop: VSYNC wait + per-scanline HSYNC-synced
;      palette flip and palette stream (cgaflip4 technique)
;
; CGA PALETTE MAPPING (with bg = entry 0):
;
;   Pixel  | Even (pal 0)  | Odd (pal 1)
;   -------+---------------+--------------
;     0    | entry 0 (bg)  | entry 0 (bg)    <- both black
;     1    | entry 2       | entry 3
;     2    | entry 4       | entry 5
;     3    | entry 6       | entry 7
;          | (entry 1 unused -- streamed through)
;
; LIMITATIONS:
;   - Only 320x200 4-bit uncompressed BMPs supported
;   - BMP palette index 0 is always treated as black (background)
;   - Maximum 3 non-black colors per scanline; excess mapped to nearest
;   - Uses INT 10h mode 4, which resets V6355D registers (not PERITEL
;     compatible)
;
; Written for NASM assembler
; Target: Olivetti Prodest PC1 with Yamaha V6355D video controller
; CPU: NEC V40 (80186 compatible) @ 8 MHz
;
; By Retro Erik - 2026
;
; ============================================================================
; BUILD
; ============================================================================
;
;   nasm -f bin -o PC1-BMP2.com PC1-BMP2.asm
;
; ============================================================================
; USAGE
; ============================================================================
;
;   PC1-BMP2 filename.bmp
;
;   Controls:
;     ESC : Exit to DOS
;     H   : Toggle HSYNC synchronization
;     V   : Toggle VSYNC synchronization
;
; ============================================================================

[BITS 16]
[ORG 0x100]

; ============================================================================
; Constants
; ============================================================================

VIDEO_SEG       equ 0xB800      ; CGA video RAM segment

; V6355D I/O Ports (short aliases for speed-critical code)
PORT_MODE       equ 0xD8        ; CGA Mode Control
PORT_COLOR      equ 0xD9        ; CGA Color Select / Palette Select
PORT_STATUS     equ 0xDA        ; CGA Status Register
PORT_REG_ADDR   equ 0xDD        ; V6355D Register Bank Address
PORT_REG_DATA   equ 0xDE        ; V6355D Register Bank Data

; BMP file header offsets
BMP_SIGNATURE   equ 0           ; 'BM' (2 bytes)
BMP_DATA_OFFSET equ 10          ; Offset to pixel data (dword)
BMP_WIDTH       equ 18          ; Image width (dword)
BMP_HEIGHT      equ 22          ; Image height (dword)
BMP_BPP         equ 28          ; Bits per pixel (word)
BMP_COMPRESSION equ 30          ; Compression (dword)
BMP_PALETTE_OFF equ 54          ; BMP palette starts here

; Screen constants
SCREEN_HEIGHT   equ 200
CGA_ROW_BYTES   equ 80          ; 320 pixels / 4 pixels per byte
BMP_ROW_BYTES   equ 160         ; 320 pixels at 4bpp

; CGA mode 4 control values (port 0xD8)
CGA_MODE4_OFF   equ 0x02        ; 320x200 graphics, video OFF
CGA_MODE4_ON    equ 0x0A        ; 320x200 graphics, video ON

; Palette flip constants
PAL_EVEN        equ 0x00        ; CGA palette 0, bg/border = entry 0
PAL_ODD         equ 0x20        ; CGA palette 1, bg/border = entry 0

; ============================================================================
; Main Program Entry Point
; ============================================================================
main:
    cld                         ; Clear direction flag for string ops

    ; --- Parse command line ---
    mov si, 0x81                ; Command line at PSP:0081

.skip_spaces:
    lodsb
    cmp al, ' '
    je .skip_spaces
    cmp al, 0x0D
    je .show_usage

    ; Check for /? or /h or /H
    cmp al, '/'
    jne .not_help
    lodsb
    cmp al, '?'
    je .show_usage
    cmp al, 'h'
    je .show_usage
    cmp al, 'H'
    je .show_usage
    dec si
    dec si
    jmp .save_filename

.not_help:
    dec si

.save_filename:
    mov [filename_ptr], si

.find_end:
    lodsb
    cmp al, ' '
    je .found_end
    cmp al, 0x0D
    jne .find_end

.found_end:
    dec si
    mov byte [si], 0            ; Null-terminate filename

    ; --- Open BMP file ---
    mov dx, [filename_ptr]
    mov ax, 0x3D00              ; DOS Open File (read-only)
    int 0x21
    jc .file_error
    mov [file_handle], ax

    ; --- Read header + palette (118 bytes) ---
    ; 14 (file header) + 40 (info header) + 64 (palette) = 118
    mov bx, ax
    mov dx, bmp_header
    mov cx, 118
    mov ah, 0x3F                ; DOS Read File
    int 0x21
    jc .file_error
    cmp ax, 118
    jb .file_error

    ; --- Validate BMP ---
    cmp word [bmp_header + BMP_SIGNATURE], 0x4D42   ; 'BM'
    jne .not_bmp

    cmp word [bmp_header + BMP_BPP], 4
    jne .wrong_format

    cmp word [bmp_header + BMP_COMPRESSION], 0
    jne .wrong_format
    cmp word [bmp_header + BMP_COMPRESSION + 2], 0
    jne .wrong_format

    ; Must be 320x200
    cmp word [bmp_header + BMP_WIDTH], 320
    jne .wrong_size
    cmp word [bmp_header + BMP_HEIGHT], 200
    jne .wrong_size

    ; --- Convert BMP palette to internal format ---
    call convert_bmp_palette

    ; --- Print loading message (while still in text mode) ---
    mov dx, msg_loading
    mov ah, 0x09
    int 0x21

    ; --- Seek to pixel data for Pass 1 ---
    mov bx, [file_handle]
    mov dx, [bmp_header + BMP_DATA_OFFSET]
    mov cx, [bmp_header + BMP_DATA_OFFSET + 2]
    mov ax, 0x4200              ; Seek from beginning
    int 0x21
    jc .file_error

    ; --- PASS 1: Analyze image (determine 3 colors per scanline) ---
    call analyze_image

    ; --- Set CGA mode 4 (320x200x4, clears screen) ---
    mov ax, 0x0004
    int 0x10
    cld                         ; BIOS may set DF — ensure forward string ops

    ; --- Blank video during VRAM write ---
    mov al, CGA_MODE4_OFF
    out PORT_MODE, al

    ; --- Seek back to pixel data for Pass 2 ---
    mov bx, [file_handle]
    mov dx, [bmp_header + BMP_DATA_OFFSET]
    mov cx, [bmp_header + BMP_DATA_OFFSET + 2]
    mov ax, 0x4200
    int 0x21
    ; (Not checking error - same seek worked before)

    ; --- PASS 2: Remap pixels and write to CGA VRAM ---
    call render_to_vram

    ; --- Close file ---
    mov bx, [file_handle]
    mov ah, 0x3E                ; DOS Close File
    int 0x21

    ; --- Enable video ---
    mov al, CGA_MODE4_ON
    out PORT_MODE, al

    ; --- Display loop with per-scanline palette flip ---
    mov byte [hsync_enabled], 1
    mov byte [vsync_enabled], 1

.display_loop:
    call wait_vblank
    call render_frame
    call check_keyboard
    cmp al, 0xFF
    jne .display_loop

    ; --- Cleanup and exit ---
    call set_cga_palette
    mov ax, 0x0003              ; Restore text mode
    int 0x10
    mov ax, 0x4C00
    int 0x21

; --- Error handlers ---
.file_error:
    mov dx, msg_file_err
    jmp .print_exit

.not_bmp:
    mov dx, msg_not_bmp
    jmp .print_exit

.wrong_format:
    mov dx, msg_format
    jmp .print_exit

.wrong_size:
    mov dx, msg_size
    jmp .print_exit

.show_usage:
    mov dx, msg_info
    mov ah, 0x09
    int 0x21
    mov ax, 0x4C00
    int 0x21

.print_exit:
    mov ah, 0x09
    int 0x21
    mov ax, 0x4C01
    int 0x21

; ============================================================================
; convert_bmp_palette
; ============================================================================
; Converts 16-color BMP palette (BGRA, 64 bytes at bmp_header+54) into:
;   - pal_r[16], pal_g[16], pal_b[16]: RGB888 components (for distance calc)
;   - v6355_pal[32]: V6355D format (2 bytes per color, for palette stream)
; ============================================================================
convert_bmp_palette:
    push ax
    push bx
    push cx
    push si
    push di

    mov si, bmp_header + BMP_PALETTE_OFF
    xor di, di                  ; Palette index 0-15
    mov cx, 16

.cvt_loop:
    ; BMP order: Blue, Green, Red, Alpha
    lodsb                       ; Blue (0-255)
    mov [pal_b + di], al

    lodsb                       ; Green (0-255)
    mov [pal_g + di], al

    lodsb                       ; Red (0-255)
    mov [pal_r + di], al

    ; Build V6355D 2-byte entry
    mov bx, di
    shl bx, 1                  ; BX = di * 2 (index into v6355_pal)

    ; Byte 1: Red >> 5 (3-bit, values 0-7)
    mov al, [pal_r + di]
    shr al, 5
    mov [v6355_pal + bx], al

    ; Byte 2: (Green & 0xE0) >> 1 | (Blue >> 5)
    mov al, [pal_g + di]
    and al, 0xE0                ; Keep top 3 bits of green
    shr al, 1                  ; Shift to bits 6-4
    mov ah, al                  ; Save green component
    mov al, [pal_b + di]
    shr al, 5                  ; Blue to bits 2-0
    or al, ah                  ; Combine: G[6:4] | B[2:0]
    mov [v6355_pal + bx + 1], al

    lodsb                       ; Skip alpha byte

    inc di
    loop .cvt_loop

    pop di
    pop si
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; analyze_image - Pass 1: Read all BMP rows, determine top 3 colors per
;                 scanline, build palette_stream
; ============================================================================
; BMP is bottom-up: first row read from file = screen row 199.
; ============================================================================
analyze_image:
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    mov word [current_row], 199

.analyze_loop:
    call read_bmp_row
    jc .analyze_done
    cmp ax, BMP_ROW_BYTES
    jb .analyze_done

    call analyze_scanline       ; Process row_buffer for [current_row]

    mov ax, [current_row]
    or ax, ax
    jz .analyze_done
    dec word [current_row]
    jmp .analyze_loop

.analyze_done:
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; analyze_scanline - Count colors, find top 3, build palette stream entry
; Input: [current_row], row_buffer filled with BMP pixel data
; Output: scanline_top3 and palette_stream updated for this row
; ============================================================================
analyze_scanline:
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    ; --- Clear color counts (16 words = 32 bytes) ---
    ; NOTE: Uses explicit DS:DI loop instead of REP STOSW (which needs ES:DI)
    ; to avoid crash if ES was clobbered by DOS INT 21h
    mov di, color_count
    mov cx, 16
.clr_counts:
    mov word [di], 0
    add di, 2
    loop .clr_counts

    ; --- Count pixel colors (320 pixels from 160 bytes) ---
    mov si, row_buffer
    mov cx, BMP_ROW_BYTES       ; 160 bytes = 320 pixels

.count_loop:
    lodsb                       ; AL = [px_hi | px_lo]
    mov ah, al

    ; High nibble = left pixel
    shr al, 4
    xor bx, bx
    mov bl, al
    shl bx, 1                  ; Word index
    inc word [color_count + bx]

    ; Low nibble = right pixel
    mov al, ah
    and al, 0x0F
    xor bx, bx
    mov bl, al
    shl bx, 1
    inc word [color_count + bx]

    loop .count_loop

    ; --- Exclude index 0 (always background/black) ---
    mov word [color_count], 0

    ; --- Find top 3 most frequent colors ---
    call find_max_color         ; Returns AL = best index
    mov [top3_temp], al
    xor bx, bx
    mov bl, al
    shl bx, 1
    mov word [color_count + bx], 0  ; Remove from future searches

    call find_max_color
    mov [top3_temp + 1], al
    xor bx, bx
    mov bl, al
    shl bx, 1
    mov word [color_count + bx], 0

    call find_max_color
    mov [top3_temp + 2], al

    ; --- Store top3 indices for Pass 2 ---
    mov bx, [current_row]
    mov ax, bx
    shl ax, 1
    add bx, ax                  ; BX = row * 3

    mov al, [top3_temp]
    mov [scanline_top3 + bx], al
    mov al, [top3_temp + 1]
    mov [scanline_top3 + bx + 1], al
    mov al, [top3_temp + 2]
    mov [scanline_top3 + bx + 2], al

    ; --- Build palette stream entry (16 bytes) ---
    mov bx, [current_row]
    shl bx, 4                  ; BX = row * 16
    lea di, [palette_stream + bx]

    ; Entries 0-1: always black (bg/border + unused)
    xor ax, ax
    mov [di], ax                ; Entry 0: 0x00, 0x00
    mov [di + 2], ax            ; Entry 1: 0x00, 0x00

    ; Entries 2-3: V6355D color for pixel value 1
    ; (same color on both even and odd palette sets)
    xor bx, bx
    mov bl, [top3_temp]
    shl bx, 1                  ; Index into v6355_pal
    mov ax, [v6355_pal + bx]
    mov [di + 4], ax            ; Entry 2 (even lines, px value 1)
    mov [di + 6], ax            ; Entry 3 (odd lines, px value 1)

    ; Entries 4-5: V6355D color for pixel value 2
    xor bx, bx
    mov bl, [top3_temp + 1]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 8], ax            ; Entry 4
    mov [di + 10], ax           ; Entry 5

    ; Entries 6-7: V6355D color for pixel value 3
    xor bx, bx
    mov bl, [top3_temp + 2]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 12], ax           ; Entry 6
    mov [di + 14], ax           ; Entry 7

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; find_max_color - Find palette index (1-15) with highest pixel count
; Returns: AL = best index (0 if no non-zero counts remain)
; ============================================================================
find_max_color:
    push bx
    push cx
    push dx

    xor ax, ax                  ; Best index = 0
    xor dx, dx                  ; Best count = 0
    mov bx, 2                   ; Start at color_count[1] (word offset)
    mov cx, 15                  ; Check indices 1-15

.fmc_loop:
    cmp [color_count + bx], dx
    jbe .fmc_not_better
    mov dx, [color_count + bx]
    mov ax, bx
    shr ax, 1                  ; Convert word offset → palette index

.fmc_not_better:
    add bx, 2
    loop .fmc_loop

    ; AL = best index (0 if none found)
    pop dx
    pop cx
    pop bx
    ret

; ============================================================================
; render_to_vram - Pass 2: Read BMP again, remap pixels, write to CGA VRAM
; ============================================================================
; For each row:
;   1. Read BMP row (4bpp, 160 bytes)
;   2. Build remap table (16 entries: BMP index → CGA value 0-3)
;   3. Convert 4bpp → 2bpp using remap, write to interlaced CGA VRAM
; ============================================================================
render_to_vram:
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es

    mov ax, VIDEO_SEG
    mov es, ax

    mov word [current_row], 199

.rv_loop:
    call read_bmp_row
    jc .rv_done
    cmp ax, BMP_ROW_BYTES
    jb .rv_done

    ; Build remap table for this scanline's top-3 color assignment
    call build_remap_table

    ; Calculate CGA VRAM offset (interlaced layout)
    ; Even rows: (row/2) * 80
    ; Odd rows:  0x2000 + (row/2) * 80
    mov ax, [current_row]
    push ax
    shr ax, 1                  ; AX = row / 2
    mov bx, CGA_ROW_BYTES
    mul bx                     ; AX = (row/2) * 80  (DX clobbered but = 0)
    mov di, ax
    pop ax
    test al, 1
    jz .rv_even
    add di, 0x2000
.rv_even:

    ; Convert 4bpp BMP → 2bpp CGA
    ; SI reads from row_buffer (DS:SI), XLAT uses remap_table (DS:BX)
    ; DI writes to CGA VRAM (ES:DI)
    mov si, row_buffer
    mov bx, remap_table
    mov cx, CGA_ROW_BYTES       ; 80 output bytes = 320 pixels

.rv_convert:
    ; Process 4 pixels into 1 CGA byte
    ; CGA byte layout: [px0(7:6)] [px1(5:4)] [px2(3:2)] [px3(1:0)]
    xor dh, dh                  ; Accumulator

    ; Pixel 0 (→ bits 7-6)
    lodsb
    mov dl, al                  ; Save for pixel 1
    shr al, 4                  ; High nibble = pixel 0 index
    xlat                        ; AL = CGA value (0-3)
    or dh, al
    shl dh, 2

    ; Pixel 1 (→ bits 5-4)
    mov al, dl
    and al, 0x0F                ; Low nibble = pixel 1 index
    xlat
    or dh, al
    shl dh, 2

    ; Pixel 2 (→ bits 3-2)
    lodsb
    mov dl, al
    shr al, 4
    xlat
    or dh, al
    shl dh, 2

    ; Pixel 3 (→ bits 1-0)
    mov al, dl
    and al, 0x0F
    xlat
    or dh, al

    ; Write CGA byte to VRAM
    mov al, dh
    stosb                       ; [ES:DI] = AL, DI++

    loop .rv_convert

    ; Next row (BMP bottom-up: 199 → 0)
    mov ax, [current_row]
    or ax, ax
    jz .rv_done
    dec word [current_row]
    jmp .rv_loop

.rv_done:
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; build_remap_table - Create 16-entry lookup: BMP index → CGA value (0-3)
; ============================================================================
; For each of the 16 BMP palette indices:
;   Index 0         → CGA 0 (always black)
;   Index == top3[0] → CGA 1
;   Index == top3[1] → CGA 2
;   Index == top3[2] → CGA 3
;   Otherwise        → nearest of {black, top3[0..2]} by RGB distance
; ============================================================================
build_remap_table:
    push ax
    push bx
    push cx
    push dx
    push si

    ; Load this scanline's top 3 chosen BMP palette indices
    mov bx, [current_row]
    mov ax, bx
    shl ax, 1
    add bx, ax                  ; BX = row * 3

    mov al, [scanline_top3 + bx]
    mov [top3_temp], al
    mov al, [scanline_top3 + bx + 1]
    mov [top3_temp + 1], al
    mov al, [scanline_top3 + bx + 2]
    mov [top3_temp + 2], al

    ; Map each BMP index 0-15 to CGA value 0-3
    xor cx, cx                  ; CL = current palette index

.brt_loop:
    ; Index 0 → always black (check FIRST to avoid false match with top3)
    or cl, cl
    jz .brt_black

    ; Direct match with chosen colors?
    cmp cl, [top3_temp]
    je .brt_1
    cmp cl, [top3_temp + 1]
    je .brt_2
    cmp cl, [top3_temp + 2]
    je .brt_3

    ; No direct match: find nearest by RGB distance
    call find_nearest           ; CL = index, returns AL = CGA value
    jmp .brt_store

.brt_black:
    xor al, al
    jmp .brt_store
.brt_1:
    mov al, 1
    jmp .brt_store
.brt_2:
    mov al, 2
    jmp .brt_store
.brt_3:
    mov al, 3

.brt_store:
    mov bx, cx
    mov [remap_table + bx], al
    inc cx
    cmp cx, 16
    jb .brt_loop

    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; find_nearest - Map a BMP palette index to the nearest available CGA value
; ============================================================================
; Uses RGB888 Manhattan distance: |dR| + |dG| + |dB|
; Compares against: black (0,0,0), top3[0], top3[1], top3[2]
; Skips top3 entries that are 0 (empty slot = fewer than 3 colors)
;
; Input:  CL = BMP palette index to remap
; Output: AL = CGA pixel value (0-3)
; ============================================================================
find_nearest:
    push bx
    push cx
    push dx
    push si

    ; Source color index in BX
    xor bh, bh
    mov bl, cl

    ; --- Distance to black (0,0,0) = R + G + B ---
    xor dx, dx
    mov al, [pal_r + bx]
    xor ah, ah
    add dx, ax
    mov al, [pal_g + bx]
    add dx, ax
    mov al, [pal_b + bx]
    add dx, ax
    mov [nn_best_dist], dx
    mov byte [nn_best_cga], 0

    ; --- Distance to top3[0] → CGA 1 ---
    mov al, [top3_temp]
    or al, al
    jz .fn_done                 ; No colors available at all
    xor ah, ah
    mov si, ax                  ; SI = target index
    call compute_dist_bx_si     ; DX = distance
    cmp dx, [nn_best_dist]
    jae .fn_try_2
    mov [nn_best_dist], dx
    mov byte [nn_best_cga], 1

.fn_try_2:
    mov al, [top3_temp + 1]
    or al, al
    jz .fn_done
    xor ah, ah
    mov si, ax
    call compute_dist_bx_si
    cmp dx, [nn_best_dist]
    jae .fn_try_3
    mov [nn_best_dist], dx
    mov byte [nn_best_cga], 2

.fn_try_3:
    mov al, [top3_temp + 2]
    or al, al
    jz .fn_done
    xor ah, ah
    mov si, ax
    call compute_dist_bx_si
    cmp dx, [nn_best_dist]
    jae .fn_done
    mov byte [nn_best_cga], 3

.fn_done:
    mov al, [nn_best_cga]

    pop si
    pop dx
    pop cx
    pop bx
    ret

; ============================================================================
; compute_dist_bx_si - RGB888 Manhattan distance between two palette entries
; Input:  BX = palette index 1, SI = palette index 2
; Output: DX = |R1-R2| + |G1-G2| + |B1-B2|
; Clobbers: AX
; ============================================================================
compute_dist_bx_si:
    xor dx, dx

    ; |R[bx] - R[si]|
    mov al, [pal_r + bx]
    sub al, [pal_r + si]
    jns .cd_r_pos
    neg al
.cd_r_pos:
    xor ah, ah
    add dx, ax

    ; |G[bx] - G[si]|
    mov al, [pal_g + bx]
    sub al, [pal_g + si]
    jns .cd_g_pos
    neg al
.cd_g_pos:
    xor ah, ah
    add dx, ax

    ; |B[bx] - B[si]|
    mov al, [pal_b + bx]
    sub al, [pal_b + si]
    jns .cd_b_pos
    neg al
.cd_b_pos:
    xor ah, ah
    add dx, ax

    ret

; ============================================================================
; read_bmp_row - Read one row of BMP pixel data into row_buffer
; Returns: CF set on error, AX = bytes read
; ============================================================================
read_bmp_row:
    push bx
    push cx
    push dx
    push es                     ; Save ES — INT 21h may clobber it

    mov bx, [file_handle]
    mov dx, row_buffer
    mov cx, BMP_ROW_BYTES       ; 160 bytes
    mov ah, 0x3F                ; DOS Read File
    int 0x21

    pop es                      ; Restore ES
    pop dx
    pop cx
    pop bx
    ret

; ============================================================================
; render_frame - Per-scanline palette flip + palette data stream
; ============================================================================
; Adapted from cgaflip4.asm. Runs every frame:
;   - Wait for HSYNC edge (if enabled)
;   - Flip CGA palette set via port 0xD9 (1 OUT during HBLANK)
;   - Stream all 8 palette entries (16 bytes via LODSB/OUT to 0xDE)
;     during the visible area (~160 cycles, within 424-cycle budget)
;
; Data source: palette_stream[200 lines × 16 bytes/line = 3200 bytes]
; ============================================================================
render_frame:
    cli

    mov si, palette_stream
    mov cx, SCREEN_HEIGHT
    mov bl, PAL_EVEN
    mov bh, PAL_ODD

    cmp byte [hsync_enabled], 0
    je .rf_no_hsync

    ; ------------------------------------------------------------------
    ; HSYNC-synchronized loop
    ; ------------------------------------------------------------------
.rf_scanline:
.rf_wait_low:
    in al, PORT_STATUS
    test al, 0x01
    jnz .rf_wait_low

.rf_wait_high:
    in al, PORT_STATUS
    test al, 0x01
    jz .rf_wait_high

    ; === HBLANK: flip palette (1 OUT) ===
    mov al, bl
    out PORT_COLOR, al

    ; === VISIBLE AREA: stream all 8 palette entries (16 bytes) ===
    mov al, 0x40
    out PORT_REG_ADDR, al       ; Open palette at entry 0

    %rep 16
    lodsb
    out PORT_REG_DATA, al
    %endrep

    mov al, 0x80
    out PORT_REG_ADDR, al       ; Close palette

    ; Swap even/odd for next scanline
    xchg bl, bh

    loop .rf_scanline
    jmp .rf_done

    ; ------------------------------------------------------------------
    ; Non-synchronized loop (for testing with H toggle)
    ; ------------------------------------------------------------------
.rf_no_hsync:
.rf_nosync_line:
    mov al, bl
    out PORT_COLOR, al

    mov al, 0x40
    out PORT_REG_ADDR, al

    %rep 16
    lodsb
    out PORT_REG_DATA, al
    %endrep

    mov al, 0x80
    out PORT_REG_ADDR, al

    xchg bl, bh
    loop .rf_nosync_line

.rf_done:
    ; Reset to palette 0
    mov al, PAL_EVEN
    out PORT_COLOR, al

    sti
    ret

; ============================================================================
; wait_vblank
; ============================================================================
wait_vblank:
    cmp byte [vsync_enabled], 0
    je .wv_skip

.wv_wait_end:
    in al, PORT_STATUS
    test al, 0x08
    jnz .wv_wait_end

.wv_wait_start:
    in al, PORT_STATUS
    test al, 0x08
    jz .wv_wait_start

.wv_skip:
    ret

; ============================================================================
; check_keyboard
; ============================================================================
check_keyboard:
    mov ah, 0x01
    int 0x16
    jz .ck_no_key

    mov ah, 0x00
    int 0x16

    cmp ah, 0x01                ; ESC scancode
    jne .ck_not_esc
    mov al, 0xFF
    ret

.ck_not_esc:
    cmp al, 'h'
    je .ck_toggle_h
    cmp al, 'H'
    jne .ck_not_h
.ck_toggle_h:
    xor byte [hsync_enabled], 1
    jmp .ck_no_key

.ck_not_h:
    cmp al, 'v'
    je .ck_toggle_v
    cmp al, 'V'
    jne .ck_no_key
.ck_toggle_v:
    xor byte [vsync_enabled], 1

.ck_no_key:
    xor al, al
    ret

; ============================================================================
; set_cga_palette - Restore default CGA text mode palette before exit
; ============================================================================
set_cga_palette:
    push ax
    push cx
    push si

    cli

    mov al, 0x40
    out PORT_REG_ADDR, al
    jmp short $+2

    mov si, cga_colors
    mov cx, 32                  ; 16 colors × 2 bytes

.scp_loop:
    lodsb
    out PORT_REG_DATA, al
    jmp short $+2               ; I/O delay
    loop .scp_loop

    mov al, 0x80
    out PORT_REG_ADDR, al

    sti

    pop si
    pop cx
    pop ax
    ret

; ============================================================================
; DATA - Messages
; ============================================================================

msg_info    db 'PC1-BMP2 v1.0 - BMP Viewer with CGA Palette Flip', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'Displays 320x200 4-bit BMP images using per-scanline', 0x0D, 0x0A
            db 'palette reprogramming (3 colors + black per line).', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'Usage: PC1-BMP2 filename.bmp', 0x0D, 0x0A
            db '       ESC=exit, H=toggle HSYNC, V=toggle VSYNC', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'By RetroErik - 2026', 0x0D, 0x0A, '$'

msg_loading  db 'Analyzing image, please wait...', 0x0D, 0x0A, '$'
msg_file_err db 'Error: Cannot open file', 0x0D, 0x0A, '$'
msg_not_bmp  db 'Error: Not a valid BMP file', 0x0D, 0x0A, '$'
msg_format   db 'Error: BMP must be 4-bit uncompressed', 0x0D, 0x0A, '$'
msg_size     db 'Error: BMP must be 320x200', 0x0D, 0x0A, '$'

; ============================================================================
; DATA - Standard CGA text mode palette
; ============================================================================
; Format: Byte 1 = Red (bits 0-2), Byte 2 = Green (bits 4-6) | Blue (bits 0-2)
; ============================================================================

cga_colors:
    db 0x00, 0x00               ; 0:  Black
    db 0x00, 0x05               ; 1:  Blue
    db 0x00, 0x50               ; 2:  Green
    db 0x00, 0x55               ; 3:  Cyan
    db 0x05, 0x00               ; 4:  Red
    db 0x05, 0x05               ; 5:  Magenta
    db 0x05, 0x20               ; 6:  Brown
    db 0x05, 0x55               ; 7:  Light Gray
    db 0x02, 0x22               ; 8:  Dark Gray
    db 0x02, 0x27               ; 9:  Light Blue
    db 0x02, 0x72               ; 10: Light Green
    db 0x02, 0x77               ; 11: Light Cyan
    db 0x07, 0x22               ; 12: Light Red
    db 0x07, 0x27               ; 13: Light Magenta
    db 0x07, 0x70               ; 14: Yellow
    db 0x07, 0x77               ; 15: White

; ============================================================================
; DATA - Variables
; ============================================================================

filename_ptr:   dw 0
file_handle:    dw 0
current_row:    dw 0
hsync_enabled:  db 1
vsync_enabled:  db 1

; BMP palette RGB888 components (for nearest-color distance calculation)
pal_r:          times 16 db 0   ; Red channel 0-255
pal_g:          times 16 db 0   ; Green channel 0-255
pal_b:          times 16 db 0   ; Blue channel 0-255

; V6355D format palette (16 colors × 2 bytes each)
v6355_pal:      times 32 db 0

; Per-scanline analysis workspace
top3_temp:      times 3 db 0    ; Temp: top 3 indices for current scanline
nn_best_dist:   dw 0            ; Temp: nearest-neighbor best distance
nn_best_cga:    db 0            ; Temp: nearest-neighbor best CGA value

; Per-scanline chosen colors (200 rows × 3 indices = 600 bytes)
scanline_top3:  times 600 db 0

; Color frequency workspace (16 words = 32 bytes)
color_count:    times 32 db 0

; Pixel remap table (16 bytes, rebuilt per scanline during Pass 2)
remap_table:    times 16 db 0

; ============================================================================
; Palette stream for render loop
; 200 lines × 16 bytes per line = 3200 bytes
; ============================================================================
; Each line: 8 palette entries × 2 bytes each
;   [0,1]   Entry 0: always black (bg/border)
;   [2,3]   Entry 1: always black (unused by any pixel value)
;   [4,5]   Entry 2: scanline color 1 (even lines, pixel value 1)
;   [6,7]   Entry 3: scanline color 1 (odd lines, pixel value 1)
;   [8,9]   Entry 4: scanline color 2 (even lines, pixel value 2)
;   [10,11] Entry 5: scanline color 2 (odd lines, pixel value 2)
;   [12,13] Entry 6: scanline color 3 (even lines, pixel value 3)
;   [14,15] Entry 7: scanline color 3 (odd lines, pixel value 3)
; ============================================================================
palette_stream: times 3200 db 0

; File I/O buffers
bmp_header:     times 128 db 0  ; BMP file header + info header + palette
row_buffer:     times 164 db 0  ; BMP row input (160 bytes + safety margin)

; ============================================================================
; END OF PROGRAM
; ============================================================================
