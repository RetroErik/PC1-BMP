; ============================================================================
; PC1-BMP8.ASM (v 6.0) - BMP Viewer with 3-Method Hero Selection (Switchable)
; ============================================================================
;
; Displays 320x200 4-bit BMP images on the Olivetti Prodest PC1 using
; CGA 320x200x4 mode with per-scanline V6355D palette reprogramming.
;
; THE HERO TECHNIQUE (optimized with 0x44 start + OUTSB + skip-unchanged):
;   CGA mode 4: 2 bits per pixel, values 0-3.
;   Always uses palette 0 (no palette flip). Mapping:
;     Pixel 0 = entry 0 = black (background/border), fixed
;     Pixel 1 = entry 2 = per-scanline "hero" color (changes every line)
;     Pixel 2 = entry 4 = global color A (most common, fixed)
;     Pixel 3 = entry 6 = global color B (2nd most common, fixed)
;
;   During each HBLANK (~80 cycles), only palette entry 2 is updated.
;   Starting at register 0x44 skips entries 0-1 entirely (proven on
;   V6355D hardware). OUTSB replaces LODSB+OUT for faster streaming.
;
;   Total per-scanline: open(0x44) + 2×OUTSB + close(0x80) = ~48 cycles.
;   This fits ENTIRELY within the ~80 cycle HBLANK — ZERO spillover!
;
;   Lines where the hero color doesn't change from the previous line
;   are SKIPPED entirely — zero palette writes, zero noise.
;
;   The two global colors (entries 4 and 6) are programmed once before
;   the display loop begins and remain constant throughout.
;
; FLICKER-FREE IMPROVEMENT OVER PC1-BMP2:
;   PC1-BMP2 used 0x40 start (8 OUTs = ~99 cycles > 80 cycle HBLANK)
;   which caused ~19 cycle spillover = left-edge flicker.
;   PC1-BMP5 uses 0x44 start + OUTSB (4 instructions = ~48 cycles)
;   which fits entirely within HBLANK. Plus skip-unchanged lines
;   eliminate all palette noise on lines where hero color is constant.
;
; NOTE ON REP OUTSB (tested, does NOT work):
;   REP OUTSB streams bytes too fast for the V6355D — the chip needs
;   inter-byte gaps (instruction fetch/decode overhead) to latch each
;   palette data byte. Without those gaps, palette data is corrupted,
;   causing MORE flickering than individual OUTs.
;
; COLOR SELECTION (3 switchable methods, all sharing smart globals):
;   - Global A: the most common non-black color across the entire image
;   - Global B: the color with highest "need" = pixel_count × distance
;     to nearest available (black, global A). This picks the color that
;     benefits the most from being always-available, not just the 2nd
;     most frequent. E.g., white with 8000 pixels and dist 500 from
;     red/black scores higher than cyan with 10000 pixels close to black.
;
;   HERO SELECTION (switchable via keys 1/2/3):
;     Method 1 — Frequency-based (BMP5-style):
;       Pick the most frequent non-black, non-global color on the line.
;       Simple and fast, but may waste the hero on a color already
;       well-served by globals.
;     Method 2 — Individual-need (BMP7-style):
;       Score = pixel_count × (distance_to_nearest_available >> 2).
;       The color with the highest score becomes hero. Prioritizes
;       colors that would be BADLY approximated without the hero slot.
;     Method 3 — Total-error minimization (default):
;       For each candidate hero, compute the total pixel error across
;       the ENTIRE scanline: sum(count[c] × min_dist(c, {black, hero,
;       gA, gB})). The candidate with the LOWEST total error wins.
;       Naturally balances frequency vs distance.
;
;   All 3 hero streams are pre-computed at load time. Keys 1/2/3
;   switch live by copying the chosen method's data to active buffers
;   and re-rendering VRAM from the BMP file.
;
;   - Remaining colors are remapped to the nearest of {black, hero,
;     global A, global B} using RGB Manhattan distance
;
; ALGORITHM:
;   1. Load and validate 320x200 4-bit BMP
;   2. Convert BMP 16-color BGRA palette to RGB arrays + V6355D format
;   3. PASS 1 (Analysis): For each scanline, count pixel color frequency,
;      pick top 3 most frequent. Accumulate global pixel counts.
;   4. Find 2 global colors (most common across entire image)
;   5. Pre-compute base distances (to black + globals) for all colors
;   5b. Build palette stream: choose hero per line using total-error
;       minimization, store V6355D data
;   6. PASS 2 (Render): Re-read BMP, remap each pixel to CGA value 0-3
;      using nearest-color matching, pack into 2bpp CGA VRAM
;   7. Program entries 4,6 with global colors (once)
;   8. Enter display loop: VSYNC wait + per-scanline HSYNC-synced
;      entry 2 update (cgaflip3 technique)
;
; CGA PALETTE ENTRY MAPPING (palette 0 only, no flip):
;
;   Pixel  | Entry  | Color
;   -------+--------+-----------------------------------
;     0    |   0    | Black (bg/border, fixed)
;     1    |   2    | Hero color (changes per scanline)
;     2    |   4    | Global color A (fixed)
;     3    |   6    | Global color B (fixed)
;          |  1,3,5,7 unused (streamed-through / not referenced)
;
; LIMITATIONS:
;   - Only 320x200 4-bit uncompressed BMPs supported
;   - BMP palette index 0 is always treated as black (background)
;   - 2 fixed global colors + 1 per-scanline color + black = 3+1 per line
;   - Images with many distinct colors per line will have reduced quality
;   - With 0x44 start + OUTSB, palette write fits within HBLANK
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
;   nasm -f bin -o PC1-BMP8.com PC1-BMP8.asm
;
; ============================================================================
; USAGE
; ============================================================================
;
;   PC1-BMP8 filename.bmp
;
;   Controls:
;     ESC : Exit to DOS
;     1   : Method 1 - Frequency-based hero (BMP5-style)
;     2   : Method 2 - Individual-need hero (BMP7-style)
;     3   : Method 3 - Total-error minimization (default)
;     H   : Toggle HSYNC synchronization
;     V   : Toggle VSYNC synchronization
;
; ============================================================================

[BITS 16]
[CPU 186]
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

; Palette select constant (always palette 0, no flip)
PAL_EVEN        equ 0x00        ; CGA palette 0, bg/border = entry 0

; ============================================================================
; Main Program Entry Point
; ============================================================================
main:
    cld                         ; Clear direction flag for string ops
    push ds
    pop es                      ; Ensure ES = DS (safety for .COM)

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

    ; --- Print diagnostic messages for debugging ---
    mov dx, msg_phase1
    mov ah, 0x09
    int 0x21

    ; --- Seek to pixel data for Pass 1 ---
    mov bx, [file_handle]
    mov dx, [bmp_header + BMP_DATA_OFFSET]
    mov cx, [bmp_header + BMP_DATA_OFFSET + 2]
    mov ax, 0x4200              ; Seek from beginning
    int 0x21
    jc .file_error

    ; --- PASS 1: Analyze image ---
    ; Count pixel colors per scanline, find top 3 per line,
    ; and accumulate global pixel counts across entire image.
    call analyze_image

    ; --- Find 2 most common global colors across entire image ---
    ; These become the fixed colors in entries 4 and 6.
    call find_global_colors

    ; --- Build all 3 hero selection methods ---
    ; Method 1: frequency-based (BMP5-style)
    call build_heroes_freq
    call build_stream_from_heroes
    ; Copy active buffers to method 1 storage
    mov si, scanline_hero
    mov di, sh_method1
    mov cx, 200
    rep movsb
    mov si, palette_stream
    mov di, ps_method1
    mov cx, 400
    rep movsb
    mov si, stream_len
    mov di, sl_method1
    mov cx, 200
    rep movsb

    ; Method 2: individual-need scoring (BMP7-style)
    call build_heroes_need
    call build_stream_from_heroes
    mov si, scanline_hero
    mov di, sh_method2
    mov cx, 200
    rep movsb
    mov si, palette_stream
    mov di, ps_method2
    mov cx, 400
    rep movsb
    mov si, stream_len
    mov di, sl_method2
    mov cx, 200
    rep movsb

    ; Method 3: total-error minimization (default)
    call build_palette_stream
    call build_stream_lengths
    mov si, scanline_hero
    mov di, sh_method3
    mov cx, 200
    rep movsb
    mov si, palette_stream
    mov di, ps_method3
    mov cx, 400
    rep movsb
    mov si, stream_len
    mov di, sl_method3
    mov cx, 200
    rep movsb

    ; --- Default method = 3 (already in active buffers) ---
    mov byte [active_method], 3

    ; --- Pass 1 done, notify user ---
    mov dx, msg_phase2
    mov ah, 0x09
    int 0x21

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

    ; --- DO NOT close file — needed for method switching re-render ---

    ; --- Program global palette entries (4,6) before enabling video ---
    call program_display_palette

    ; --- Set palette 0 permanently (no flip) + bg/border = entry 0 ---
    mov al, PAL_EVEN
    out PORT_COLOR, al

    ; --- Enable video ---
    mov al, CGA_MODE4_ON
    out PORT_MODE, al

    ; --- Display loop ---
    ; Each frame: wait for VBLANK, then update entry 2 per scanline
    ; during HBLANK. Keys 1/2/3 switch hero selection method.
    mov byte [hsync_enabled], 1
    mov byte [vsync_enabled], 1

.display_loop:
    call wait_vblank
    call render_frame
    call check_keyboard
    cmp al, 0xFF
    je .exit_program
    cmp al, 0xFE                ; Method switch requested?
    jne .display_loop

    ; --- Method switch: re-render VRAM with new hero assignment ---
    mov al, CGA_MODE4_OFF
    out PORT_MODE, al           ; Blank video during re-render

    ; Seek to pixel data
    mov bx, [file_handle]
    mov dx, [bmp_header + BMP_DATA_OFFSET]
    mov cx, [bmp_header + BMP_DATA_OFFSET + 2]
    mov ax, 0x4200
    int 0x21

    ; Re-render with new scanline_hero
    call render_to_vram

    mov al, CGA_MODE4_ON
    out PORT_MODE, al           ; Re-enable video
    jmp .display_loop

.exit_program:
    ; --- Close file ---
    mov bx, [file_handle]
    mov ah, 0x3E
    int 0x21

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
;                 scanline, accumulate global pixel counts
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

    ; --- Clear global pixel counts before analysis ---
    mov di, global_pixel_count
    mov cx, 16
.clr_global:
    mov word [di], 0
    add di, 2
    loop .clr_global

    mov word [current_row], 199

.analyze_loop:
    call read_bmp_row
    jc .analyze_done
    cmp ax, BMP_ROW_BYTES
    jb .analyze_done

    call analyze_scanline       ; Process row_buffer for [current_row]

    ; --- Diagnostic: print '.' every 50 rows to show progress ---
    push ax
    push bx
    push dx
    mov ax, [current_row]
    xor dx, dx
    mov bx, 50
    div bx                      ; AX = row/50, DX = row mod 50
    or dx, dx
    jnz .no_dot
    mov dl, '.'
    mov ah, 0x02
    int 0x21
.no_dot:
    pop dx
    pop bx
    pop ax

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
; analyze_scanline - Count colors, find top 3 for this scanline
; Input: [current_row], row_buffer filled with BMP pixel data
; Output: scanline_top3 updated, global_pixel_count accumulated
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

    ; --- Accumulate into global pixel counts (for finding global colors) ---
    mov si, color_count
    mov di, global_pixel_count
    mov cx, 16
.accum_global:
    mov ax, [si]
    add [di], ax
    add si, 2
    add di, 2
    loop .accum_global

    ; --- Save full color counts for error-minimization hero selection ---
    ; Copy color_count[32 bytes] to scanline_counts[current_row * 32]
    ; (Used later by build_palette_stream to evaluate ALL colors, not just top 3)
    mov ax, [current_row]
    shl ax, 5                   ; AX = row × 32 (16 words × 2 bytes)
    mov di, scanline_counts
    add di, ax                  ; DI → scanline_counts[row]
    mov si, color_count         ; SI → source counts
    mov cx, 16
.save_counts:
    mov ax, [si]
    mov [di], ax
    add si, 2
    add di, 2
    loop .save_counts

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; find_global_colors - Find 2 most common non-black colors across entire image
; Called after analyze_image. Uses global_pixel_count[] accumulated during
; per-scanline analysis.
; Output: global_color_a, global_color_b = BMP palette indices
; ============================================================================
find_global_colors:
    push ax
    push bx
    push cx
    push dx

    ; --- Find most common color (excluding black = index 0) ---
    xor ax, ax                  ; Best index = 0
    xor dx, dx                  ; Best count = 0
    mov bx, 2                   ; Start at index 1 (word offset 2)
    mov cx, 15
.fgc_loop1:
    cmp [global_pixel_count + bx], dx
    jbe .fgc_not1
    mov dx, [global_pixel_count + bx]
    mov ax, bx
    shr ax, 1                  ; Convert word offset to index
.fgc_not1:
    add bx, 2
    loop .fgc_loop1

    mov [global_color_a], al

    ; --- Find second global: error-weighted selection ---
    ; For each color c (1-15, skip black and global_a):
    ;   dist = min(dist_to_black(c), dist_to_global_a(c))
    ;   score = global_pixel_count[c] × dist  (32-bit)
    ; Pick the color with the highest score.
    ; This selects the color that would be WORST served without its own slot.
    push si

    mov word [fgc_best_lo], 0
    mov word [fgc_best_hi], 0
    mov byte [global_color_b], 0

    mov cx, 1                   ; CX = color index 1-15
.fgc_loop2:
    cmp cl, [global_color_a]
    je .fgc_next2

    ; Load global pixel count
    mov bx, cx
    shl bx, 1
    mov ax, [global_pixel_count + bx]
    or ax, ax
    jz .fgc_next2               ; No pixels → skip
    mov [fgc_count], ax

    ; Distance to black = R + G + B
    mov bx, cx
    xor dx, dx
    mov al, [pal_r + bx]
    xor ah, ah
    add dx, ax
    mov al, [pal_g + bx]
    add dx, ax
    mov al, [pal_b + bx]
    add dx, ax
    mov [fgc_dist], dx          ; Start with dist to black

    ; Distance to global_a
    xor ah, ah
    mov al, [global_color_a]
    mov si, ax
    mov bx, cx                  ; BX = current color index
    call compute_dist_bx_si     ; DX = dist(c, globalA)
    cmp dx, [fgc_dist]
    jae .fgc_use_saved
    mov [fgc_dist], dx          ; Closer to globalA
.fgc_use_saved:

    ; Score = count × min_dist (32-bit)
    mov ax, [fgc_count]
    mul word [fgc_dist]         ; DX:AX = score

    ; Compare with best (32-bit: DX:AX vs fgc_best_hi:fgc_best_lo)
    cmp dx, [fgc_best_hi]
    jb .fgc_next2
    ja .fgc_update2
    cmp ax, [fgc_best_lo]
    jbe .fgc_next2

.fgc_update2:
    mov [fgc_best_lo], ax
    mov [fgc_best_hi], dx
    mov [global_color_b], cl

.fgc_next2:
    inc cx
    cmp cx, 16
    jb .fgc_loop2

    ; Fallback: if no good 2nd global found, use global_a (harmless)
    cmp byte [global_color_b], 0
    jne .fgc_done
    mov al, [global_color_a]
    mov [global_color_b], al
.fgc_done:

    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; build_heroes_freq - Method 1: Frequency-based hero (BMP5-style)
; ============================================================================
; For each scanline, picks the most frequent non-black, non-global color.
; Simple and fast — good for images dominated by a few colors per line.
; ============================================================================
build_heroes_freq:
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    xor cx, cx                  ; CX = current row (0-199)

.bhf_row:
    ; Get pointer to this row's color counts
    mov ax, cx
    shl ax, 5                   ; AX = row × 32
    mov si, scanline_counts
    add si, ax                  ; SI → counts for this row

    ; Find most frequent non-black, non-global
    mov word [bhf_best_count], 0
    mov byte [bhf_best_idx], 0

    mov dx, 1                   ; DX = color index 1-15
.bhf_color:
    cmp dl, [global_color_a]
    je .bhf_skip
    cmp dl, [global_color_b]
    je .bhf_skip

    mov bx, dx
    shl bx, 1
    mov ax, [si + bx]           ; AX = pixel count
    cmp ax, [bhf_best_count]
    jbe .bhf_skip

    mov [bhf_best_count], ax
    mov [bhf_best_idx], dl

.bhf_skip:
    inc dx
    cmp dx, 16
    jb .bhf_color

    ; Store hero
    mov al, [bhf_best_idx]
    or al, al
    jnz .bhf_ok
    mov al, [global_color_a]    ; Fallback
.bhf_ok:
    mov bx, cx
    mov [scanline_hero + bx], al

    inc cx
    cmp cx, 200
    jb .bhf_row

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; build_heroes_need - Method 2: Individual-need scoring (BMP7-style)
; ============================================================================
; For each scanline, scores each color: count × (dist_to_nearest_base >> 2).
; Picks the color with the HIGHEST score (most "needed" hero slot).
; ============================================================================
build_heroes_need:
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    ; Pre-compute base distances (needed for scoring)
    call precompute_base_dist

    xor cx, cx                  ; CX = current row (0-199)

.bhn_row:
    mov [bps_row], cx

    ; Get pointer to this row's color counts
    mov ax, cx
    shl ax, 5
    add ax, scanline_counts
    mov [bps_counts_ptr], ax

    mov word [bps_best_err_lo], 0   ; Reuse as "best score" (highest wins)
    mov byte [bps_best_idx], 0

    mov cx, 1                   ; CX = color index 1-15
.bhn_color:
    cmp cl, [global_color_a]
    je .bhn_next
    cmp cl, [global_color_b]
    je .bhn_next

    ; Load pixel count
    mov bx, cx
    shl bx, 1
    mov si, [bps_counts_ptr]
    mov ax, [si + bx]
    or ax, ax
    jz .bhn_next
    mov [bps_eval_count], ax

    ; Get pre-computed base distance for this color
    mov bx, cx
    shl bx, 1
    mov ax, [base_dist + bx]
    shr ax, 2                   ; >> 2 to keep in 16-bit range
    or ax, ax
    jz .bhn_next                ; Already well-served

    ; Score = count × (base_dist >> 2)
    mul word [bps_eval_count]   ; DX:AX = score
    cmp ax, [bps_best_err_lo]
    jbe .bhn_next

    mov [bps_best_err_lo], ax
    mov [bps_best_idx], cl

.bhn_next:
    inc cx
    cmp cx, 16
    jb .bhn_color

    ; Store hero
    mov al, [bps_best_idx]
    or al, al
    jnz .bhn_ok
    mov al, [global_color_a]
.bhn_ok:
    mov cx, [bps_row]
    mov bx, cx
    mov [scanline_hero + bx], al

    inc cx
    cmp cx, 200
    jb .bhn_row

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; build_stream_from_heroes - Derive palette_stream + stream_len from heroes
; ============================================================================
; Reads scanline_hero[200], produces palette_stream[400] and stream_len[200].
; Used by methods 1 and 2. Method 3 builds them directly.
; ============================================================================
build_stream_from_heroes:
    push ax
    push bx
    push cx
    push dx
    push si

    ; Build palette_stream from scanline_hero
    xor cx, cx
.bsfh_fill:
    mov bx, cx
    mov al, [scanline_hero + bx]

    xor bx, bx
    mov bl, al
    shl bx, 1
    mov ax, [v6355_pal + bx]

    mov bx, cx
    shl bx, 1
    mov [palette_stream + bx], ax

    inc cx
    cmp cx, 200
    jb .bsfh_fill

    ; Build stream_len by comparing consecutive entries
    call build_stream_lengths

    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; precompute_base_dist - Pre-compute base distances for all 16 palette colors
; ============================================================================
; For each color c (0-15), computes:
;   base_dist[c] = min(dist(c, black), dist(c, globalA), dist(c, globalB))
; This is the best approximation distance WITHOUT any hero color.
; Used by build_palette_stream to evaluate hero candidates.
; Must be called AFTER find_global_colors.
; ============================================================================
precompute_base_dist:
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    xor cx, cx                  ; CX = color index 0-15
    mov di, base_dist           ; DI → output array

.pbd_loop:
    mov bx, cx                  ; BX = color index for pal_r/g/b

    ; Distance to black (0,0,0) = R + G + B
    xor dx, dx
    mov al, [pal_r + bx]
    xor ah, ah
    add dx, ax
    mov al, [pal_g + bx]
    add dx, ax
    mov al, [pal_b + bx]
    add dx, ax
    mov [di], dx                ; base_dist[c] = dist to black initially

    ; Distance to global_a
    xor ah, ah
    mov al, [global_color_a]
    mov si, ax
    call compute_dist_bx_si     ; DX = dist(c, globalA), BX preserved
    cmp dx, [di]
    jae .pbd_try_b
    mov [di], dx

.pbd_try_b:
    ; Distance to global_b
    xor ah, ah
    mov al, [global_color_b]
    mov si, ax
    call compute_dist_bx_si     ; DX = dist(c, globalB), BX preserved
    cmp dx, [di]
    jae .pbd_next
    mov [di], dx

.pbd_next:
    add di, 2
    inc cx
    cmp cx, 16
    jb .pbd_loop

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; build_palette_stream - Total-error-minimization hero selection
; Called after find_global_colors. Uses scanline_counts[] and base_dist[].
;
; For each scanline, tries each non-black, non-global color as hero candidate.
; For each candidate, computes total pixel error across the ENTIRE scanline:
;   total_error = Σ count[c] × min(base_dist[c], dist(c, hero_candidate))
;
; The candidate with the LOWEST total error wins the hero slot.
;
; This naturally balances frequency vs distance:
;   - A color with many pixels and high base_dist needs the hero
;   - A color already close to a global contributes little error regardless
;   - The hero that helps the MOST pixels the MOST wins
;
; Also stores hero BMP palette index in scanline_hero[] for Pass 2.
; ============================================================================
build_palette_stream:
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    ; Pre-compute base distances for all colors
    call precompute_base_dist

    xor cx, cx                  ; CX = current row (0-199)

.bps_row_loop:
    mov [bps_row], cx

    ; Compute pointer to this row's scanline_counts
    mov ax, cx
    shl ax, 5                   ; AX = row × 32
    add ax, scanline_counts
    mov [bps_counts_ptr], ax

    ; Initialize best total error to max (32-bit)
    mov word [bps_best_err_lo], 0xFFFF
    mov word [bps_best_err_hi], 0xFFFF
    mov byte [bps_best_idx], 0

    ; Evaluate each candidate hero (colors 1-15, skip globals)
    mov byte [bps_candidate], 1

.bps_eval_hero:
    mov al, [bps_candidate]

    ; Skip globals (already available — making them hero wastes the slot)
    cmp al, [global_color_a]
    je .bps_next_hero
    cmp al, [global_color_b]
    je .bps_next_hero

    ; Skip if this color has no pixels on this line
    xor bx, bx
    mov bl, al
    shl bx, 1
    mov si, [bps_counts_ptr]
    cmp word [si + bx], 0
    je .bps_next_hero

    ; --- Compute total error for this hero candidate ---
    mov word [bps_total_lo], 0
    mov word [bps_total_hi], 0

    ; Save hero index as word for compute_dist_bx_si
    xor ah, ah
    mov al, [bps_candidate]
    mov [bps_hero_w], ax

    ; Evaluate all colors 1-15
    mov byte [bps_color], 1

.bps_eval_color:
    ; Load pixel count for this color on this line
    xor bx, bx
    mov bl, [bps_color]
    shl bx, 1
    mov si, [bps_counts_ptr]
    mov ax, [si + bx]           ; AX = pixel count
    or ax, ax
    jz .bps_next_color          ; No pixels → zero contribution
    mov [bps_eval_count], ax

    ; Compute distance from this color to hero candidate
    xor bx, bx
    mov bl, [bps_color]
    mov si, [bps_hero_w]
    call compute_dist_bx_si     ; DX = dist(color, hero)

    ; Effective distance = min(base_dist[color], dist_to_hero)
    push bx
    xor bx, bx
    mov bl, [bps_color]
    shl bx, 1
    cmp dx, [base_dist + bx]
    jbe .bps_hero_closer
    mov dx, [base_dist + bx]    ; Base (black/globals) is closer
.bps_hero_closer:
    pop bx

    ; Accumulate: total_error += count × effective_dist (32-bit)
    mov ax, [bps_eval_count]
    mul dx                      ; DX:AX = count × eff_dist
    add [bps_total_lo], ax
    adc [bps_total_hi], dx

.bps_next_color:
    inc byte [bps_color]
    cmp byte [bps_color], 16
    jb .bps_eval_color

    ; --- Compare this hero's total error with best so far ---
    ; Lower total error = better hero
    mov ax, [bps_total_hi]
    cmp ax, [bps_best_err_hi]
    ja .bps_next_hero           ; Worse (higher error)
    jb .bps_new_best            ; Better (lower error)
    mov ax, [bps_total_lo]
    cmp ax, [bps_best_err_lo]
    jae .bps_next_hero          ; Worse or equal

.bps_new_best:
    mov ax, [bps_total_lo]
    mov [bps_best_err_lo], ax
    mov ax, [bps_total_hi]
    mov [bps_best_err_hi], ax
    mov al, [bps_candidate]
    mov [bps_best_idx], al

.bps_next_hero:
    inc byte [bps_candidate]
    cmp byte [bps_candidate], 16
    jb .bps_eval_hero

    ; --- Best hero selected for this row ---
    mov al, [bps_best_idx]
    or al, al
    jnz .bps_hero_ok
    mov al, [global_color_a]    ; Fallback if no good hero found

.bps_hero_ok:
    ; Store hero BMP palette index
    mov cx, [bps_row]
    mov bx, cx
    mov [scanline_hero + bx], al

    ; Store V6355D color for entry 2
    xor bx, bx
    mov bl, al
    shl bx, 1                   ; Index into v6355_pal
    mov ax, [v6355_pal + bx]

    mov bx, [bps_row]
    shl bx, 1                   ; BX = row × 2
    mov [palette_stream + bx], ax

    mov cx, [bps_row]
    inc cx
    cmp cx, 200
    jb .bps_row_loop

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; build_stream_lengths - Determine per-line skip/write flag
; ============================================================================
; Compares consecutive palette_stream entries (2 bytes each = hero color).
; If entry matches previous line → stream_len = 0 (skip: no write needed)
; Otherwise → stream_len = 2 (write: update entry 2 with new hero color)
;
; Line 0 always gets stream_len = 2 (must write initial hero).
;
; When stream_len = 0, entry 2 already contains the correct color
; from the last line that wrote it (values are identical).
;
; Output: stream_len[200] filled with values 0 or 2
; ============================================================================
build_stream_lengths:
    push ax
    push bx
    push cx
    push si

    ; Line 0: always write
    mov byte [stream_len], 2

    ; Compare lines 1-199 to predecessor
    mov si, palette_stream          ; SI → line 0's data
    mov bx, stream_len + 1          ; BX → output for line 1
    mov cx, 199

.bsl_loop:
    ; Compare 2-byte hero color entry (word compare)
    mov ax, [si]                ; Previous line's hero
    cmp ax, [si + 2]            ; Current line's hero
    jne .bsl_write

    ; Same hero color — skip
    mov byte [bx], 0
    jmp .bsl_next

.bsl_write:
    mov byte [bx], 2

.bsl_next:
    add si, 2
    inc bx
    loop .bsl_loop

    pop si
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
; CGA value mapping:
;   0 = black (pixel value 0 → entry 0)
;   1 = per-scanline hero color (pixel value 1 → entry 2, updated per line)
;   2 = global color A (pixel value 2 → entry 4, fixed)
;   3 = global color B (pixel value 3 → entry 6, fixed)
;
; For each BMP palette index:
;   Index 0              → CGA 0 (black)
;   Index == hero        → CGA 1
;   Index == global_a    → CGA 2
;   Index == global_b    → CGA 3
;   Otherwise            → nearest of {black, hero, global_a, global_b}
; ============================================================================
build_remap_table:
    push ax
    push bx
    push cx
    push dx
    push si

    ; Load this scanline's color assignments
    mov bx, [current_row]
    mov al, [scanline_hero + bx]
    mov [top3_temp], al             ; top3_temp[0] = hero
    mov al, [global_color_a]
    mov [top3_temp + 1], al         ; top3_temp[1] = global A
    mov al, [global_color_b]
    mov [top3_temp + 2], al         ; top3_temp[2] = global B

    ; Map each BMP index 0-15 to CGA value 0-3
    xor cx, cx                  ; CL = current palette index

.brt_loop:
    ; Index 0 → always black
    or cl, cl
    jz .brt_black

    ; Direct match with chosen colors?
    cmp cl, [top3_temp]         ; Hero color?
    je .brt_1
    cmp cl, [top3_temp + 1]     ; Global A?
    je .brt_2
    cmp cl, [top3_temp + 2]     ; Global B?
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
; Compares against: black (0,0,0), hero, global_a, global_b
; (stored in top3_temp[0..2] by build_remap_table)
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
; render_frame - Per-scanline HBLANK palette update (optimized Hero)
; ============================================================================
; Hero technique with 0x44 start + OUTSB + skip-unchanged optimization.
;
; Per scanline during HBLANK:
;   1. Check stream_len[line] — skip if 0 (hero color unchanged)
;   2. Open palette at entry 2 (OUT 0xDD = 0x44) — skips E0-E1
;   3. Stream 2 bytes for entry 2 via OUTSB (hero color)
;   4. Close palette (OUT 0xDD = 0x80)
;
; Total for write lines: 4 instructions = ~48 cycles.
; HBLANK is ~80 cycles → ZERO spillover into visible area!
;
; PC1-BMP2 comparison: 8 OUTs (~99 cycles) → 4 instructions (~48 cycles)
;   = 52% fewer instructions, fits entirely within HBLANK.
;
; Skip lines: zero palette writes = zero noise.
;
; Entries 4,6 (global colors) are set once via program_display_palette.
;
; Uses BP to index stream_len[]. In COM file, SS=DS so [BP] is safe.
;
; Data: palette_stream[200 × 2], stream_len[200]
; ============================================================================
render_frame:
    cli
    cld                         ; Ensure OUTSB increments SI
    push bp

    mov si, palette_stream
    mov cx, SCREEN_HEIGHT
    mov dx, PORT_REG_DATA       ; DX = 0xDE for OUTSB

    cmp byte [hsync_enabled], 0
    je .rf_no_hsync

    ; ------------------------------------------------------------------
    ; HSYNC-synchronized loop with skip optimization
    ; ------------------------------------------------------------------
    mov bp, stream_len

.rf_scanline:
.rf_wait_low:
    in al, PORT_STATUS
    test al, 0x01
    jnz .rf_wait_low

.rf_wait_high:
    in al, PORT_STATUS
    test al, 0x01
    jz .rf_wait_high

    ; === HBLANK: check if hero color changed ===
    cmp byte [bp], 0
    je .rf_skip
    inc bp

    ; === Write entry 2 via 0x44 start + OUTSB ===
    mov al, 0x44
    out PORT_REG_ADDR, al       ; Open palette at entry 2 (skip E0-E1)

    outsb                       ; Entry 2 R (hero color)
    outsb                       ; Entry 2 G|B

    mov al, 0x80
    out PORT_REG_ADDR, al       ; Close palette

    loop .rf_scanline
    jmp .rf_done

.rf_skip:
    ; === Hero color unchanged — zero palette writes ===
    inc bp
    add si, 2                   ; Advance past this line's stream data
    loop .rf_scanline
    jmp .rf_done

    ; ------------------------------------------------------------------
    ; Non-synchronized loop (for testing with H toggle)
    ; ------------------------------------------------------------------
.rf_no_hsync:
.rf_nosync_line:
    mov al, 0x44
    out PORT_REG_ADDR, al       ; Open palette at entry 2

    outsb                       ; Entry 2 R
    outsb                       ; Entry 2 G|B

    mov al, 0x80
    out PORT_REG_ADDR, al       ; Close palette

    loop .rf_nosync_line

.rf_done:
    pop bp
    sti
    ret

; ============================================================================
; program_display_palette - Set V6355D entries for display
; ============================================================================
; Called once before the display loop. Sets:
;   Entry 0 = black (bg/border)
;   Entry 1 = black (unused)
;   Entry 2 = black (will be overwritten per-scanline by render_frame)
;   Entry 3 = black (not used — no palette flip)
;   Entry 4 = global color A (pixel value 2)
;   Entry 5 = black (not used — no palette flip)
;   Entry 6 = global color B (pixel value 3)
;   Entry 7 = black (not used — no palette flip)
; ============================================================================
program_display_palette:
    push ax
    push bx
    push si

    cli

    mov al, 0x40
    out PORT_REG_ADDR, al       ; Open palette at entry 0
    jmp short $+2

    ; Entries 0-3: all black (6 bytes + entry 2 placeholder + entry 3)
    xor al, al
    out PORT_REG_DATA, al       ; E0 R
    jmp short $+2
    out PORT_REG_DATA, al       ; E0 GB
    jmp short $+2
    out PORT_REG_DATA, al       ; E1 R
    jmp short $+2
    out PORT_REG_DATA, al       ; E1 GB
    jmp short $+2
    out PORT_REG_DATA, al       ; E2 R (placeholder)
    jmp short $+2
    out PORT_REG_DATA, al       ; E2 GB (placeholder)
    jmp short $+2
    out PORT_REG_DATA, al       ; E3 R (unused)
    jmp short $+2
    out PORT_REG_DATA, al       ; E3 GB (unused)
    jmp short $+2

    ; Entry 4 = global color A
    xor bx, bx
    mov bl, [global_color_a]
    shl bx, 1
    mov al, [v6355_pal + bx]
    out PORT_REG_DATA, al       ; E4 R
    jmp short $+2
    mov al, [v6355_pal + bx + 1]
    out PORT_REG_DATA, al       ; E4 GB
    jmp short $+2

    ; Entry 5 = black (unused)
    xor al, al
    out PORT_REG_DATA, al       ; E5 R
    jmp short $+2
    out PORT_REG_DATA, al       ; E5 GB
    jmp short $+2

    ; Entry 6 = global color B
    xor bx, bx
    mov bl, [global_color_b]
    shl bx, 1
    mov al, [v6355_pal + bx]
    out PORT_REG_DATA, al       ; E6 R
    jmp short $+2
    mov al, [v6355_pal + bx + 1]
    out PORT_REG_DATA, al       ; E6 GB
    jmp short $+2

    ; Entry 7 = black (unused)
    xor al, al
    out PORT_REG_DATA, al       ; E7 R
    jmp short $+2
    out PORT_REG_DATA, al       ; E7 GB
    jmp short $+2

    mov al, 0x80
    out PORT_REG_ADDR, al       ; Close palette

    sti

    pop si
    pop bx
    pop ax
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
; check_keyboard - Handle keys: ESC, 1/2/3 (method), H, V
; Returns: AL = 0xFF for ESC, 0xFE for method switch, 0x00 otherwise
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
    ; --- Method switching: keys 1, 2, 3 ---
    cmp al, '1'
    je .ck_method1
    cmp al, '2'
    je .ck_method2
    cmp al, '3'
    je .ck_method3

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

.ck_method1:
    cmp byte [active_method], 1
    je .ck_no_key               ; Already active
    mov byte [active_method], 1
    ; Copy method 1 data to active buffers
    mov si, sh_method1
    mov di, scanline_hero
    mov cx, 200
    rep movsb
    mov si, ps_method1
    mov di, palette_stream
    mov cx, 400
    rep movsb
    mov si, sl_method1
    mov di, stream_len
    mov cx, 200
    rep movsb
    mov al, 0xFE                ; Signal re-render needed
    ret

.ck_method2:
    cmp byte [active_method], 2
    je .ck_no_key
    mov byte [active_method], 2
    mov si, sh_method2
    mov di, scanline_hero
    mov cx, 200
    rep movsb
    mov si, ps_method2
    mov di, palette_stream
    mov cx, 400
    rep movsb
    mov si, sl_method2
    mov di, stream_len
    mov cx, 200
    rep movsb
    mov al, 0xFE
    ret

.ck_method3:
    cmp byte [active_method], 3
    je .ck_no_key
    mov byte [active_method], 3
    mov si, sh_method3
    mov di, scanline_hero
    mov cx, 200
    rep movsb
    mov si, ps_method3
    mov di, palette_stream
    mov cx, 400
    rep movsb
    mov si, sl_method3
    mov di, stream_len
    mov cx, 200
    rep movsb
    mov al, 0xFE
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

msg_info    db 'PC1-BMP8 v6.0 - 3-Method Hero Selection', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'Displays 320x200 4-bit BMP images using HBLANK', 0x0D, 0x0A
            db 'Hero palette with 3 switchable methods:', 0x0D, 0x0A
            db '  1=Frequency  2=Need  3=Total-Error (default)', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'Usage: PC1-BMP8 filename.bmp', 0x0D, 0x0A
            db '  ESC=exit H=hsync V=vsync 1/2/3=method', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'By RetroErik - 2026', 0x0D, 0x0A, '$'

msg_phase1   db 'Pass 1: Analyzing...', 0x0D, 0x0A, '$'
msg_phase2   db 'Pass 2: Rendering...', 0x0D, 0x0A, '$'
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
active_method:  db 3            ; Currently active method (1/2/3)

; BMP palette RGB888 components (for nearest-color distance calculation)
pal_r:          times 16 db 0   ; Red channel 0-255
pal_g:          times 16 db 0   ; Green channel 0-255
pal_b:          times 16 db 0   ; Blue channel 0-255

; V6355D format palette (16 colors × 2 bytes each)
v6355_pal:      times 32 db 0

; Per-scanline analysis workspace
top3_temp:      times 3 db 0    ; Temp: color indices for build_remap_table
nn_best_dist:   dw 0            ; Temp: nearest-neighbor best distance
nn_best_cga:    db 0            ; Temp: nearest-neighbor best CGA value

; Global image analysis
global_pixel_count: times 32 db 0  ; 16 words: total pixel counts per color
global_color_a: db 0               ; BMP index of most common global color
global_color_b: db 0               ; BMP index of 2nd most common global color

; Per-scanline chosen colors (no longer uses top3 — full counts saved instead)
; scanline_top3 removed — replaced by scanline_counts below

; Per-scanline hero color index (200 bytes)
scanline_hero:  times 200 db 0

; Color frequency workspace (16 words = 32 bytes)
color_count:    times 32 db 0

; Pixel remap table (16 bytes, rebuilt per scanline during Pass 2)
remap_table:    times 16 db 0

; Per-line stream skip table (200 bytes): 0 = skip, 2 = write
stream_len:     times 200 db 0

; Pre-computed base distances: min(dist_to_black, dist_to_globalA, dist_to_globalB)
; for each of 16 colors. Used by build_palette_stream.
base_dist:      times 32 db 0   ; 16 words (16 colors × 2 bytes)

; Temp variables for build_heroes_freq (method 1)
bhf_best_count:  dw 0            ; Best pixel count found
bhf_best_idx:    db 0            ; Best hero index found

; Temp variables for find_global_colors (error-weighted 2nd global)
fgc_count:       dw 0            ; Pixel count being evaluated
fgc_dist:        dw 0            ; Min distance to available
fgc_best_lo:     dw 0            ; Best score low word
fgc_best_hi:     dw 0            ; Best score high word

; Temp variables for build_palette_stream (total-error hero selection)
bps_row:         dw 0            ; Current row being evaluated
bps_best_err_lo: dw 0            ; Best (lowest) total error, low word
bps_best_err_hi: dw 0            ; Best (lowest) total error, high word
bps_best_idx:    db 0            ; BMP palette index of best hero
bps_candidate:   db 0            ; Current hero candidate index
bps_total_lo:    dw 0            ; Running total error, low word
bps_total_hi:    dw 0            ; Running total error, high word
bps_hero_w:      dw 0            ; Hero candidate as word (for compute_dist SI)
bps_color:       db 0            ; Color being evaluated in inner loop
bps_eval_count:  dw 0            ; Pixel count of color being evaluated
bps_counts_ptr:  dw 0            ; Pointer to current row's counts

; Per-scanline full color counts (for error-minimization hero selection)
; 200 lines × 16 colors × 2 bytes/count = 6400 bytes
; Saved during analyze_scanline, used by build_palette_stream
scanline_counts: times 6400 db 0

; ============================================================================
; Palette stream for render loop
; 200 lines × 2 bytes per line = 400 bytes
; ============================================================================
; Each line: entry 2 R byte, entry 2 G|B byte (per-scanline hero color)
; Entries 0-1 are always black (hardcoded in render_frame).
; Entries 4,6 are global colors (set once by program_display_palette).
; ============================================================================
palette_stream: times 400 db 0

; ============================================================================
; Per-method stored data (3 methods × (200 + 400 + 200) = 2400 bytes)
; ============================================================================
; Method 1: frequency-based hero (BMP5-style)
sh_method1:     times 200 db 0  ; scanline_hero for method 1
ps_method1:     times 400 db 0  ; palette_stream for method 1
sl_method1:     times 200 db 0  ; stream_len for method 1

; Method 2: individual-need scoring (BMP7-style)
sh_method2:     times 200 db 0
ps_method2:     times 400 db 0
sl_method2:     times 200 db 0

; Method 3: total-error minimization
sh_method3:     times 200 db 0
ps_method3:     times 400 db 0
sl_method3:     times 200 db 0

; File I/O buffers
bmp_header:     times 128 db 0  ; BMP file header + info header + palette
row_buffer:     times 164 db 0  ; BMP row input (160 bytes + safety margin)

; ============================================================================
; END OF PROGRAM
; ============================================================================
