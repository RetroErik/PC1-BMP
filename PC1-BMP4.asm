; ============================================================================
; PC1-BMP4.ASM (v 4.3) - BMP Viewer: Flip-First Technique
; ============================================================================
;
; Displays 320x200 4-bit or 8-bit BMP images on the Olivetti Prodest PC1
; using CGA 320x200x4 mode with per-scanline V6355D palette reprogramming.
; 8-bit BMPs allow up to 256 palette entries, accessing the full V6355D
; 512-color space (3 bits per R/G/B channel = 8×8×8 = 512 colors).
;
; The key innovation: use CGA palette flip (port 0xD9 bit 5) to alternate
; between two palettes each scanline. Combined with per-scanline palette
; reprogramming, this gives 3 INDEPENDENT colors per scanline.
;
; v4.0: FLIP-FIRST
;   The palette flip is the VERY FIRST instruction after HBLANK detection,
;   calibrated at nanosecond precision. The flip reveals colors that were
;   pre-loaded into inactive entries during the PREVIOUS HBLANK. All
;   subsequent palette writes target only INACTIVE entries — pre-loading
;   for the NEXT same-parity line (N+2).
;
;   Result: near-zero flicker. The only visible artifact is the first
;   scanline at the top of the screen. The rest of the image is stable.
;   This is the best visual quality achieved with the flip-first technique.
;
; v4.3: C64-STYLE LOADING + EXIT INFO + BG AUTO-DETECT
;   - ANSI-colored splash screen with loading message.
;   - Border color cycling during Pass 1 and Pass 2, matching the
;     classic Commodore 64 loading screen effect. White flash on done.
;   - On exit (ESC): displays image statistics in text mode —
;     BMP format, dimensions, bit depth, palette capacity, and
;     number of unique colors actually used in the image.
;   - Auto-detects darkest palette entry as background — BMPs where
;     index 0 is not black now display correctly.
;   - Unsigned distance fix (jnc) for correct nearest-color matching.
;
; COMPARISON WITH OTHER TECHNIQUES:
;   BMP4 (this): 3 colors/line, near-zero flicker (flip-first), best color
;   BMP8 (Hero): 2 globals + 1 hero/line, zero flicker, fewer colors
;   BMP9 (Flip Hero): 2 globals + 1 hero/line, zero flicker, flip-first
;
; ============================================================================
; THE TECHNIQUE (flip-first + inactive-entry pre-loading)
; ============================================================================
;
;   CGA mode 4: 2 bits per pixel, values 0-3.
;   Per-scanline palette select flip via 0xD9 (alternating 0x00 / 0x20).
;
;   Palette 0 (even lines): pixel values map to entries {0, 2, 4, 6}
;   Palette 1 (odd lines):  pixel values map to entries {0, 3, 5, 7}
;
;   PER-SCANLINE FLOW (Flip-First):
;
;     1. HBLANK start: IMMEDIATELY flip palette (1 OUT to 0xD9)
;        → Active palette now displays colors pre-loaded during the
;          previous HBLANK into then-inactive entries. The flip is
;          the nanosecond-critical operation; everything else follows.
;
;     2. Remaining HBLANK: stream entries E2-E7 via OUTSB
;        (open at 0x44 + 12×OUTSB + close = ~132 cycles).
;        E0-E1 (always black) are skipped by starting at 0x44.
;        Now-ACTIVE entries get same-value passthrough rewrites
;        (harmless during HBLANK — not visible to the viewer).
;        Now-INACTIVE entries get line N+2's colors (pre-loading
;        for the next same-parity flip, 2 HBLANKs ahead).
;
;   WHY N+2, NOT N+1:
;     In flip-first mode, after flipping on line N, the inactive entries
;     won't display until the NEXT same-parity line (N+2). Line N+1 uses
;     the OTHER palette, whose entries were pre-loaded by iter N-1.
;     So the stream data must target the same-parity line 2 ahead.
;
;   V6355D SEQUENTIAL CONSTRAINT:
;     To write entry N, all entries 2..N-1 must be written first.
;     Starting at 0x44 (entry 2), we stream E2-E7. Active entries
;     receive same-value passthrough rewrites to satisfy the sequential
;     constraint. Since the flip already happened, these rewrites
;     occur during HBLANK and are invisible.
;
;   INTERLEAVING PATTERN (Flip-First):
;
;     Even line N (just flipped to pal 1 — entries 3,5,7 now active):
;       E2 = line N+2 A   E3 = line N+1 A  (inactive preload / active pass)
;       E4 = line N+2 B   E5 = line N+1 B
;       E6 = line N+2 C   E7 = line N+1 C
;
;     Odd line N (just flipped to pal 0 — entries 2,4,6 now active):
;       E2 = line N+1 A   E3 = line N+2 A  (active pass / inactive preload)
;       E4 = line N+1 B   E5 = line N+2 B
;       E6 = line N+1 C   E7 = line N+2 C
;
;   SKIP OPTIMIZATION:
;     Lines where scanline_top3[N+2] == scanline_top3[N] (all 3 colors
;     match across the 2-line gap) get ZERO palette writes — just the
;     flip. The inactive entries already hold the correct values from
;     2 HBLANKs ago.
;
; ============================================================================
; COLOR SELECTION (3 independent per scanline + stability ordering)
; ============================================================================
;
;   Each scanline independently picks its top 3 most frequent non-background
;   BMP palette colors. Colors are then REORDERED by global stability:
;
;   CGA value mapping (sorted by volatility, not frequency):
;     0 = black (background, entry 0)
;     1 = Color A (most volatile — entries 2/3, changes most often)
;     2 = Color B (medium stability — entries 4/5)
;     3 = Color C (most stable — entries 6/7, changes least often)
;
;   STABILITY ORDERING: After per-line frequency analysis, a global
;   pass counts how many scanlines each BMP palette index appears on.
;   The most globally common color → slot C (entries 6/7), 2nd → slot B.
;   This maximizes skip optimization: if C doesn't change between
;   same-parity lines (N vs N+2), we can skip those entries.
;
;   Remaining BMP colors are remapped to the nearest of
;   {black, Color A, Color B, Color C} using RGB888 Manhattan distance.
;
; ============================================================================
; ALGORITHM
; ============================================================================
;
;   1. Load and validate 320x200 4-bit or 8-bit BMP
;   2. Convert BMP palette (16 or 256 colors) to RGB888 + V6355D format
;   2b. AUTO-DETECT BACKGROUND: find darkest palette entry (min R+G+B),
;       use as background index, force to black in RGB888 + V6355D
;   3. PASS 1: For each scanline, count pixel color frequency,
;      pick top 3 most frequent non-background colors
;   3b. STABILITY REORDER: Count global color frequency across all
;       lines, reorder each line's top3 so most stable → slot C
;       (entries 6/7), most volatile → slot A (entries 2/3)
;   4. Build palette stream: interleaved V6355D data for entries 2-7
;      (12 bytes per line = 2400 bytes total). Pre-loads inactive
;      entries with line N+2 colors (next same-parity line).
;   4b. BUILD STREAM LENGTHS: Compare scanline_top3[N+2] vs [N]
;       (same-parity gap). Skip if all 3 colors match (0 or 12 bytes).
;   5. PASS 2: Re-read BMP, remap each pixel to CGA value 0-3
;      using nearest-color matching, pack into 2bpp CGA VRAM
;   6. Program initial palette: entries for lines 0 and 1
;   7. Enter display loop: VSYNC wait + per-scanline HSYNC-synced
;      FLIP-FIRST + OUTSB entry 2-7 streaming to inactive entries
;
; ============================================================================
; KNOWN ARTIFACT — FIRST SCANLINE
; ============================================================================
;
;   The flip-first approach eliminates virtually all visible flicker.
;   The only remaining artifact is on the first scanline at the top of
;   the screen, where the initial palette state may not perfectly match
;   the pre-load expectations. The rest of the 200-line image displays
;   with stable, flicker-free colors.
;
;   This is the best visual quality achieved with the flip-first technique
;   on real Olivetti Prodest PC1 hardware.
;
; ============================================================================
; LIMITATIONS
; ============================================================================
;
;   - Only 320x200 4-bit or 8-bit uncompressed BMPs supported
;   - 8-bit BMPs allow up to 256 palette colors from V6355D's 512-color space
;   - Darkest palette entry auto-detected as background (forced to black)
;   - 3 independent colors per scanline + black = 4 values per line
;   - Images with many distinct colors per line will have reduced quality
;   - Minor first-scanline artifact (only artifact remaining)
;   - Palette writes extend ~118 cycles past HBLANK into visible area
;     but target INACTIVE entries only — invisible to the viewer
;   - Uses INT 10h mode 4 (resets V6355D — not PERITEL compatible)
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
;   nasm -f bin -o PC1-BMP4.com PC1-BMP4.asm
;
; ============================================================================
; USAGE
; ============================================================================
;
;   PC1-BMP4 filename.bmp
;
;   Controls:
;     ESC : Exit to DOS (shows image info)
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
BMP_ROW_4BPP    equ 160         ; 320 pixels at 4bpp (2 pixels/byte)
BMP_ROW_8BPP    equ 320         ; 320 pixels at 8bpp (1 pixel/byte)

; CGA mode 4 control values (port 0xD8)
CGA_MODE4_OFF   equ 0x02        ; 320x200 graphics, video OFF
CGA_MODE4_ON    equ 0x0A        ; 320x200 graphics, video ON

; Palette select values (port 0xD9)
PAL_EVEN        equ 0x00        ; Palette 0: entries {0,2,4,6}, bg = entry 0
PAL_ODD         equ 0x20        ; Palette 1: entries {0,3,5,7}, bg = entry 0

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

    ; --- Read header + palette (up to 1078 bytes) ---
    ; 14 (file hdr) + 40 (info hdr) + up to 1024 (8-bit palette) = 1078
    mov bx, ax
    mov dx, bmp_header
    mov cx, 1078
    mov ah, 0x3F                ; DOS Read File
    int 0x21
    jc .file_error
    cmp ax, 54                  ; Need at least file+info headers
    jb .file_error

    ; --- Validate BMP ---
    cmp word [bmp_header + BMP_SIGNATURE], 0x4D42   ; 'BM'
    jne .not_bmp

    ; --- Detect BPP and set format parameters ---
    mov ax, [bmp_header + BMP_BPP]
    cmp ax, 4
    je .bpp_4
    cmp ax, 8
    je .bpp_8
    jmp .wrong_format

.bpp_4:
    mov byte [bmp_bpp], 4
    mov word [num_colors], 16
    mov word [bmp_row_bytes], BMP_ROW_4BPP
    jmp .bpp_done

.bpp_8:
    mov byte [bmp_bpp], 8
    mov word [num_colors], 256
    mov word [bmp_row_bytes], BMP_ROW_8BPP

.bpp_done:
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

    ; --- Auto-detect background color (darkest palette entry) ---
    call find_bg_index

    ; --- Show splash screen centered on screen ---
    mov ax, 0x0003              ; Clear text mode screen
    int 0x10
    mov dx, msg_splash
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
    ; Count pixel colors per scanline, find top 3 per line.
    ; No global color accumulation — each line is independent.
    call analyze_image

    ; --- Reorder colors by stability (most stable → high entries) ---
    call reorder_by_stability

    ; --- Build per-scanline palette stream ---
    ; Interleaved V6355D data for entries 2-7 (12 bytes/line = 2400).
    call build_palette_stream

    ; --- Build per-line stream length table ---
    call build_stream_lengths

    ; --- Count skipped lines for exit info ---
    call count_skipped_lines

    ; --- Count unique colors for exit info ---
    call count_unique_colors

    ; --- Pass 2: Render ---

    ; --- Set CGA mode 4 (320x200x4, clears screen) ---
    mov ax, 0x0004
    int 0x10
    cld                         ; BIOS may set DF — ensure forward string ops

    ; --- Blank video during VRAM write (faster, like PC1-BMP.asm) ---
    mov al, CGA_MODE4_OFF
    out PORT_MODE, al

    ; --- Reset border cycling for Pass 2 ---
    mov byte [border_ctr], 0

    ; --- Seek back to pixel data for Pass 2 ---
    mov bx, [file_handle]
    mov dx, [bmp_header + BMP_DATA_OFFSET]
    mov cx, [bmp_header + BMP_DATA_OFFSET + 2]
    mov ax, 0x4200
    int 0x21
    ; (Not checking error — same seek worked before)

    ; --- PASS 2: Remap pixels and write to CGA VRAM ---
    call render_to_vram

    ; --- Close file (no longer needed) ---
    mov bx, [file_handle]
    mov ah, 0x3E                ; DOS Close File
    int 0x21

    ; --- Program initial palette ---
    call program_initial_palette

    ; --- Enable video — image appears instantly ---
    mov al, PAL_EVEN
    out PORT_COLOR, al

    mov al, CGA_MODE4_ON
    out PORT_MODE, al

    ; --- Display loop ---

.display_loop:
    call wait_vblank
    call render_frame
    call check_keyboard
    cmp al, 0xFF
    jne .display_loop

    ; --- Exit: show image info in text mode ---
    call set_cga_palette
    mov ax, 0x0003              ; Restore text mode
    int 0x10
    call show_exit_info
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
; Converts BMP palette (BGRA, up to 256 colors at bmp_header+54) into:
;   - pal_r[], pal_g[], pal_b[]: RGB888 components (for distance calc)
;   - v6355_pal[]: V6355D format (2 bytes per color, for palette stream)
; ============================================================================
convert_bmp_palette:
    push ax
    push bx
    push cx
    push si
    push di

    mov si, bmp_header + BMP_PALETTE_OFF
    xor di, di                  ; Palette index 0..num_colors-1
    mov cx, [num_colors]

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
; find_bg_index - Auto-detect darkest palette entry as background
; ============================================================================
; Scans all palette entries to find the one closest to black (min R+G+B).
; This allows BMPs where index 0 is not black to display correctly.
; The detected background entry is forced to pure black (0,0,0) in both
; RGB888 and V6355D formats, ensuring CGA entry 0 always renders as black.
; Result stored in bg_index.
; ============================================================================
find_bg_index:
    push ax
    push bx
    push cx
    push dx
    push si

    xor ax, ax                  ; Best index = 0
    mov dx, 0xFFFF              ; Best sum = max (higher = brighter)
    xor bx, bx                  ; Current palette index
    mov cx, [num_colors]        ; 16 or 256

.fbg_loop:
    ; Compute brightness: R + G + B
    xor si, si
    push ax
    xor ah, ah
    mov al, [pal_r + bx]
    add si, ax
    mov al, [pal_g + bx]
    add si, ax
    mov al, [pal_b + bx]
    add si, ax
    pop ax

    cmp si, dx
    jae .fbg_not_darker
    mov dx, si                  ; New darkest sum
    mov ax, bx                  ; New best index

.fbg_not_darker:
    inc bx
    loop .fbg_loop

    ; AL = darkest palette index
    mov [bg_index], al

    ; Force bg_index's palette to pure black (0,0,0)
    xor bh, bh
    mov bl, al
    mov byte [pal_r + bx], 0
    mov byte [pal_g + bx], 0
    mov byte [pal_b + bx], 0
    shl bx, 1                  ; Word offset for v6355_pal
    mov word [v6355_pal + bx], 0

    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; analyze_image - Pass 1: Read all BMP rows, find top 3 colors per scanline
; ============================================================================
; BMP is bottom-up: first row read from file = screen row 199.
; Each scanline is analyzed independently — no global color accumulation.
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
    cmp ax, [bmp_row_bytes]
    jb .analyze_done

    call analyze_scanline       ; Process row_buffer for [current_row]

    ; --- C64-style border cycling during analysis ---
    push ax
    mov al, [border_ctr]
    out PORT_COLOR, al
    inc byte [border_ctr]
    and byte [border_ctr], 0x0F
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
; ============================================================================
; Input: [current_row], row_buffer filled with BMP pixel data
; Output: scanline_top3 updated for this row
;
; Each scanline is independent — no global accumulation.
; ============================================================================
analyze_scanline:
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    ; --- Clear color counts (256 words = 512 bytes) ---
    ; NOTE: Uses explicit DS:DI loop instead of REP STOSW (which needs ES:DI)
    ; to avoid crash if ES was clobbered by DOS INT 21h
    mov di, color_count
    mov cx, 256
.clr_counts:
    mov word [di], 0
    add di, 2
    loop .clr_counts

    ; --- Count pixel colors (320 pixels) ---
    mov si, row_buffer
    cmp byte [bmp_bpp], 8
    je .count_8bpp

    ; --- 4bpp: 2 pixels per byte (160 bytes = 320 pixels) ---
    mov cx, BMP_ROW_4BPP

.count_loop_4:
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

    loop .count_loop_4
    jmp .count_done

    ; --- 8bpp: 1 pixel per byte (320 bytes = 320 pixels) ---
.count_8bpp:
    mov cx, BMP_ROW_8BPP

.count_loop_8:
    lodsb                       ; AL = palette index (0-255)
    xor bx, bx
    mov bl, al
    shl bx, 1                  ; Word index
    inc word [color_count + bx]

    loop .count_loop_8

.count_done:

    ; --- Exclude background index (darkest palette entry) ---
    xor bx, bx
    mov bl, [bg_index]
    shl bx, 1                  ; Word offset
    mov word [color_count + bx], 0

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

    ; --- Store top3 indices for this scanline ---
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

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; find_max_color - Find palette index with highest pixel count
; Returns: AL = best index (bg_index if no non-zero counts remain)
; ============================================================================
find_max_color:
    push bx
    push cx
    push dx

    xor ax, ax
    mov al, [bg_index]          ; Best index = bg_index (sentinel)
    xor dx, dx                  ; Best count = 0
    xor bx, bx                  ; Start at color_count[0] (word offset)
    mov cx, [num_colors]        ; Check all palette entries

.fmc_loop:
    cmp [color_count + bx], dx
    jbe .fmc_not_better
    mov dx, [color_count + bx]
    mov ax, bx
    shr ax, 1                  ; Convert word offset → palette index

.fmc_not_better:
    add bx, 2
    loop .fmc_loop

    ; AL = best index (bg_index if none found)
    pop dx
    pop cx
    pop bx
    ret

; ============================================================================
; reorder_by_stability - Sort top3 colors: most stable → high entries
; ============================================================================
; Counts how many scanlines each BMP palette index appears across
; all 200 lines. The two most globally common colors are assigned to
; the highest palette entries (C→6/7, B→4/5) to maximize truncation
; opportunities in the adaptive streaming render loop.
;
; Colors that appear on many lines tend to stay constant across
; consecutive scanlines. By placing them in entries 6/7 and 4/5,
; the render loop can truncate the OUTSB stream early when those
; entries don't change — reducing visible-area writes and blinking.
;
; Modifies: scanline_top3[] (reordered in-place)
; Sets: global_c, global_b
; ============================================================================
reorder_by_stability:
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    ; --- Clear line presence count (256 words) ---
    mov di, color_line_count
    mov cx, 256
.rbs_clr:
    mov word [di], 0
    add di, 2
    loop .rbs_clr

    ; --- Count lines where each color index appears ---
    mov si, scanline_top3
    mov cx, 200
.rbs_count:
    ; 3 entries per line (skip bg_index = empty sentinel)
    lodsb                       ; Slot A
    cmp al, [bg_index]
    je .rbs_skip1
    xor bx, bx
    mov bl, al
    shl bx, 1
    inc word [color_line_count + bx]
.rbs_skip1:
    lodsb                       ; Slot B
    cmp al, [bg_index]
    je .rbs_skip2
    xor bx, bx
    mov bl, al
    shl bx, 1
    inc word [color_line_count + bx]
.rbs_skip2:
    lodsb                       ; Slot C
    cmp al, [bg_index]
    je .rbs_skip3
    xor bx, bx
    mov bl, al
    shl bx, 1
    inc word [color_line_count + bx]
.rbs_skip3:
    loop .rbs_count

    ; --- Find most common color → global_c (entries 6/7) ---
    xor ax, ax
    mov al, [bg_index]          ; Best index = bg_index (sentinel)
    xor dx, dx                  ; Best count = 0
    xor bx, bx                  ; Start at index 0
    mov cx, [num_colors]        ; Check all palette entries
.rbs_find1:
    cmp [color_line_count + bx], dx
    jbe .rbs_f1_skip
    mov dx, [color_line_count + bx]
    mov ax, bx
    shr ax, 1                  ; Convert word offset → index
.rbs_f1_skip:
    add bx, 2
    loop .rbs_find1

    mov [global_c], al

    ; Zero out its count for finding 2nd
    cmp al, [bg_index]
    je .rbs_no_reorder          ; No colors found at all
    xor bx, bx
    mov bl, al
    shl bx, 1
    mov word [color_line_count + bx], 0

    ; --- Find 2nd most common → global_b (entries 4/5) ---
    xor ax, ax
    mov al, [bg_index]          ; Sentinel
    xor dx, dx
    xor bx, bx
    mov cx, [num_colors]
.rbs_find2:
    cmp [color_line_count + bx], dx
    jbe .rbs_f2_skip
    mov dx, [color_line_count + bx]
    mov ax, bx
    shr ax, 1
.rbs_f2_skip:
    add bx, 2
    loop .rbs_find2

    mov [global_b], al

    ; --- Reorder each line's top3 ---
    ; For each line: move global_c to slot 2, global_b to slot 1
    mov si, scanline_top3
    mov cx, 200
.rbs_reorder:
    ; Read current top3 into registers
    mov al, [si]                ; Slot 0 (A)
    mov ah, [si + 1]            ; Slot 1 (B)
    mov dl, [si + 2]            ; Slot 2 (C)

    ; --- Place global_c into slot 2 ---
    mov dh, [global_c]
    cmp dl, dh
    je .rbs_check_b             ; Already in slot 2
    cmp al, dh
    je .rbs_c_in_0
    cmp ah, dh
    je .rbs_c_in_1
    jmp .rbs_check_b            ; global_c not in this line's top3

.rbs_c_in_0:
    xchg al, dl                 ; Swap slot 0 ↔ slot 2
    jmp .rbs_check_b

.rbs_c_in_1:
    xchg ah, dl                 ; Swap slot 1 ↔ slot 2

.rbs_check_b:
    ; --- Place global_b into slot 1 ---
    mov dh, [global_b]
    cmp dh, [bg_index]
    je .rbs_store               ; No global_b
    cmp ah, dh
    je .rbs_store               ; Already in slot 1
    cmp al, dh
    jne .rbs_store              ; global_b not in slot 0

    xchg al, ah                 ; Swap slot 0 ↔ slot 1

.rbs_store:
    mov [si], al
    mov [si + 1], ah
    mov [si + 2], dl

    add si, 3
    loop .rbs_reorder

.rbs_no_reorder:
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; build_palette_stream - Build FLIP-FIRST interleaved V6355D data (E2-E7)
; ============================================================================
; Uses scanline_top3[] to build 12 bytes per line (entries 2-7 × 2 bytes).
;
; FLIP-FIRST INTERLEAVING:
;
;   After HBLANK for even line N (just flipped to pal 1):
;     E2 = line N+2 color A  (inactive, pre-load for next even line)
;     E3 = line N+1 color A  (active, same-value passthrough)
;     E4 = line N+2 color B  (inactive, pre-load)
;     E5 = line N+1 color B  (active, passthrough)
;     E6 = line N+2 color C  (inactive, pre-load)
;     E7 = line N+1 color C  (active, passthrough)
;
;   After HBLANK for odd line N (just flipped to pal 0):
;     E2 = line N+1 color A  (active, same-value passthrough)
;     E3 = line N+2 color A  (inactive, pre-load for next odd line)
;     E4 = line N+1 color B  (active, passthrough)
;     E5 = line N+2 color B  (inactive, pre-load)
;     E6 = line N+1 color C  (active, passthrough)
;     E7 = line N+2 color C  (inactive, pre-load)
;
; Total: 200 lines × 12 bytes = 2400 bytes.
; Target lines clamped to 199.
; ============================================================================
build_palette_stream:
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    xor cx, cx                  ; CX = current line (0-199)
    mov di, palette_stream

.bps_loop:
    ; (E0-E1 black zeros are set in program_initial_palette — not in stream)

    ; --- Read "far" line's top 3 (line N+2, the INACTIVE pre-load target) ---
    mov bx, cx
    add bx, 2
    cmp bx, 200
    jb .bps_far_ok
    sub bx, 200                 ; Wrap: 200→0, 201→1 (for next frame)
.bps_far_ok:
    mov ax, bx
    shl ax, 1
    add bx, ax                  ; BX = far_line * 3

    mov al, [scanline_top3 + bx]
    mov [bps_cur_a], al
    mov al, [scanline_top3 + bx + 1]
    mov [bps_cur_b], al
    mov al, [scanline_top3 + bx + 2]
    mov [bps_cur_c], al

    ; --- Read "near" line's top 3 (line N+1, same-value passthrough) ---
    mov bx, cx
    inc bx
    cmp bx, 200
    jb .bps_near_ok
    sub bx, 200                 ; Wrap: 200→0 (for next frame)
.bps_near_ok:
    mov ax, bx
    shl ax, 1
    add bx, ax                  ; BX = near_line * 3

    mov al, [scanline_top3 + bx]
    mov [bps_nxt_a], al
    mov al, [scanline_top3 + bx + 1]
    mov [bps_nxt_b], al
    mov al, [scanline_top3 + bx + 2]
    mov [bps_nxt_c], al

    test cl, 1
    jnz .bps_odd

    ; =================================================================
    ; EVEN line N: just flipped to pal 1 (E3,E5,E7 active)
    ; Inactive (E2,E4,E6) get line N+2 colors (pre-load for next even)
    ; Active (E3,E5,E7) get line N+1 colors (same-value passthrough)
    ; =================================================================

    ; E2 = line N+2 Color A (inactive, pre-load)
    xor bx, bx
    mov bl, [bps_cur_a]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di], ax

    ; E3 = line N+1 Color A (active, passthrough)
    xor bx, bx
    mov bl, [bps_nxt_a]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 2], ax

    ; E4 = line N+2 Color B (inactive, pre-load)
    xor bx, bx
    mov bl, [bps_cur_b]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 4], ax

    ; E5 = line N+1 Color B (active, passthrough)
    xor bx, bx
    mov bl, [bps_nxt_b]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 6], ax

    ; E6 = line N+2 Color C (inactive, pre-load)
    xor bx, bx
    mov bl, [bps_cur_c]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 8], ax

    ; E7 = line N+1 Color C (active, passthrough)
    xor bx, bx
    mov bl, [bps_nxt_c]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 10], ax

    jmp .bps_next

.bps_odd:
    ; =================================================================
    ; ODD line N: just flipped to pal 0 (E2,E4,E6 active)
    ; Inactive (E3,E5,E7) get line N+2 colors (pre-load for next odd)
    ; Active (E2,E4,E6) get line N+1 colors (same-value passthrough)
    ; =================================================================

    ; E2 = line N+1 Color A (active, passthrough)
    xor bx, bx
    mov bl, [bps_nxt_a]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di], ax

    ; E3 = line N+2 Color A (inactive, pre-load)
    xor bx, bx
    mov bl, [bps_cur_a]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 2], ax

    ; E4 = line N+1 Color B (active, passthrough)
    xor bx, bx
    mov bl, [bps_nxt_b]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 4], ax

    ; E5 = line N+2 Color B (inactive, pre-load)
    xor bx, bx
    mov bl, [bps_cur_b]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 6], ax

    ; E6 = line N+1 Color C (active, passthrough)
    xor bx, bx
    mov bl, [bps_nxt_c]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 8], ax

    ; E7 = line N+2 Color C (inactive, pre-load)
    xor bx, bx
    mov bl, [bps_cur_c]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 10], ax

.bps_next:
    add di, 12
    inc cx
    cmp cx, 200
    jb .bps_loop

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; build_stream_lengths - Determine per-line skip/write flag (flip-first)
; ============================================================================
; Compares scanline_top3[N+2] vs scanline_top3[N] (same-parity gap).
; If the 3 colors are identical → stream_len = 0 (skip: inactive entries
; already hold the correct values from 2 HBLANKs ago).
; Otherwise → stream_len = 12 (full write: all entries E2-E7).
;
; Line 0 always gets stream_len = 12 (must write initial state).
;
; Output: stream_len[200] filled with values 0 or 12
; ============================================================================
build_stream_lengths:
    push ax
    push bx
    push cx
    push si
    push di

    ; Line 0: always full
    mov byte [stream_len], 12

    ; Lines 1-199: compare top3[N+2] vs top3[N] (same-parity skip)
    mov si, 1                       ; SI = current line index

.bsl_loop:
    ; Get top3 pointer for line N (SI)
    mov bx, si
    mov ax, bx
    shl ax, 1
    add bx, ax                      ; BX = SI * 3 (line N offset)

    ; Get top3 pointer for line N+2 (wraps around for frame continuity)
    mov di, si
    add di, 2
    cmp di, 200
    jb .bsl_clamp_ok
    sub di, 200                     ; Wrap: 200→0, 201→1
.bsl_clamp_ok:
    mov ax, di
    shl ax, 1
    add di, ax                      ; DI = far_line * 3

    ; Compare all 3 color indices
    mov al, [scanline_top3 + bx]
    cmp al, [scanline_top3 + di]
    jne .bsl_full
    mov al, [scanline_top3 + bx + 1]
    cmp al, [scanline_top3 + di + 1]
    jne .bsl_full
    mov al, [scanline_top3 + bx + 2]
    cmp al, [scanline_top3 + di + 2]
    jne .bsl_full

    ; All 3 colors match — skip
    mov byte [stream_len + si], 0
    jmp .bsl_next

.bsl_full:
    mov byte [stream_len + si], 12

.bsl_next:
    inc si
    cmp si, 200
    jb .bsl_loop

    pop di
    pop si
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; render_to_vram - Pass 2: Read BMP again, remap pixels, write to CGA VRAM
; ============================================================================
; For each row:
;   1. Read BMP row (4bpp=160 bytes, 8bpp=320 bytes)
;   2. Build remap table (up to 256 entries: BMP index → CGA value 0-3)
;   3. Convert pixels → 2bpp CGA using remap, write to interlaced VRAM
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
    cmp ax, [bmp_row_bytes]
    jb .rv_done

    ; Build remap table for this scanline's 3 color assignment
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

    ; Convert BMP → 2bpp CGA
    ; SI reads from row_buffer (DS:SI), XLAT uses remap_table (DS:BX)
    ; DI writes to CGA VRAM (ES:DI)
    mov si, row_buffer
    mov bx, remap_table
    mov cx, CGA_ROW_BYTES       ; 80 output bytes = 320 pixels

    cmp byte [bmp_bpp], 8
    je .rv_convert_8

    ; --- 4bpp: 2 pixels per byte ---
.rv_convert_4:
    ; Process 4 pixels into 1 CGA byte (reads 2 input bytes)
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

    loop .rv_convert_4
    jmp .rv_convert_done

    ; --- 8bpp: 1 pixel per byte ---
.rv_convert_8:
    ; Process 4 pixels into 1 CGA byte (reads 4 input bytes)
    xor dh, dh                  ; Accumulator

    lodsb                       ; Pixel 0 (→ bits 7-6)
    xlat
    or dh, al
    shl dh, 2

    lodsb                       ; Pixel 1 (→ bits 5-4)
    xlat
    or dh, al
    shl dh, 2

    lodsb                       ; Pixel 2 (→ bits 3-2)
    xlat
    or dh, al
    shl dh, 2

    lodsb                       ; Pixel 3 (→ bits 1-0)
    xlat
    or dh, al

    mov al, dh
    stosb

    loop .rv_convert_8

.rv_convert_done:

    ; --- C64-style border cycling: once per row ---
    push ax
    mov al, [border_ctr]
    out PORT_COLOR, al
    inc byte [border_ctr]
    and byte [border_ctr], 0x0F
    pop ax

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
; build_remap_table - Create lookup: BMP index → CGA value (0-3) (up to 256)
; ============================================================================
; CGA value mapping (3 independent colors per scanline):
;   0 = black (pixel value 0 → entry 0)
;   1 = Color A (most frequent per-line, → entry 2/3)
;   2 = Color B (2nd most frequent, → entry 4/5)
;   3 = Color C (3rd most frequent, → entry 6/7)
;
; For each BMP palette index:
;   Index == bg_index    → CGA 0 (black/background)
;   Index == Color A     → CGA 1
;   Index == Color B     → CGA 2
;   Index == Color C     → CGA 3
;   Otherwise            → nearest of {black, A, B, C} by RGB distance
; ============================================================================
build_remap_table:
    push ax
    push bx
    push cx
    push dx
    push si

    ; Get this scanline's top 3 colors directly from scanline_top3
    mov bx, [current_row]
    mov ax, bx
    shl ax, 1
    add bx, ax                  ; BX = row * 3

    mov al, [scanline_top3 + bx]
    mov [top3_temp], al             ; Color A → CGA 1
    mov al, [scanline_top3 + bx + 1]
    mov [top3_temp + 1], al         ; Color B → CGA 2
    mov al, [scanline_top3 + bx + 2]
    mov [top3_temp + 2], al         ; Color C → CGA 3

    ; Map each BMP index to CGA value 0-3
    xor cx, cx                  ; CX = current palette index

.brt_loop:
    ; Background index → always black (CGA value 0)
    cmp cl, [bg_index]
    je .brt_black

    ; Direct match with chosen colors?
    cmp cl, [top3_temp]         ; Color A?
    je .brt_1
    cmp cl, [top3_temp + 1]     ; Color B?
    je .brt_2
    cmp cl, [top3_temp + 2]     ; Color C?
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
    cmp cx, [num_colors]
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
; Compares against: black (0,0,0), Color A, Color B, Color C
; (stored in top3_temp[0..2] by build_remap_table)
; Skips top3 entries matching bg_index (empty slot = fewer than 3 colors)
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
    cmp al, [bg_index]
    je .fn_done                 ; No colors available at all
    xor ah, ah
    mov si, ax                  ; SI = target index
    call compute_dist_bx_si     ; DX = distance
    cmp dx, [nn_best_dist]
    jae .fn_try_2
    mov [nn_best_dist], dx
    mov byte [nn_best_cga], 1

.fn_try_2:
    mov al, [top3_temp + 1]
    cmp al, [bg_index]
    je .fn_done
    xor ah, ah
    mov si, ax
    call compute_dist_bx_si
    cmp dx, [nn_best_dist]
    jae .fn_try_3
    mov [nn_best_dist], dx
    mov byte [nn_best_cga], 2

.fn_try_3:
    mov al, [top3_temp + 2]
    cmp al, [bg_index]
    je .fn_done
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
    jnc .cd_r_pos               ; CF=0: A >= B, result is correct
    neg al                      ; CF=1: A < B, negate to get |A-B|
.cd_r_pos:
    xor ah, ah
    add dx, ax

    ; |G[bx] - G[si]|
    mov al, [pal_g + bx]
    sub al, [pal_g + si]
    jnc .cd_g_pos
    neg al
.cd_g_pos:
    xor ah, ah
    add dx, ax

    ; |B[bx] - B[si]|
    mov al, [pal_b + bx]
    sub al, [pal_b + si]
    jnc .cd_b_pos
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
    mov cx, [bmp_row_bytes]     ; 160 or 320 bytes depending on BPP
    mov ah, 0x3F                ; DOS Read File
    int 0x21

    pop es                      ; Restore ES
    pop dx
    pop cx
    pop bx
    ret

; ============================================================================
; render_frame - FLIP-FIRST adaptive per-scanline HBLANK streaming
; ============================================================================
; FLIP-FIRST with SKIP OPTIMIZATION (V40/80186+):
;
;   The flip is the FIRST thing after HBLANK detection.
;   This reveals the pre-loaded inactive entries
;   instantly with nanosecond precision. Everything after targets
;   the NOW-INACTIVE entries (pre-loading for N+2, next same-parity).
;
;   For each scanline, checks stream_len[] (0 or 12):
;     stream_len = 12: Full write (flip, then 12×OUTSB to inactive)
;     stream_len = 0:  SKIP — flip only, no palette writes needed
;
;   Skip condition: scanline_top3[N+2] == scanline_top3[N]. The
;   inactive entries already contain the correct colors from 2
;   HBLANKs ago.
;
;   The palette write targets only INACTIVE entries. Active entries
;   get same-value passthrough (required by V6355D sequential write
;   constraint, harmless during HBLANK).
;
; Uses BP to index stream_len[]. In COM file, SS=DS so [BP] is safe.
;
; Data: palette_stream[200 × 12], stream_len[200]
; ============================================================================
render_frame:
    cli
    cld                         ; Ensure OUTSB increments SI
    push bp

    mov si, palette_stream
    mov cx, SCREEN_HEIGHT
    mov dx, PORT_REG_DATA       ; DX = 0xDE (stays constant for OUTSB)
    mov bl, PAL_ODD             ; After line 0's stream, flip to ODD for line 1
    mov bh, PAL_EVEN

    ; ------------------------------------------------------------------
    ; HSYNC-synchronized loop with skip optimization
    ; ------------------------------------------------------------------
    mov bp, stream_len          ; BP → stream length table

.rf_scanline:
.rf_wait_low:
    in al, PORT_STATUS
    test al, 0x01
    jnz .rf_wait_low            ; Wait while in retrace

.rf_wait_high:
    in al, PORT_STATUS
    test al, 0x01
    jz .rf_wait_high            ; Wait for HBLANK start

    ; === FLIP FIRST — nanosecond-critical ===
    ; Reveals pre-loaded inactive entries instantly.
    mov al, bl
    out PORT_COLOR, al
    xchg bl, bh

    ; === Now write INACTIVE entries (pre-load for N+2) ===
    cmp byte [bp], 0
    je .rf_skip
    inc bp

    ; === Full write: open palette at entry 2, stream E2-E7 ===
    mov al, 0x44
    out PORT_REG_ADDR, al       ; Open palette at entry 2 (skip E0-E1)

    outsb                       ; E2 R
    outsb                       ; E2 GB
    outsb                       ; E3 R
    outsb                       ; E3 GB
    outsb                       ; E4 R
    outsb                       ; E4 GB
    outsb                       ; E5 R
    outsb                       ; E5 GB
    outsb                       ; E6 R
    outsb                       ; E6 GB
    outsb                       ; E7 R
    outsb                       ; E7 GB

    mov al, 0x80
    out PORT_REG_ADDR, al       ; Close palette
    jmp .rf_continue

.rf_skip:
    ; === No palette writes needed — flip was enough ===
    inc bp
    add si, 12                  ; Advance past this line's stream data

.rf_continue:
    loop .rf_scanline

.rf_done:
    ; Reset to palette 0, clean state
    mov al, PAL_EVEN
    out PORT_COLOR, al

    pop bp
    sti
    ret

; ============================================================================
; program_initial_palette - Set V6355D entries for first display frame
; ============================================================================
; Called once before enabling video. Sets all 8 entries from scanline_top3:
;   Entry 0 = black (bg/border)
;   Entry 1 = black (unused)
;   Entry 2 = line 0 Color A  (even palette: pixel 1)
;   Entry 3 = line 1 Color A  (odd palette: pixel 1)
;   Entry 4 = line 0 Color B  (even palette: pixel 2)
;   Entry 5 = line 1 Color B  (odd palette: pixel 2)
;   Entry 6 = line 0 Color C  (even palette: pixel 3)
;   Entry 7 = line 1 Color C  (odd palette: pixel 3)
;
; Since video is OFF at this point, no timing concerns.
; ============================================================================
program_initial_palette:
    push ax
    push bx
    push cx
    push si

    cli

    mov al, 0x40
    out PORT_REG_ADDR, al       ; Open palette at entry 0
    jmp short $+2

    ; Entries 0-1: black (4 zero bytes)
    xor al, al
    out PORT_REG_DATA, al       ; E0 R
    jmp short $+2
    out PORT_REG_DATA, al       ; E0 GB
    jmp short $+2
    out PORT_REG_DATA, al       ; E1 R
    jmp short $+2
    out PORT_REG_DATA, al       ; E1 GB
    jmp short $+2

    ; Entry 2 = line 0, Color A (scanline_top3[0])
    xor bx, bx
    mov bl, [scanline_top3]     ; Line 0 color A
    shl bx, 1
    mov al, [v6355_pal + bx]
    out PORT_REG_DATA, al       ; E2 R
    jmp short $+2
    mov al, [v6355_pal + bx + 1]
    out PORT_REG_DATA, al       ; E2 GB
    jmp short $+2

    ; Entry 3 = line 1, Color A (scanline_top3[3])
    xor bx, bx
    mov bl, [scanline_top3 + 3] ; Line 1 color A
    shl bx, 1
    mov al, [v6355_pal + bx]
    out PORT_REG_DATA, al       ; E3 R
    jmp short $+2
    mov al, [v6355_pal + bx + 1]
    out PORT_REG_DATA, al       ; E3 GB
    jmp short $+2

    ; Entry 4 = line 0, Color B (scanline_top3[1])
    xor bx, bx
    mov bl, [scanline_top3 + 1] ; Line 0 color B
    shl bx, 1
    mov al, [v6355_pal + bx]
    out PORT_REG_DATA, al       ; E4 R
    jmp short $+2
    mov al, [v6355_pal + bx + 1]
    out PORT_REG_DATA, al       ; E4 GB
    jmp short $+2

    ; Entry 5 = line 1, Color B (scanline_top3[4])
    xor bx, bx
    mov bl, [scanline_top3 + 4] ; Line 1 color B
    shl bx, 1
    mov al, [v6355_pal + bx]
    out PORT_REG_DATA, al       ; E5 R
    jmp short $+2
    mov al, [v6355_pal + bx + 1]
    out PORT_REG_DATA, al       ; E5 GB
    jmp short $+2

    ; Entry 6 = line 0, Color C (scanline_top3[2])
    xor bx, bx
    mov bl, [scanline_top3 + 2] ; Line 0 color C
    shl bx, 1
    mov al, [v6355_pal + bx]
    out PORT_REG_DATA, al       ; E6 R
    jmp short $+2
    mov al, [v6355_pal + bx + 1]
    out PORT_REG_DATA, al       ; E6 GB
    jmp short $+2

    ; Entry 7 = line 1, Color C (scanline_top3[5])
    xor bx, bx
    mov bl, [scanline_top3 + 5] ; Line 1 color C
    shl bx, 1
    mov al, [v6355_pal + bx]
    out PORT_REG_DATA, al       ; E7 R
    jmp short $+2
    mov al, [v6355_pal + bx + 1]
    out PORT_REG_DATA, al       ; E7 GB
    jmp short $+2

    mov al, 0x80
    out PORT_REG_ADDR, al       ; Close palette

    sti

    pop si
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; wait_vblank
; ============================================================================
wait_vblank:

.wv_wait_end:
    in al, PORT_STATUS
    test al, 0x08
    jnz .wv_wait_end

.wv_wait_start:
    in al, PORT_STATUS
    test al, 0x08
    jz .wv_wait_start

    ret

; ============================================================================
; check_keyboard - Returns AL = 0xFF if ESC pressed, 0 otherwise
; ============================================================================
check_keyboard:
    mov ah, 0x01
    int 0x16
    jz .ck_no_key

    mov ah, 0x00
    int 0x16

    cmp ah, 0x01                ; ESC scancode
    jne .ck_no_key
    mov al, 0xFF
    ret

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
; show_exit_info - Display image statistics before exiting to DOS
; ============================================================================
; Called after ESC. Text mode is already active and CGA palette restored.
; Prints BMP format details and color statistics, then returns.
; ============================================================================
show_exit_info:
    push ax
    push dx

    ; --- Header ---
    mov dx, msg_ex_header
    mov ah, 0x09
    int 0x21

    ; --- "  Format:  BMP, 320x200, " ---
    mov dx, msg_ex_format
    mov ah, 0x09
    int 0x21

    ; --- BPP ---
    xor ah, ah
    mov al, [bmp_bpp]
    call print_decimal_ax

    ; --- "-bit (" ---
    mov dx, msg_ex_bit
    mov ah, 0x09
    int 0x21

    ; --- num_colors ---
    mov ax, [num_colors]
    call print_decimal_ax

    ; --- " palette colors)" ---
    mov dx, msg_ex_cap
    mov ah, 0x09
    int 0x21

    ; --- "  Unique:  " + unique count + " colors used in image" ---
    mov dx, msg_ex_unique
    mov ah, 0x09
    int 0x21

    mov ax, [stat_unique_colors]
    call print_decimal_ax

    mov dx, msg_ex_used
    mov ah, 0x09
    int 0x21

    ; --- Blank line ---
    mov dx, msg_crlf
    mov ah, 0x09
    int 0x21

    pop dx
    pop ax
    ret

; ============================================================================
; print_decimal_ax - Print AX as unsigned decimal number via DOS
; ============================================================================
; Uses stack to reverse digit order. Handles 0-65535.
; ============================================================================
print_decimal_ax:
    push ax
    push bx
    push cx
    push dx

    xor cx, cx                  ; Digit count
    mov bx, 10

.pda_divide:
    xor dx, dx
    div bx                      ; AX = AX/10, DX = remainder
    push dx                     ; Save digit (on stack, reversed)
    inc cx
    or ax, ax
    jnz .pda_divide

.pda_print:
    pop dx                      ; Get digit (most significant first)
    add dl, '0'
    mov ah, 0x02
    int 0x21
    loop .pda_print

    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; count_skipped_lines - Count lines with stream_len = 0 (skip optimization)
; ============================================================================
; Scans stream_len[200]; counts entries == 0 (lines where scanline_top3[N+2]
; matches scanline_top3[N], so no palette writes are needed).
; Result stored in stat_lines_skipped for info overlay.
; ============================================================================
count_skipped_lines:
    push ax
    push cx
    push si

    xor ax, ax
    mov si, stream_len
    mov cx, 200

.csl_loop:
    cmp byte [si], 0
    jnz .csl_not_skip
    inc ax
.csl_not_skip:
    inc si
    loop .csl_loop

    mov [stat_lines_skipped], ax

    pop si
    pop cx
    pop ax
    ret

; ============================================================================
; count_unique_colors - Count palette indices used at least once in any line
; ============================================================================
; Scans color_line_count[0..num_colors-1] (set by reorder_by_stability).
; Any index with a non-zero line count is considered "used".
; Note: reorder_by_stability zeroes color_line_count[global_c] during its
; search, so we add 1 if global_c is not bg_index (that color WAS used).
; bg_index entries always have count 0 (background, not counted as "used").
; Result stored in stat_unique_colors.
; ============================================================================
count_unique_colors:
    push ax
    push bx
    push cx

    xor ax, ax                  ; Count of unique colors
    xor bx, bx                  ; Start at index 0 (word offset)
    mov cx, [num_colors]        ; Check all palette entries

.cuc_loop:
    cmp word [color_line_count + bx], 0
    je .cuc_not_used
    inc ax
.cuc_not_used:
    add bx, 2
    loop .cuc_loop

    ; Compensate for global_c whose count was zeroed by reorder_by_stability
    mov bl, [global_c]
    cmp bl, [bg_index]
    je .cuc_store               ; global_c is background, not a real color
    inc ax

.cuc_store:
    mov [stat_unique_colors], ax

    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; DATA - Messages
; ============================================================================

msg_info    db 'PC1-BMP4 v4.3 - Flip-First (512-color)', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'Displays 320x200 4-bit or 8-bit BMP images', 0x0D, 0x0A
            db 'using palette flip + per-line streaming.', 0x0D, 0x0A
            db '3 colors/line + black, up to 512 V6355D colors.', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'Usage: PC1-BMP4 filename.bmp', 0x0D, 0x0A
            db '       ESC = exit (shows image info)', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'By RetroErik - 2026', 0x0D, 0x0A, '$'

; Splash screen with ANSI color escape codes (ESC = 0x1B)
; ANSI: ESC[<attr>m  where 1=bold, 0=reset, 3x=fg color, 9x=bright fg
;   30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white
msg_splash   db 0x1B, '[2J', 0x1B, '[H'          ; Clear screen, home cursor
             db 0x0D, 0x0A
             db 0x0D, 0x0A
             db 0x0D, 0x0A
             db 0x0D, 0x0A
             db 0x0D, 0x0A
             db 0x0D, 0x0A
             db 0x0D, 0x0A
             db 0x0D, 0x0A
             db '                              '
             db 0x1B, '[1;36m'                    ; Bright Cyan
             db 'Olivetti Prodest PC1'
             db 0x1B, '[0m', 0x0D, 0x0A
             db '                            '
             db 0x1B, '[1;33m'                    ; Bright Yellow
             db 'CGA Mode 4 Image Viewer'
             db 0x1B, '[0m', 0x0D, 0x0A
             db '                 '
             db 0x1B, '[1;37m'                    ; Bright White
             db 'Gives you up to '
             db 0x1B, '[1;32m'                    ; Bright Green
             db '512'
             db 0x1B, '[1;37m'                    ; Bright White
             db ' unique colors in 320x200'
             db 0x1B, '[0m', 0x0D, 0x0A
             db 0x0D, 0x0A
             db '                              '
             db 0x1B, '[1;35m'                    ; Bright Magenta
             db 'RetroErik'
             db 0x1B, '[0m'
             db ' 2026', 0x0D, 0x0A
             db 0x0D, 0x0A
             db 0x0D, 0x0A
             db '                                '
             db 0x1B, '[1;33m'                    ; Bright Yellow
             db 'Loading Image...'
             db 0x1B, '[0m', 0x0D, 0x0A, '$'
msg_file_err db 'Error: Cannot open file', 0x0D, 0x0A, '$'
msg_not_bmp  db 'Error: Not a valid BMP file', 0x0D, 0x0A, '$'
msg_format   db 'Error: BMP must be 4-bit or 8-bit uncompressed', 0x0D, 0x0A, '$'
msg_size     db 'Error: BMP must be 320x200', 0x0D, 0x0A, '$'

; Exit info messages (shown when ESC is pressed)
msg_ex_header   db 0x0D, 0x0A
                db '  PC1-BMP4 - Image Info', 0x0D, 0x0A
                db '  ----------------------', 0x0D, 0x0A
                db 0x0D, 0x0A, '$'
msg_ex_format   db '  Format:  BMP, 320x200, $'
msg_ex_bit      db '-bit ($'
msg_ex_cap      db ' palette colors)', 0x0D, 0x0A, '$'
msg_ex_unique   db '  Unique:  $'
msg_ex_used     db ' colors used in image', 0x0D, 0x0A, '$'
msg_crlf        db 0x0D, 0x0A, '$'

; ============================================================================
; DATA - Standard CGA text mode palette (for restore on exit)
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

; Format detection (set during BMP validation)
bmp_bpp:        db 0            ; 4 or 8 (bits per pixel)
num_colors:     dw 0            ; 16 or 256 (palette entries)
bmp_row_bytes:  dw 0            ; 160 or 320 (bytes per BMP row)

; Background color detection
bg_index:       db 0            ; Darkest palette entry (auto-detected)

; C64-style loading effect
border_ctr:     db 0            ; Border color cycling counter (0-15)

; Exit info statistics
stat_lines_skipped: dw 0        ; Lines with stream_len=0 (skip optimization)
stat_unique_colors: dw 0        ; Unique palette colors used in image

; BMP palette RGB888 components (for nearest-color distance calculation)
pal_r:          times 256 db 0  ; Red channel 0-255
pal_g:          times 256 db 0  ; Green channel 0-255
pal_b:          times 256 db 0  ; Blue channel 0-255

; V6355D format palette (up to 256 colors × 2 bytes each)
v6355_pal:      times 512 db 0

; Per-scanline analysis workspace
top3_temp:      times 3 db 0    ; Temp: color indices for build_remap_table
nn_best_dist:   dw 0            ; Temp: nearest-neighbor best distance
nn_best_cga:    db 0            ; Temp: nearest-neighbor best CGA value

; Temp: build_palette_stream current/next line color indices
bps_cur_a:      db 0
bps_cur_b:      db 0
bps_cur_c:      db 0
bps_nxt_a:      db 0
bps_nxt_b:      db 0
bps_nxt_c:      db 0

; Color frequency workspace (256 words = 512 bytes)
color_count:    times 512 db 0

; Per-scanline top 3 color indices (200 rows × 3 = 600 bytes)
scanline_top3:  times 600 db 0

; Pixel remap table (256 bytes, rebuilt per scanline during Pass 2)
remap_table:    times 256 db 0

; Per-line stream length (200 bytes): 0 or 12 bytes to write
; 0 = skip (no entries changed), 12 = full write (all entries changed)
stream_len:     times 200 db 0

; Global color stability analysis
color_line_count: times 512 db 0 ; 256 words: line count per BMP palette index
global_c:       db 0            ; Most common color → entries 6/7 (most stable)
global_b:       db 0            ; 2nd most common → entries 4/5

; ============================================================================
; Palette stream for render loop
; ============================================================================
; 200 lines × 12 bytes per line = 2400 bytes
;
; Each line: V6355D data for entries 2-7 only (2 bytes each, 6 entries).
; Entries 0-1 (always black) are hardcoded in render_frame.
;
; INTERLEAVING: active entries get same-value rewrites, inactive
; entries get the next line's colors pre-loaded for the upcoming flip.
;
;   Even line: E2/E4/E6 = current (active), E3/E5/E7 = next (inactive)
;   Odd line:  E3/E5/E7 = current (active), E2/E4/E6 = next (inactive)
; ============================================================================
palette_stream: times 2400 db 0

; File I/O buffers
bmp_header:     times 1088 db 0 ; BMP file header + info header + palette (up to 1078)
row_buffer:     times 324 db 0  ; BMP row input (up to 320 bytes + safety margin)

; ============================================================================
; END OF PROGRAM
; ============================================================================
