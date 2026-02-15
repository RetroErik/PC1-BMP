; ============================================================================
; PC1-BMP v6.0 - E0 Reprogramming.asm
; BMP Viewer: Simone Technique + Dithering + E0 Color (Experimental)
; ============================================================================
; ARCHIVED — E0 reprogramming experiment. Proved the technique works but
; provides minimal benefit: black (index 0) is in the top 4 on almost every
; scanline, so the 4th slot just reclaims what was already free. Only gains
; a color on scanlines with zero black pixels (rare in game art).
; Also causes colored border artifacts on E0-reprogrammed lines.
; ============================================================================
;
; Displays 320x200 4-bit BMP images on the Olivetti Prodest PC1 using
; CGA 320x200x4 mode with per-scanline V6355D palette reprogramming.
;
; "SIMONE TECHNIQUE" — named after Simone, who first demonstrated this
; approach in his Monkey Island conversion on the Olivetti PC1.
; Thanks for the inspiration, Simone!
;
; The key innovation: use CGA palette flip (port 0xD9 bit 5) to alternate
; between two palettes each scanline. Combined with per-scanline palette
; reprogramming, this gives 3 INDEPENDENT colors per scanline.
;
; v4.0: FLIP-FIRST (Simone-calibrated)
;   The palette flip is the VERY FIRST instruction after HBLANK detection,
;   exactly as Simone prescribes ("calibrated at nanosecond"). The flip
;   reveals colors that were pre-loaded into inactive entries during the
;   PREVIOUS HBLANK. All subsequent palette writes target only INACTIVE
;   entries — pre-loading for the NEXT same-parity line (N+2).
;
;   Result: near-zero flicker. The only visible artifact is the first
;   scanline at the top of the screen. The rest of the image is stable.
;   This is the best visual quality achieved with the Simone technique.
;
; v5.0: DITHERING MODES
;   Adds 4 switchable dithering modes (keys 1-4), re-rendering on switch:
;     1 = None (nearest-color via XLAT) — sharpest, fewest perceived colors
;     2 = Bayer 4×4 ordered dithering (spread=32) — visible grid pattern
;     3 = 1D scanline error diffusion (err/2 right) — no cross-line artifacts
;     4 = Sierra-style 2×2 stipple (color-pair checkerboard) ⭐ BEST
;   Mode 4 uses the same approach as Sierra On-Line's CGA game drivers:
;   precomputed color-pair lookup tables with 2×2 checkerboard alternation.
;   For each BMP color, finds the CGA color pair whose average RGB best
;   matches the target. XLAT-based inner loop — fast as mode 0.
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
;        (open at 0x44 + 12×OUTSB + close = ~198 cycles).
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
;   Each scanline independently picks its top 3 most frequent non-black
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
;   1. Load and validate 320x200 4-bit BMP
;   2. Convert BMP 16-color BGRA palette to RGB888 + V6355D format
;   3. PASS 1: For each scanline, count pixel color frequency,
;      pick top 3 most frequent non-black colors
;   3b. STABILITY REORDER: Count global color frequency across all
;       lines, reorder each line's top3 so most stable → slot C
;       (entries 6/7), most volatile → slot A (entries 2/3)
;   4. Build palette stream: interleaved V6355D data for entries 2-7
;      (12 bytes per line = 2400 bytes total). Pre-loads inactive
;      entries with line N+2 colors (next same-parity line).
;   4b. BUILD STREAM LENGTHS: Compare scanline_top3[N+2] vs [N]
;       (same-parity gap). Skip if all 3 colors match (0 or 12 bytes).
;   5. PASS 2: Re-read BMP, remap each pixel to CGA value 0-3
;      using selected dithering mode (none / Bayer / 1D error / stipple),
;      pack into 2bpp CGA VRAM
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
;   This is the best visual quality achieved with the Simone technique
;   on real Olivetti Prodest PC1 hardware.
;
; ============================================================================
; LIMITATIONS
; ============================================================================
;
;   - Only 320x200 4-bit uncompressed BMPs supported
;   - BMP palette index 0 is always treated as black (background)
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
;   nasm -f bin -o "PC1-BMP v6.0 - E0 Reprogramming.com" "PC1-BMP v6.0 - E0 Reprogramming.asm"
;
; ============================================================================
; USAGE
; ============================================================================
;
;   PC1-BMP4 filename.bmp
;
;   Controls:
;     ESC : Exit to DOS
;     1   : No dithering (nearest-color)
;     2   : Bayer ordered dithering (4x4)
;     3   : 1D scanline error diffusion
;     4   : Sierra-style 2×2 stipple (color-pair checkerboard) ⭐ Best
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
    mov dx, msg_dither_hint
    mov ah, 0x09
    int 0x21

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
    ; Count pixel colors per scanline, find top 3 per line.
    ; No global color accumulation — each line is independent.
    call analyze_image

    ; --- Reorder colors by stability (most stable → high entries) ---
    call reorder_by_stability

    ; --- Build per-scanline palette stream ---
    ; Interleaved V6355D data for entries 0-7 (16 bytes/line = 3200).
    call build_palette_stream

    ; --- Pass 2: Render ---
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
    ; (Not checking error — same seek worked before)

    ; --- PASS 2: Remap pixels and write to CGA VRAM ---
    call render_to_vram

    ; (File stays open for re-render on dither mode switch)

    ; --- Program initial palette (line 0 + line 1 colors) ---
    call program_initial_palette

    ; --- Set palette 0 + enable video ---
    mov al, PAL_EVEN
    out PORT_COLOR, al

    mov al, CGA_MODE4_ON
    out PORT_MODE, al

    ; --- Display loop ---
    mov byte [hsync_enabled], 1
    mov byte [vsync_enabled], 1

.display_loop:
    call wait_vblank
    call render_frame
    call check_keyboard
    cmp al, 0xFF
    jne .display_loop

    ; --- Close file ---
    mov bx, [file_handle]
    mov ah, 0x3E                ; DOS Close File
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
; analyze_scanline - Count colors, find top 3 + conditional 4th for E0
; ============================================================================
; Input: [current_row], row_buffer filled with BMP pixel data
; Output: scanline_top3 updated for this row, scanline_e0 updated
;
; Strategy: top 3 non-black → CGA 1-3. If scanline has NO black pixels,
; E0 = 4th non-black color (gain a color). If black present, E0 = black.
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

    ; --- Save and exclude black (index 0) from color search ---
    mov di, [color_count]       ; DI = black pixel count (saved)
    mov word [color_count], 0   ; Exclude black from search

    ; --- Find top 3 most frequent non-black colors ---
    call find_max_color         ; Returns AL = best non-black index
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
    xor bx, bx
    mov bl, al
    shl bx, 1
    mov word [color_count + bx], 0

    ; --- 4th non-black color → candidate for E0 (Color D) ---
    call find_max_color         ; AL = 4th non-black, or 0 if none

    ; Use 4th color for E0 only when scanline has NO black pixels.
    ; If black is present, keep E0 = black (border stays clean).
    mov bx, [current_row]
    or di, di                   ; Any black pixels on this line?
    jnz .e0_use_black
    ; No black pixels — E0 = 4th non-black color (gain a color!)
    mov [scanline_e0 + bx], al
    jmp .e0_done
.e0_use_black:
    ; Black pixels present — E0 = black (safe, same as BMP3)
    mov byte [scanline_e0 + bx], 0
.e0_done:

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
; find_max_color - Find palette index (1-15) with highest pixel count
; Returns: AL = best index (0 if no non-zero counts remain)
; ============================================================================
find_max_color:
    push bx
    push cx
    push dx

    xor ax, ax                  ; Best index = 0
    xor dx, dx                  ; Best count = 0
    xor bx, bx                 ; Start at color_count[0] (word offset)
    mov cx, 16                  ; Check indices 0-15

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

    ; --- Clear line presence count (16 words) ---
    mov di, color_line_count
    mov cx, 16
.rbs_clr:
    mov word [di], 0
    add di, 2
    loop .rbs_clr

    ; --- Count lines where each color index appears ---
    mov si, scanline_top3
    mov cx, 200
.rbs_count:
    ; 3 entries per line
    lodsb                       ; Slot A
    or al, al
    jz .rbs_skip1
    xor bx, bx
    mov bl, al
    shl bx, 1
    inc word [color_line_count + bx]
.rbs_skip1:
    lodsb                       ; Slot B
    or al, al
    jz .rbs_skip2
    xor bx, bx
    mov bl, al
    shl bx, 1
    inc word [color_line_count + bx]
.rbs_skip2:
    lodsb                       ; Slot C
    or al, al
    jz .rbs_skip3
    xor bx, bx
    mov bl, al
    shl bx, 1
    inc word [color_line_count + bx]
.rbs_skip3:
    loop .rbs_count

    ; --- Find most common color → global_c (entries 6/7) ---
    xor ax, ax                  ; Best index
    xor dx, dx                  ; Best count
    mov bx, 2                   ; Start at index 1
    mov cx, 15
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
    or al, al
    jz .rbs_no_reorder          ; No colors found at all
    xor bx, bx
    mov bl, al
    shl bx, 1
    mov word [color_line_count + bx], 0

    ; --- Find 2nd most common → global_b (entries 4/5) ---
    xor ax, ax
    xor dx, dx
    mov bx, 2
    mov cx, 15
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
    or dh, dh
    jz .rbs_store               ; No global_b
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
; build_palette_stream - Build FLIP-FIRST interleaved V6355D data (E0-E7)
; ============================================================================
; Uses scanline_top3[] and scanline_e0[] to build 16 bytes per line:
;   Bytes  0-1: E0 = Color D for line N+1 (immediate effect, shared by both pals)
;   Bytes  2-3: E1 = zeros (unused entry)
;   Bytes  4-15: E2-E7 interleaved (same flip-first logic as before)
;
; FLIP-FIRST INTERLEAVING (Simone-calibrated):
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
; Total: 200 lines × 16 bytes = 3200 bytes.
; Target lines wrap around (line 200→0, 201→1) for seamless frame loop.
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
    ; --- E0: Color D for line N+1 (immediate effect after HBLANK) ---
    mov bx, cx
    inc bx
    cmp bx, 200
    jb .bps_e0_ok
    sub bx, 200
.bps_e0_ok:
    xor ah, ah
    mov al, [scanline_e0 + bx]  ; 4th color index for line N+1
    shl ax, 1
    mov si, ax
    mov ax, [v6355_pal + si]
    mov [di], ax                ; [di+0..1] = E0 R, E0 GB

    ; --- E1: zeros (unused entry) ---
    xor ax, ax
    mov [di + 2], ax            ; [di+2..3] = E1 R, E1 GB

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
    mov [di + 4], ax

    ; E3 = line N+1 Color A (active, passthrough)
    xor bx, bx
    mov bl, [bps_nxt_a]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 6], ax

    ; E4 = line N+2 Color B (inactive, pre-load)
    xor bx, bx
    mov bl, [bps_cur_b]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 8], ax

    ; E5 = line N+1 Color B (active, passthrough)
    xor bx, bx
    mov bl, [bps_nxt_b]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 10], ax

    ; E6 = line N+2 Color C (inactive, pre-load)
    xor bx, bx
    mov bl, [bps_cur_c]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 12], ax

    ; E7 = line N+1 Color C (active, passthrough)
    xor bx, bx
    mov bl, [bps_nxt_c]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 14], ax

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
    mov [di + 4], ax

    ; E3 = line N+2 Color A (inactive, pre-load)
    xor bx, bx
    mov bl, [bps_cur_a]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 6], ax

    ; E4 = line N+1 Color B (active, passthrough)
    xor bx, bx
    mov bl, [bps_nxt_b]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 8], ax

    ; E5 = line N+2 Color B (inactive, pre-load)
    xor bx, bx
    mov bl, [bps_cur_b]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 10], ax

    ; E6 = line N+1 Color C (active, passthrough)
    xor bx, bx
    mov bl, [bps_nxt_c]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 12], ax

    ; E7 = line N+2 Color C (inactive, pre-load)
    xor bx, bx
    mov bl, [bps_cur_c]
    shl bx, 1
    mov ax, [v6355_pal + bx]
    mov [di + 14], ax

.bps_next:
    add di, 16
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

    ; Get top3 pointer for line N+2 (clamped to 199)
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
;   1. Read BMP row (4bpp, 160 bytes)
;   2. Build remap table (16 entries: BMP index → CGA value 0-3)
;   3. Convert 4bpp → 2bpp using remap, write to interlaced CGA VRAM
;
; Dithering modes (selected by [dither_mode]):
;   0 = None (nearest-color via remap_table XLAT)
;   1 = Bayer 4×4 ordered dithering (position-dependent remap)
;   2 = 1D scanline error diffusion (horizontal-only, no cross-line)
;   3 = Sierra-style 2×2 stipple (precomputed color-pair checkerboard)
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

    ; Build remap table for this scanline's 3 color assignment
    ; (also sets top3_temp — needed by all dither modes)
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

    ; Branch on dithering mode
    cmp byte [dither_mode], 0
    je .rv_mode0
    cmp byte [dither_mode], 1
    je .rv_mode1
    cmp byte [dither_mode], 2
    je .rv_mode2
    jmp .rv_mode3

    ; ==================================================================
    ; MODE 0: No dithering (original nearest-color XLAT)
    ; ==================================================================
.rv_mode0:
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
    jmp .rv_next_row

    ; ==================================================================
    ; MODE 1: Bayer 4×4 ordered dithering
    ; ==================================================================
    ; Uses a 64-byte precomputed table: bayer_remap[idx*4 + x%4]
    ; The table is rebuilt per row (threshold row changes with y%4).
    ; Inner loop is still XLAT-based — nearly as fast as mode 0.
    ; ==================================================================
.rv_mode1:
    call build_bayer_remap       ; Build 64-byte position-dependent remap

    mov si, row_buffer
    mov bx, bayer_remap
    mov cx, CGA_ROW_BYTES        ; 80 output bytes

.rv_bayer_convert:
    xor dh, dh

    ; Pixel 0 (x%4 = 0)
    lodsb
    mov dl, al
    shr al, 4                   ; High nibble = BMP index
    shl al, 2                   ; × 4 (index into per-color block)
    ; x%4 = 0, no add needed
    xlat                         ; bayer_remap[idx*4 + 0]
    or dh, al
    shl dh, 2

    ; Pixel 1 (x%4 = 1)
    mov al, dl
    and al, 0x0F
    shl al, 2
    inc al                       ; + 1
    xlat
    or dh, al
    shl dh, 2

    ; Pixel 2 (x%4 = 2)
    lodsb
    mov dl, al
    shr al, 4
    shl al, 2
    add al, 2
    xlat
    or dh, al
    shl dh, 2

    ; Pixel 3 (x%4 = 3)
    mov al, dl
    and al, 0x0F
    shl al, 2
    add al, 3
    xlat
    or dh, al

    mov al, dh
    stosb
    loop .rv_bayer_convert
    jmp .rv_next_row

    ; ==================================================================
    ; MODE 2: 1D scanline error diffusion (horizontal-only)
    ; ==================================================================
    ; Error propagates only RIGHT within each scanline (err/2).
    ; No cross-line buffers — avoids palette-mismatch artifacts.
    ; Naturally suited to per-scanline palette changes.
    ;
    ; Two passes per row:
    ;   Pass A: Error-adjusted nearest-color → fs_cga_row[320]
    ;   Pass B: Pack fs_cga_row into 2bpp CGA bytes → VRAM
    ; ==================================================================
.rv_mode2:
    mov [fs_vram_ofs], di        ; Save VRAM offset for pass B

    ; Reset running error accumulators for this scanline
    xor ax, ax
    mov [err_r], ax
    mov [err_g], ax
    mov [err_b], ax

    mov word [fs_pixel_x], 0

.rv_1d_pixel:
    ; Extract BMP palette index for pixel X
    mov bx, [fs_pixel_x]
    shr bx, 1                   ; Byte offset = x / 2
    mov al, [row_buffer + bx]
    test byte [fs_pixel_x], 1
    jz .rv_1d_hi
    and al, 0x0F                 ; Odd pixel: low nibble
    jmp .rv_1d_got_idx
.rv_1d_hi:
    shr al, 4                   ; Even pixel: high nibble
.rv_1d_got_idx:
    ; AL = BMP palette index (0-15)
    xor ah, ah
    mov bx, ax                  ; BX = source palette index

    ; Adjusted R = pal_r[bx] + accumulated error
    xor ah, ah
    mov al, [pal_r + bx]
    add ax, [err_r]
    call clamp_byte
    mov [temp_adj_r], al

    ; Adjusted G
    xor ah, ah
    mov al, [pal_g + bx]
    add ax, [err_g]
    call clamp_byte
    mov [temp_adj_g], al

    ; Adjusted B
    xor ah, ah
    mov al, [pal_b + bx]
    add ax, [err_b]
    call clamp_byte
    mov [temp_adj_b], al

    ; Find nearest of {black, A, B, C}
    call find_nearest_rgb        ; AL = CGA value (0-3)

    ; Store CGA value for pass B packing
    mov bx, [fs_pixel_x]
    mov [fs_cga_row + bx], al

    ; Get chosen color's RGB components
    call get_cga_rgb             ; Sets chosen_r, chosen_g, chosen_b

    ; --- Compute error and propagate right (err / 2) ---

    ; Error R = adjusted_r - chosen_r, propagate half right
    xor ah, ah
    mov al, [temp_adj_r]
    xor bh, bh
    mov bl, [chosen_r]
    sub ax, bx                   ; AX = error (signed)
    sar ax, 1                   ; err / 2
    mov [err_r], ax

    ; Error G
    xor ah, ah
    mov al, [temp_adj_g]
    xor bh, bh
    mov bl, [chosen_g]
    sub ax, bx
    sar ax, 1
    mov [err_g], ax

    ; Error B
    xor ah, ah
    mov al, [temp_adj_b]
    xor bh, bh
    mov bl, [chosen_b]
    sub ax, bx
    sar ax, 1
    mov [err_b], ax

    ; Next pixel
    inc word [fs_pixel_x]
    cmp word [fs_pixel_x], 320
    jb .rv_1d_pixel

    ; --- Pass B: Pack fs_cga_row → 2bpp CGA VRAM ---
    mov si, fs_cga_row
    mov di, [fs_vram_ofs]
    mov cx, CGA_ROW_BYTES        ; 80 output bytes

.rv_1d_pack:
    xor dh, dh

    lodsb                        ; Pixel 0
    or dh, al
    shl dh, 2

    lodsb                        ; Pixel 1
    or dh, al
    shl dh, 2

    lodsb                        ; Pixel 2
    or dh, al
    shl dh, 2

    lodsb                        ; Pixel 3
    or dh, al

    mov al, dh
    stosb
    loop .rv_1d_pack

    ; jmp .rv_next_row (fall through)

    ; ==================================================================
    ; MODE 3: Sierra-style 2×2 stipple (color-pair checkerboard)
    ; ==================================================================
    ; For each BMP palette index, precomputes the TWO CGA colors whose
    ; average best matches that color. Pixels alternate between the
    ; two colors in a 2×2 checkerboard pattern.
    ; Uses XLAT with stride-2 table — same speed as Bayer mode.
    ; ==================================================================
.rv_mode3:
    call build_sierra_remap      ; Build 32-byte pair remap for this row

    mov si, row_buffer
    mov bx, sierra_remap
    mov cx, CGA_ROW_BYTES        ; 80 output bytes

.rv_sierra_convert:
    xor dh, dh

    ; Pixel 0 (phase 0)
    lodsb
    mov dl, al
    shr al, 4                   ; High nibble = BMP index
    shl al, 1                   ; × 2 (stride into pair table)
    xlat                         ; sierra_remap[idx*2 + 0]
    or dh, al
    shl dh, 2

    ; Pixel 1 (phase 1)
    mov al, dl
    and al, 0x0F
    shl al, 1
    inc al                       ; + 1
    xlat                         ; sierra_remap[idx*2 + 1]
    or dh, al
    shl dh, 2

    ; Pixel 2 (phase 0)
    lodsb
    mov dl, al
    shr al, 4
    shl al, 1
    xlat
    or dh, al
    shl dh, 2

    ; Pixel 3 (phase 1)
    mov al, dl
    and al, 0x0F
    shl al, 1
    inc al
    xlat
    or dh, al

    mov al, dh
    stosb
    loop .rv_sierra_convert
    jmp .rv_next_row

    ; ==================================================================
    ; Common: advance to next row
    ; ==================================================================
.rv_next_row:
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
; CGA value mapping (4 independent colors per scanline):
;   0 = Color D (4th color, → entry 0, shared by both palettes)
;   1 = Color A (most frequent per-line, → entry 2/3)
;   2 = Color B (2nd most frequent, → entry 4/5)
;   3 = Color C (3rd most frequent, → entry 6/7)
;
; For each BMP palette index:
;   Index == Color D     → CGA 0
;   Index == Color A     → CGA 1
;   Index == Color B     → CGA 2
;   Index == Color C     → CGA 3
;   Otherwise            → nearest of {D, A, B, C} by RGB distance
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

    ; Get E0 color (Color D → CGA 0)
    mov bx, [current_row]
    mov al, [scanline_e0 + bx]
    mov [e0_temp], al

    ; Map each BMP index 0-15 to CGA value 0-3
    xor cx, cx                  ; CL = current palette index

.brt_loop:
    ; Direct match with chosen colors?
    cmp cl, [e0_temp]           ; Color D?
    je .brt_0
    cmp cl, [top3_temp]         ; Color A?
    je .brt_1
    cmp cl, [top3_temp + 1]     ; Color B?
    je .brt_2
    cmp cl, [top3_temp + 2]     ; Color C?
    je .brt_3

    ; No direct match: find nearest by RGB distance
    call find_nearest           ; CL = index, returns AL = CGA value
    jmp .brt_store

.brt_0:
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
; Compares against: Color D (e0_temp), Color A, Color B, Color C
; (stored in top3_temp[0..2] by build_remap_table)
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

    ; --- Distance to Color D (e0_temp) → CGA 0 ---
    mov al, [e0_temp]
    xor ah, ah
    mov si, ax                  ; SI = Color D index
    call compute_dist_bx_si     ; DX = distance
    mov [nn_best_dist], dx
    mov byte [nn_best_cga], 0

    ; --- Distance to top3[0] → CGA 1 ---
    mov al, [top3_temp]
    xor ah, ah
    mov si, ax                  ; SI = Color A index
    call compute_dist_bx_si     ; DX = distance
    cmp dx, [nn_best_dist]
    jae .fn_try_2
    mov [nn_best_dist], dx
    mov byte [nn_best_cga], 1

.fn_try_2:
    mov al, [top3_temp + 1]
    xor ah, ah
    mov si, ax
    call compute_dist_bx_si
    cmp dx, [nn_best_dist]
    jae .fn_try_3
    mov [nn_best_dist], dx
    mov byte [nn_best_cga], 2

.fn_try_3:
    mov al, [top3_temp + 2]
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
; render_frame - FLIP-FIRST per-scanline HBLANK streaming (E0-E7)
; ============================================================================
; SIMONE-CALIBRATED FLIP-FIRST with E0 REPROGRAMMING:
;
;   The flip is the FIRST thing after HBLANK detection. Then the
;   full 16-byte stream (E0-E7) is written starting at 0x40.
;
;   E0-E1 complete within HBLANK (~44 cycles, invisible).
;   E2-E7 spill into visible area but target INACTIVE entries only.
;
;   E0 = 4th color for next line (immediate effect, no flip trick)
;   E1 = zeros (unused entry)
;   E2-E7 = flip-first interleaved (pre-load N+2, passthrough N+1)
;
; Data: palette_stream[200 × 16] = 3200 bytes
; ============================================================================
render_frame:
    cli
    cld                         ; Ensure OUTSB increments SI

    mov si, palette_stream
    mov cx, SCREEN_HEIGHT
    mov dx, PORT_REG_DATA       ; DX = 0xDE (stays constant for OUTSB)
    mov bl, PAL_ODD             ; After line 0's stream, flip to ODD for line 1
    mov bh, PAL_EVEN

    cmp byte [hsync_enabled], 0
    je .rf_no_hsync

    ; ------------------------------------------------------------------
    ; HSYNC-synchronized loop (always writes 16 bytes)
    ; ------------------------------------------------------------------

.rf_scanline:
.rf_wait_low:
    in al, PORT_STATUS
    test al, 0x01
    jnz .rf_wait_low            ; Wait while in retrace

.rf_wait_high:
    in al, PORT_STATUS
    test al, 0x01
    jz .rf_wait_high            ; Wait for HBLANK start

    ; === FLIP FIRST — nanosecond-critical (Simone-calibrated) ===
    mov al, bl
    out PORT_COLOR, al
    xchg bl, bh

    ; === Stream all 16 bytes: E0-E7 ===
    mov al, 0x40
    out PORT_REG_ADDR, al       ; Open palette at entry 0

    outsb                       ; E0 R   (Color D for N+1)
    outsb                       ; E0 GB
    outsb                       ; E1 R   (zeros)
    outsb                       ; E1 GB
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

    loop .rf_scanline
    jmp .rf_done

    ; ------------------------------------------------------------------
    ; Non-synchronized loop (for testing with H toggle)
    ; ------------------------------------------------------------------
.rf_no_hsync:
.rf_nosync_line:
    ; Flip first (same order as HSYNC loop)
    mov al, bl
    out PORT_COLOR, al
    xchg bl, bh

    mov al, 0x40
    out PORT_REG_ADDR, al

    outsb                       ; E0 R
    outsb                       ; E0 GB
    outsb                       ; E1 R
    outsb                       ; E1 GB
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
    out PORT_REG_ADDR, al

    loop .rf_nosync_line

.rf_done:
    ; Reset to palette 0, clean state
    mov al, PAL_EVEN
    out PORT_COLOR, al
    sti
    ret

; ============================================================================
; program_initial_palette - Set V6355D entries for first display frame
; ============================================================================
; Called once before enabling video. Sets all 8 entries from analysis:
;   Entry 0 = Color D[0] (4th color for line 0, via scanline_e0)
;   Entry 1 = zeros (unused)
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

    ; Entry 0: Color D for line 0 (from scanline_e0[0])
    xor bh, bh
    mov bl, [scanline_e0]       ; Color D index for line 0
    shl bx, 1
    mov al, [v6355_pal + bx]
    out PORT_REG_DATA, al       ; E0 R
    jmp short $+2
    mov al, [v6355_pal + bx + 1]
    out PORT_REG_DATA, al       ; E0 GB
    jmp short $+2
    ; Entry 1: zeros (unused)
    xor al, al
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
    cmp al, '1'
    je .ck_dither_0
    cmp al, '2'
    je .ck_dither_1
    cmp al, '3'
    je .ck_dither_2
    cmp al, '4'
    je .ck_dither_3

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
    jmp .ck_no_key

    ; --- Dither mode switch handlers ---
.ck_dither_0:
    cmp byte [dither_mode], 0
    je .ck_no_key
    mov byte [dither_mode], 0
    call re_render
    jmp .ck_no_key

.ck_dither_1:
    cmp byte [dither_mode], 1
    je .ck_no_key
    mov byte [dither_mode], 1
    call re_render
    jmp .ck_no_key

.ck_dither_2:
    cmp byte [dither_mode], 2
    je .ck_no_key
    mov byte [dither_mode], 2
    call re_render
    jmp .ck_no_key

.ck_dither_3:
    cmp byte [dither_mode], 3
    je .ck_no_key
    mov byte [dither_mode], 3
    call re_render
    jmp .ck_no_key

.ck_no_key:
    xor al, al
    ret

; ============================================================================
; re_render - Re-render VRAM with current dither_mode
; ============================================================================
; Called when user switches dither mode (keys 1/2/3/4).
; Blanks video, seeks back to BMP pixel data, re-renders, re-enables video.
; The palette stream and scanline_top3 are unchanged — only VRAM changes.
; ============================================================================
re_render:
    pusha

    ; Disable video during VRAM rewrite
    mov al, CGA_MODE4_OFF
    out PORT_MODE, al

    ; Seek back to pixel data
    mov bx, [file_handle]
    mov dx, [bmp_header + BMP_DATA_OFFSET]
    mov cx, [bmp_header + BMP_DATA_OFFSET + 2]
    mov ax, 0x4200
    int 0x21

    ; Render with current dither mode
    call render_to_vram

    ; Re-enable video
    mov al, CGA_MODE4_ON
    out PORT_MODE, al

    popa
    ret

; ============================================================================
; build_bayer_remap - Build 64-byte position-dependent remap table
; ============================================================================
; For current row, precomputes: bayer_remap[bmp_idx * 4 + x%4] = CGA value
; Uses Bayer 4×4 threshold matrix to adjust RGB before nearest-color match.
; Called once per scanline in Bayer mode. ~64 nearest-color lookups.
;
; Input: [current_row] set, top3_temp set by build_remap_table
; Output: bayer_remap[64] filled
; ============================================================================
build_bayer_remap:
    pusha

    ; Get Bayer row offset: (current_row & 3) * 4
    mov ax, [current_row]
    and ax, 3
    shl ax, 2
    mov si, ax                  ; SI = Bayer row offset (0, 4, 8, or 12)

    xor cx, cx                  ; CX = BMP palette index (0-15)

.bbr_idx_loop:
    xor dx, dx                  ; DX = x_mod4 (0-3)

.bbr_xmod_loop:
    ; Get Bayer threshold for (y%4, x%4)
    mov bx, si
    add bx, dx
    mov al, [bayer_thresholds + bx]
    cbw                         ; AX = signed threshold (-32 to +28)
    mov [bayer_thresh_tmp], ax

    ; Adjust R = clamp(pal_r[idx] + threshold, 0, 255)
    xor bh, bh
    mov bl, cl                  ; BX = palette index
    xor ah, ah
    mov al, [pal_r + bx]
    add ax, [bayer_thresh_tmp]
    call clamp_byte
    mov [temp_adj_r], al

    ; Adjust G
    xor ah, ah
    mov al, [pal_g + bx]
    add ax, [bayer_thresh_tmp]
    call clamp_byte
    mov [temp_adj_g], al

    ; Adjust B
    xor ah, ah
    mov al, [pal_b + bx]
    add ax, [bayer_thresh_tmp]
    call clamp_byte
    mov [temp_adj_b], al

    ; Find nearest CGA color using adjusted RGB
    call find_nearest_rgb        ; AL = CGA value (0-3)

    ; Store in bayer_remap[idx * 4 + x_mod]
    mov bx, cx
    shl bx, 2
    add bx, dx
    mov [bayer_remap + bx], al

    inc dx
    cmp dx, 4
    jb .bbr_xmod_loop

    inc cx
    cmp cx, 16
    jb .bbr_idx_loop

    popa
    ret

; ============================================================================
; build_sierra_remap - Build 32-byte color-pair checkerboard remap table
; ============================================================================
; For each BMP palette index (0-15), finds the pair of CGA colors (0-3)
; whose average RGB best matches the target BMP color. Stores the pair
; into sierra_remap[idx*2 + phase], where phase alternates in a 2×2
; checkerboard. Phase ordering is swapped for odd rows.
;
; Sierra games used this approach for CGA: precomputed color-pair lookup
; tables with 2×2 stipple patterns. Speed was the priority.
;
; Input: top3_temp[0..2] set by build_remap_table, [current_row]
; Output: sierra_remap[32] filled
; ============================================================================
build_sierra_remap:
    pusha

    ; --- Precompute CGA color RGB (4 colors × 3 channels) ---
    ; CGA 0 = Color D (e0_temp)
    xor bh, bh
    mov bl, [e0_temp]
    mov al, [pal_r + bx]
    mov [cga_rgb], al
    mov al, [pal_g + bx]
    mov [cga_rgb+1], al
    mov al, [pal_b + bx]
    mov [cga_rgb+2], al

    ; CGA 1 = top3_temp[0]
    xor bh, bh
    mov bl, [top3_temp]
    mov al, [pal_r + bx]
    mov [cga_rgb+3], al
    mov al, [pal_g + bx]
    mov [cga_rgb+4], al
    mov al, [pal_b + bx]
    mov [cga_rgb+5], al

    ; CGA 2 = top3_temp[1]
    mov bl, [top3_temp+1]
    mov al, [pal_r + bx]
    mov [cga_rgb+6], al
    mov al, [pal_g + bx]
    mov [cga_rgb+7], al
    mov al, [pal_b + bx]
    mov [cga_rgb+8], al

    ; CGA 3 = top3_temp[2]
    mov bl, [top3_temp+2]
    mov al, [pal_r + bx]
    mov [cga_rgb+9], al
    mov al, [pal_g + bx]
    mov [cga_rgb+10], al
    mov al, [pal_b + bx]
    mov [cga_rgb+11], al

    ; --- For each BMP palette index 0-15 ---
    mov byte [sr_bmp_idx], 0

.bsr_idx:
    ; Double target RGB for sum-based comparison (no division)
    xor bh, bh
    mov bl, [sr_bmp_idx]
    xor ah, ah
    mov al, [pal_r + bx]
    shl ax, 1
    mov [sr_target_r2], ax
    mov al, [pal_g + bx]
    xor ah, ah
    shl ax, 1
    mov [sr_target_g2], ax
    mov al, [pal_b + bx]
    xor ah, ah
    shl ax, 1
    mov [sr_target_b2], ax

    mov word [nn_best_dist], 0xFFFF

    ; Try all pairs (i, j) where i <= j (10 combinations)
    mov byte [sr_i], 0

.bsr_i:
    mov al, [sr_i]
    mov [sr_j], al              ; j starts from i

.bsr_j:
    ; Compute i*3 offset into BX
    xor bh, bh
    mov bl, [sr_i]
    mov ax, bx
    shl bx, 1
    add bx, ax                  ; BX = i * 3

    ; Compute j*3 offset into SI
    xor ah, ah
    mov al, [sr_j]
    mov si, ax
    mov cx, ax
    shl si, 1
    add si, cx                  ; SI = j * 3

    ; Distance = |2*target_r - (cga_r[i] + cga_r[j])| + G + B
    ; (using sums avoids division — relative distances preserved)

    ; Red
    xor ah, ah
    mov al, [cga_rgb + bx]
    xor dh, dh
    mov dl, [cga_rgb + si]
    add ax, dx                  ; AX = sum_r
    mov dx, [sr_target_r2]
    sub dx, ax                  ; DX = 2*target_r - sum_r
    test dx, dx
    jns .bsr_r_pos
    neg dx
.bsr_r_pos:
    mov [sr_dist], dx

    ; Green
    xor ah, ah
    mov al, [cga_rgb + bx + 1]
    xor dh, dh
    mov dl, [cga_rgb + si + 1]
    add ax, dx
    mov dx, [sr_target_g2]
    sub dx, ax
    test dx, dx
    jns .bsr_g_pos
    neg dx
.bsr_g_pos:
    add [sr_dist], dx

    ; Blue
    xor ah, ah
    mov al, [cga_rgb + bx + 2]
    xor dh, dh
    mov dl, [cga_rgb + si + 2]
    add ax, dx
    mov dx, [sr_target_b2]
    sub dx, ax
    test dx, dx
    jns .bsr_b_pos
    neg dx
.bsr_b_pos:
    add [sr_dist], dx

    ; Better than best?
    mov ax, [sr_dist]
    cmp ax, [nn_best_dist]
    jae .bsr_not_better
    mov [nn_best_dist], ax
    mov al, [sr_i]
    mov [best_pair_i], al
    mov al, [sr_j]
    mov [best_pair_j], al
.bsr_not_better:

    inc byte [sr_j]
    cmp byte [sr_j], 4
    jb .bsr_j

    inc byte [sr_i]
    cmp byte [sr_i], 4
    jb .bsr_i

    ; Store result into sierra_remap[idx*2 + phase]
    ; Even row: phase 0 = color_i, phase 1 = color_j
    ; Odd row:  swap (so checkerboard inverts each row)
    xor bh, bh
    mov bl, [sr_bmp_idx]
    shl bx, 1                   ; BX = idx * 2

    test byte [current_row], 1
    jnz .bsr_odd_row

    mov al, [best_pair_i]
    mov [sierra_remap + bx], al
    mov al, [best_pair_j]
    mov [sierra_remap + bx + 1], al
    jmp .bsr_stored

.bsr_odd_row:
    mov al, [best_pair_j]
    mov [sierra_remap + bx], al
    mov al, [best_pair_i]
    mov [sierra_remap + bx + 1], al

.bsr_stored:
    inc byte [sr_bmp_idx]
    cmp byte [sr_bmp_idx], 16
    jb .bsr_idx

    popa
    ret

; ============================================================================
; find_nearest_rgb - Nearest CGA color from adjusted RGB values
; ============================================================================
; Like find_nearest but uses temp_adj_r/g/b instead of a palette index.
; Used by Bayer, 1D error diffusion, and Sierra stipple dithering.
;
; Input:  temp_adj_r, temp_adj_g, temp_adj_b = adjusted RGB (0-255)
;         top3_temp[0..2] = available color palette indices
;         e0_temp = Color D palette index
; Output: AL = CGA value (0-3)
; ============================================================================
find_nearest_rgb:
    push bx
    push cx
    push dx
    push si

    ; Distance to Color D (e0_temp) → CGA 0
    mov al, [e0_temp]
    xor ah, ah
    mov si, ax
    call compute_dist_adj_si
    mov [nn_best_dist], dx
    mov byte [nn_best_cga], 0

    ; Distance to top3[0] → CGA 1
    mov al, [top3_temp]
    xor ah, ah
    mov si, ax
    call compute_dist_adj_si
    cmp dx, [nn_best_dist]
    jae .fnr_try_2
    mov [nn_best_dist], dx
    mov byte [nn_best_cga], 1

.fnr_try_2:
    mov al, [top3_temp + 1]
    xor ah, ah
    mov si, ax
    call compute_dist_adj_si
    cmp dx, [nn_best_dist]
    jae .fnr_try_3
    mov [nn_best_dist], dx
    mov byte [nn_best_cga], 2

.fnr_try_3:
    mov al, [top3_temp + 2]
    xor ah, ah
    mov si, ax
    call compute_dist_adj_si
    cmp dx, [nn_best_dist]
    jae .fnr_done
    mov byte [nn_best_cga], 3

.fnr_done:
    mov al, [nn_best_cga]

    pop si
    pop dx
    pop cx
    pop bx
    ret

; ============================================================================
; compute_dist_adj_si - Manhattan distance: adjusted RGB vs palette[SI]
; ============================================================================
; Input:  SI = palette index, temp_adj_r/g/b = adjusted color
; Output: DX = |adj_r - pal_r[si]| + |adj_g - pal_g[si]| + |adj_b - pal_b[si]|
; Clobbers: AX
; ============================================================================
compute_dist_adj_si:
    xor dx, dx

    mov al, [temp_adj_r]
    sub al, [pal_r + si]
    jns .cda_r_pos
    neg al
.cda_r_pos:
    xor ah, ah
    add dx, ax

    mov al, [temp_adj_g]
    sub al, [pal_g + si]
    jns .cda_g_pos
    neg al
.cda_g_pos:
    xor ah, ah
    add dx, ax

    mov al, [temp_adj_b]
    sub al, [pal_b + si]
    jns .cda_b_pos
    neg al
.cda_b_pos:
    xor ah, ah
    add dx, ax

    ret

; ============================================================================
; get_cga_rgb - Get RGB of a CGA color value
; ============================================================================
; Input:  AL = CGA value (0-3)
; Output: chosen_r, chosen_g, chosen_b set
; ============================================================================
get_cga_rgb:
    push bx

    cmp al, 0
    je .gcr_0
    cmp al, 1
    je .gcr_1
    cmp al, 2
    je .gcr_2

    ; CGA 3 = top3[2]
    xor bh, bh
    mov bl, [top3_temp + 2]
    jmp .gcr_set

.gcr_0:
    ; CGA 0 = Color D (e0_temp)
    xor bh, bh
    mov bl, [e0_temp]
    jmp .gcr_set

.gcr_1:
    xor bh, bh
    mov bl, [top3_temp]
    jmp .gcr_set

.gcr_2:
    xor bh, bh
    mov bl, [top3_temp + 1]
    jmp .gcr_set

.gcr_set:
    mov al, [pal_r + bx]
    mov [chosen_r], al
    mov al, [pal_g + bx]
    mov [chosen_g], al
    mov al, [pal_b + bx]
    mov [chosen_b], al
    pop bx
    ret

; ============================================================================
; clamp_byte - Clamp signed 16-bit value to [0, 255]
; ============================================================================
; Input:  AX = signed 16-bit value
; Output: AL = clamped to [0, 255]
; ============================================================================
clamp_byte:
    cmp ax, 0
    jge .cb_not_neg
    xor al, al
    ret
.cb_not_neg:
    cmp ax, 255
    jle .cb_ok
    mov al, 255
.cb_ok:
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

msg_info    db 'PC1-BMP v6.0 - E0 Reprogramming', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'Displays 320x200 4-bit BMP images using', 0x0D, 0x0A
            db 'palette flip + E0 reprogramming.', 0x0D, 0x0A
            db '4 colors/line (3 in left guard zone).', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'Usage: PC1-BMP4 filename.bmp', 0x0D, 0x0A
            db '  ESC=exit, 1=none, 2=Bayer, 3=1D err, 4=stipple', 0x0D, 0x0A
            db '  H=toggle HSYNC, V=toggle VSYNC', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'By RetroErik - 2026', 0x0D, 0x0A, '$'

msg_phase1   db 'Pass 1: Analyzing...', 0x0D, 0x0A, '$'
msg_dither_hint db 'Press 1=none 2=Bayer 3=1D err 4=stipple', 0x0D, 0x0A, '$'
msg_phase2   db 'Pass 2: Rendering...', 0x0D, 0x0A, '$'
msg_file_err db 'Error: Cannot open file', 0x0D, 0x0A, '$'
msg_not_bmp  db 'Error: Not a valid BMP file', 0x0D, 0x0A, '$'
msg_format   db 'Error: BMP must be 4-bit uncompressed', 0x0D, 0x0A, '$'
msg_size     db 'Error: BMP must be 320x200', 0x0D, 0x0A, '$'

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
hsync_enabled:  db 1
vsync_enabled:  db 1

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

; Temp: build_palette_stream current/next line color indices
bps_cur_a:      db 0
bps_cur_b:      db 0
bps_cur_c:      db 0
bps_nxt_a:      db 0
bps_nxt_b:      db 0
bps_nxt_c:      db 0

; Color frequency workspace (16 words = 32 bytes)
color_count:    times 32 db 0

; Per-scanline top 3 color indices (200 rows × 3 = 600 bytes)
scanline_top3:  times 600 db 0

; Per-scanline 4th color for E0 (200 bytes: BMP palette index)
scanline_e0:    times 200 db 0

; Temp: E0 color index for current scanline (set by build_remap_table)
e0_temp:        db 0

; Pixel remap table (16 bytes, rebuilt per scanline during Pass 2)
remap_table:    times 16 db 0

; Per-line stream length (200 bytes): 0 or 12 bytes to write
; 0 = skip (no entries changed), 12 = full write (all entries changed)
stream_len:     times 200 db 0

; Global color stability analysis
color_line_count: times 32 db 0 ; 16 words: line count per BMP palette index
global_c:       db 0            ; Most common color → entries 6/7 (most stable)
global_b:       db 0            ; 2nd most common → entries 4/5

; ============================================================================
; Palette stream for render loop
; ============================================================================
; 200 lines × 16 bytes per line = 3200 bytes
;
; Each line: V6355D data for entries 0-7 (2 bytes each, 8 entries).
;   Bytes 0-1: E0 = Color D for line N+1 (immediate, no flip trick)
;   Bytes 2-3: E1 = zeros (unused)
;   Bytes 4-15: E2-E7 = flip-first interleaved N+1/N+2 colors
;
; INTERLEAVING: active entries get same-value rewrites, inactive
; entries get the next line's colors pre-loaded for the upcoming flip.
;
;   Even line: E2/E4/E6 = current (active), E3/E5/E7 = next (inactive)
;   Odd line:  E3/E5/E7 = current (active), E2/E4/E6 = next (inactive)
; ============================================================================
palette_stream: times 3200 db 0

; File I/O buffers
bmp_header:     times 128 db 0  ; BMP file header + info header + palette
row_buffer:     times 164 db 0  ; BMP row input (160 bytes + safety margin)

; ============================================================================
; DATA - Dithering
; ============================================================================

dither_mode:    db 0            ; 0=none, 1=Bayer, 2=1D err, 3=stipple

; --- Bayer 4×4 ordered dithering ---
; Threshold matrix (signed bytes), spread = 32
; Values: (bayer_value - 8) * 2, range -16 to +14
; Standard Bayer 4×4 pattern:  0  8  2 10 / 12  4 14  6 / 3 11  1  9 / 15  7 13  5
bayer_thresholds:
    db -16,   0, -12,   4      ; Row 0
    db   8,  -8,  12,  -4      ; Row 1
    db -10,   6, -14,   2      ; Row 2
    db  14,  -2,  10,  -6      ; Row 3

bayer_remap:    times 64 db 0   ; Precomputed per-row: [idx*4 + x%4] → CGA val
bayer_thresh_tmp: dw 0          ; Temp: current Bayer threshold

; --- Dithering shared temps ---
; Adjusted RGB temps (shared by Bayer, 1D error diffusion, Sierra)
temp_adj_r:     db 0
temp_adj_g:     db 0
temp_adj_b:     db 0

; Chosen color RGB (for 1D error diffusion)
chosen_r:       db 0
chosen_g:       db 0
chosen_b:       db 0

; 1D error diffusion / Sierra pixel processing state
fs_pixel_x:     dw 0            ; Current pixel X position (0-319)
fs_vram_ofs:    dw 0            ; Saved VRAM offset for pack pass

; 1D error diffusion running accumulators (signed 16-bit, reset per scanline)
err_r:          dw 0
err_g:          dw 0
err_b:          dw 0

; CGA value row buffer (320 bytes: one byte per pixel, values 0-3)
fs_cga_row:     times 320 db 0

; --- Sierra 2×2 stipple dithering ---
sierra_remap:   times 32 db 0   ; Precomputed per-row: [idx*2 + phase] → CGA val
cga_rgb:        times 12 db 0   ; RGB of 4 CGA colors (black + top3), 3 bytes each
sr_bmp_idx:     db 0            ; Temp: current BMP index in pair search
sr_i:           db 0            ; Temp: pair search i (0-3)
sr_j:           db 0            ; Temp: pair search j (i-3)
sr_target_r2:   dw 0            ; Temp: 2 × target red
sr_target_g2:   dw 0            ; Temp: 2 × target green
sr_target_b2:   dw 0            ; Temp: 2 × target blue
sr_dist:        dw 0            ; Temp: current pair distance
best_pair_i:    db 0            ; Temp: best pair first CGA color
best_pair_j:    db 0            ; Temp: best pair second CGA color

; ============================================================================
; END OF PROGRAM
; ============================================================================
