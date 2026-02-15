; ============================================================================
; PC1-BMP9.ASM (v 2.0) - BMP Viewer: Flip Hero Technique (Simone-calibrated)
; ============================================================================
;
; Displays 320x200 4-bit BMP images on the Olivetti Prodest PC1 using
; CGA 320x200x4 mode with per-scanline V6355D palette reprogramming.
;
; THE FLIP HERO TECHNIQUE (Simone-calibrated):
;   Combines the best of BMP4 (Simone palette flip) and BMP8 (Hero HBLANK):
;
;   - CGA palette flip alternates between palette 0 and 1 each scanline
;   - FLIP FIRST: palette switch happens IMMEDIATELY at HBLANK start
;     ("calibrated at nanosecond" — Simone). This is the critical operation.
;   - After flip, write the NOW-INACTIVE entry with hero for line N+2
;     (next same-parity line, pre-loading for the flip 2 HBLANKs ahead).
;     Inactive writes are invisible to the display.
;   - Entries 4-7 are 2 fixed global colors, SHARED across both palettes:
;     E4=E5=global_a, E6=E7=global_b. This ensures non-hero pixels show
;     the SAME color on every scanline regardless of active palette.
;
;   TIMING (all within HBLANK!):
;     Even lines: flip + open + 2×OUTSB + close ≈ 54 cycles
;     Odd lines:  flip + open + 4×OUTSB + close ≈ 82 cycles
;       (odd writes through E2 same-value passthrough to reach E3)
;
;   KEY INSIGHT (from Simone): the palette flip must be nanosecond-precise
;   at HBLANK start. The subsequent writes to INACTIVE entries are safe
;   even during HBLANK — those entries aren't being displayed.
;
;   RESULT: Zero flicker on all 200 scanlines. This is the most stable
;   display technique achieved on the Olivetti Prodest PC1.
;
;   WHY N+2, NOT N+1:
;     In flip-first mode, after flipping on line N, the inactive entry
;     won't display until the NEXT same-parity line (N+2). Line N+1
;     uses the OTHER palette, whose entry was pre-loaded by iter N-1.
;     So the stream must target the same-parity line 2 HBLANKs ahead.
;
;   PALETTE MAPPING (globals are SHARED = same color both palettes):
;     Even lines (pal 0): px0=E0(black), px1=E2(hero), px2=E4(globA), px3=E6(globB)
;     Odd lines  (pal 1): px0=E0(black), px1=E3(hero), px2=E5(globA), px3=E7(globB)
;
;   INTERLEAVING (pre-loading inactive hero for N+2):
;     Even line N: E2=hero[N+2] (inactive preload), E3=hero[N+1] (filler)
;     Odd line  N: E2=hero[N+1] (passthrough), E3=hero[N+2] (inactive preload)
;
;   GLOBAL COLOR ASSIGNMENT (same color for both palettes):
;     E4 = E5 = Global A (#1 most frequent non-black)
;     E6 = E7 = Global B (#2 most frequent non-black)
;     Only the hero color (E2/E3) differs between scanlines.
;
;   SKIP OPTIMIZATION: Lines where scanline_hero[N+2] == scanline_hero[N]
;   get ZERO palette writes — just the flip.
;
; COMPARISON:
;   BMP8 (Hero):      2 globals, 1 hero/line, ~48 cyc, zero flicker
;   BMP9 (Flip Hero): 2 globals, 1 hero/line, ≤82 cyc, zero flicker
;   BMP4 (Simone):    3 indep/line, ~198 cyc, near-zero flicker (flip-first)
;
;   BMP9 vs BMP8: Same color count, but BMP9 uses HBLANK-only timing
;   on both even AND odd lines (BMP8 only writes even). Both achieve
;   zero flicker.
;
;   BMP4 has more colors per line (3 independent vs 2 globals + 1 hero)
;   with near-zero flicker (only first scanline artifact).
;
; ALGORITHM:
;   1. Load and validate 320x200 4-bit BMP
;   2. Convert BMP 16-color BGRA palette to RGB888 + V6355D format
;   3. PASS 1: Count pixel color frequency per scanline, accumulate globals
;   4. Find 2 global colors (top 2 most frequent non-black), assign
;      identically to both palettes (E4=E5=globA, E6=E7=globB)
;   5. Hero selection per line: total-error minimization (parity-aware)
;   6. Build interleaved palette stream (4 bytes/line, targets N+2)
;   7. Build stream skip table (compare hero[N+2] vs hero[N])
;   8. PASS 2: Re-read BMP, remap pixels to CGA values, write to VRAM
;   9. Program initial palette (all 8 entries: heroes + globals)
;  10. Display loop: VSYNC + per-scanline FLIP-FIRST + HBLANK streaming
;
; LIMITATIONS:
;   - Only 320x200 4-bit uncompressed BMPs supported
;   - BMP palette index 0 is always treated as black (background)
;   - 1 hero + 2 globals + black = 3+1 colors per line
;   - 2 global colors shared across all lines (not 4 different ones)
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
;   nasm -f bin -o PC1-BMP9.com PC1-BMP9.asm
;
; ============================================================================
; USAGE
; ============================================================================
;
;   PC1-BMP9 filename.bmp
;
;   Controls:
;     ESC : Exit to DOS
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

; V6355D I/O Ports
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

; CGA mode 4 control values
CGA_MODE4_OFF   equ 0x02        ; 320x200 graphics, video OFF
CGA_MODE4_ON    equ 0x0A        ; 320x200 graphics, video ON

; Palette select values (port 0xD9)
PAL_EVEN        equ 0x00        ; Palette 0: entries {0,2,4,6}, bg = entry 0
PAL_ODD         equ 0x20        ; Palette 1: entries {0,3,5,7}, bg = entry 0

; ============================================================================
; Main Program Entry Point
; ============================================================================
main:
    cld
    push ds
    pop es

    ; --- Parse command line ---
    mov si, 0x81

.skip_spaces:
    lodsb
    cmp al, ' '
    je .skip_spaces
    cmp al, 0x0D
    je .show_usage

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
    mov byte [si], 0

    ; --- Open BMP file ---
    mov dx, [filename_ptr]
    mov ax, 0x3D00
    int 0x21
    jc .file_error
    mov [file_handle], ax

    ; --- Read header + palette (118 bytes) ---
    mov bx, ax
    mov dx, bmp_header
    mov cx, 118
    mov ah, 0x3F
    int 0x21
    jc .file_error
    cmp ax, 118
    jb .file_error

    ; --- Validate BMP ---
    cmp word [bmp_header + BMP_SIGNATURE], 0x4D42
    jne .not_bmp

    cmp word [bmp_header + BMP_BPP], 4
    jne .wrong_format

    cmp word [bmp_header + BMP_COMPRESSION], 0
    jne .wrong_format
    cmp word [bmp_header + BMP_COMPRESSION + 2], 0
    jne .wrong_format

    cmp word [bmp_header + BMP_WIDTH], 320
    jne .wrong_size
    cmp word [bmp_header + BMP_HEIGHT], 200
    jne .wrong_size

    ; --- Convert BMP palette ---
    call convert_bmp_palette

    ; --- Pass 1: Analyze image ---
    mov dx, msg_phase1
    mov ah, 0x09
    int 0x21

    mov bx, [file_handle]
    mov dx, [bmp_header + BMP_DATA_OFFSET]
    mov cx, [bmp_header + BMP_DATA_OFFSET + 2]
    mov ax, 0x4200
    int 0x21
    jc .file_error

    call analyze_image

    ; --- Find 4 global colors ---
    call find_4_global_colors

    ; --- Build hero selections + interleaved stream ---
    call precompute_base_dists
    call build_heroes
    call build_hero_stream
    call build_stream_lengths

    ; --- Pass 2: Render to VRAM ---
    mov dx, msg_phase2
    mov ah, 0x09
    int 0x21

    mov ax, 0x0004
    int 0x10
    cld

    mov al, CGA_MODE4_OFF
    out PORT_MODE, al

    mov bx, [file_handle]
    mov dx, [bmp_header + BMP_DATA_OFFSET]
    mov cx, [bmp_header + BMP_DATA_OFFSET + 2]
    mov ax, 0x4200
    int 0x21

    call render_to_vram

    ; --- Close file ---
    mov bx, [file_handle]
    mov ah, 0x3E
    int 0x21

    ; --- Program initial palette ---
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

    ; --- Cleanup and exit ---
    call set_cga_palette
    mov ax, 0x0003
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
convert_bmp_palette:
    push ax
    push bx
    push cx
    push si
    push di

    mov si, bmp_header + BMP_PALETTE_OFF
    xor di, di
    mov cx, 16

.cvt_loop:
    lodsb
    mov [pal_b + di], al

    lodsb
    mov [pal_g + di], al

    lodsb
    mov [pal_r + di], al

    mov bx, di
    shl bx, 1

    mov al, [pal_r + di]
    shr al, 5
    mov [v6355_pal + bx], al

    mov al, [pal_g + di]
    and al, 0xE0
    shr al, 1
    mov ah, al
    mov al, [pal_b + di]
    shr al, 5
    or al, ah
    mov [v6355_pal + bx + 1], al

    lodsb                       ; Skip alpha

    inc di
    loop .cvt_loop

    pop di
    pop si
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; analyze_image - Pass 1: Read all BMP rows, count colors per scanline,
;                 accumulate global pixel counts
; ============================================================================
analyze_image:
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    ; Clear global pixel counts
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

    call analyze_scanline

    ; Progress dot every 50 rows
    push ax
    push bx
    push dx
    mov ax, [current_row]
    xor dx, dx
    mov bx, 50
    div bx
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
; analyze_scanline - Count colors, accumulate globals, save full counts
; ============================================================================
analyze_scanline:
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    ; Clear color counts
    mov di, color_count
    mov cx, 16
.clr_counts:
    mov word [di], 0
    add di, 2
    loop .clr_counts

    ; Count pixel colors
    mov si, row_buffer
    mov cx, BMP_ROW_BYTES

.count_loop:
    lodsb
    mov ah, al

    shr al, 4
    xor bx, bx
    mov bl, al
    shl bx, 1
    inc word [color_count + bx]

    mov al, ah
    and al, 0x0F
    xor bx, bx
    mov bl, al
    shl bx, 1
    inc word [color_count + bx]

    loop .count_loop

    ; Exclude index 0
    mov word [color_count], 0

    ; Accumulate into global pixel counts
    mov si, color_count
    mov di, global_pixel_count
    mov cx, 16
.accum_global:
    mov ax, [si]
    add [di], ax
    add si, 2
    add di, 2
    loop .accum_global

    ; Save full color counts for hero selection
    mov ax, [current_row]
    shl ax, 5                   ; row × 32
    mov di, scanline_counts
    add di, ax
    mov si, color_count
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
; find_4_global_colors - Find top 2 most frequent non-black colors
; ============================================================================
; Output:
;   global_even_a = global_odd_a = #1 most frequent → E4, E5 (pixel 2)
;   global_even_b = global_odd_b = #2 most frequent → E6, E7 (pixel 3)
;
; The globals MUST be the same for both palettes! Only the hero (E2/E3)
; differs between even and odd palettes. If globals differed, non-hero
; pixels would show different colors on alternating lines → visible
; striping/wrong colors.
;
; Uses global_pixel_count[] accumulated during analyze_image.
; ============================================================================
find_4_global_colors:
    push ax
    push bx
    push cx
    push dx

    ; Copy global_pixel_count to temp (so we can zero out found colors)
    mov si, global_pixel_count
    mov di, fgc_temp_counts
    mov cx, 16
.fgc_copy:
    mov ax, [si]
    mov [di], ax
    add si, 2
    add di, 2
    loop .fgc_copy

    ; Exclude black (index 0)
    mov word [fgc_temp_counts], 0

    ; --- Find #1 (most frequent) → global_a (both palettes) ---
    call .fgc_find_max
    mov [global_even_a], al
    mov [global_odd_a], al          ; Same color for odd palette
    xor bx, bx
    mov bl, al
    shl bx, 1
    mov word [fgc_temp_counts + bx], 0

    ; --- Find #2 → global_b (both palettes) ---
    call .fgc_find_max
    mov [global_even_b], al
    mov [global_odd_b], al          ; Same color for odd palette

    ; Fallback: if global_b is 0, use global_a
    cmp byte [global_even_b], 0
    jne .fgc_done
    mov al, [global_even_a]
    mov [global_even_b], al
    mov [global_odd_b], al

.fgc_done:
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; Internal: find max in fgc_temp_counts, return AL = index
.fgc_find_max:
    xor ax, ax
    xor dx, dx
    mov bx, 2
    mov cx, 15
.fgc_scan:
    cmp [fgc_temp_counts + bx], dx
    jbe .fgc_not
    mov dx, [fgc_temp_counts + bx]
    mov ax, bx
    shr ax, 1
.fgc_not:
    add bx, 2
    loop .fgc_scan
    ret

; ============================================================================
; precompute_base_dists - Compute base distances for both parities
; ============================================================================
; base_dist_even[c] = min(dist(c,black), dist(c,global_even_a), dist(c,global_even_b))
; base_dist_odd[c]  = min(dist(c,black), dist(c,global_odd_a), dist(c,global_odd_b))
; ============================================================================
precompute_base_dists:
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    ; --- Even palette base distances ---
    mov al, [global_even_a]
    mov [pbd_ga], al
    mov al, [global_even_b]
    mov [pbd_gb], al
    mov word [pbd_dst], base_dist_even
    call .pbd_compute

    ; --- Odd palette base distances ---
    mov al, [global_odd_a]
    mov [pbd_ga], al
    mov al, [global_odd_b]
    mov [pbd_gb], al
    mov word [pbd_dst], base_dist_odd
    call .pbd_compute

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; Internal: compute one base_dist table using pbd_ga, pbd_gb → [pbd_dst]
.pbd_compute:
    xor cx, cx
    mov di, [pbd_dst]

.pbd_loop:
    mov bx, cx

    ; Distance to black (0,0,0) = R + G + B
    xor dx, dx
    mov al, [pal_r + bx]
    xor ah, ah
    add dx, ax
    mov al, [pal_g + bx]
    add dx, ax
    mov al, [pal_b + bx]
    add dx, ax
    mov [di], dx

    ; Distance to global A
    xor ah, ah
    mov al, [pbd_ga]
    mov si, ax
    call compute_dist_bx_si
    cmp dx, [di]
    jae .pbd_try_b
    mov [di], dx

.pbd_try_b:
    ; Distance to global B
    xor ah, ah
    mov al, [pbd_gb]
    mov si, ax
    call compute_dist_bx_si
    cmp dx, [di]
    jae .pbd_next
    mov [di], dx

.pbd_next:
    add di, 2
    inc cx
    cmp cx, 16
    jb .pbd_loop
    ret

; ============================================================================
; build_heroes - Total-error-minimization hero selection (parity-aware)
; ============================================================================
; For each scanline, selects the hero color that minimizes total pixel error.
; Even lines use {black, hero, global_even_a, global_even_b}.
; Odd lines use {black, hero, global_odd_a, global_odd_b}.
; Output: scanline_hero[200]
; ============================================================================
build_heroes:
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    xor cx, cx                  ; CX = current row (0-199)

.bh_row_loop:
    mov [bps_row], cx

    ; Determine parity and set active globals + base distances
    test cl, 1
    jnz .bh_odd_line

    ; Even line
    mov al, [global_even_a]
    mov [active_ga], al
    mov al, [global_even_b]
    mov [active_gb], al
    mov word [active_base_ptr], base_dist_even
    jmp .bh_eval_start

.bh_odd_line:
    mov al, [global_odd_a]
    mov [active_ga], al
    mov al, [global_odd_b]
    mov [active_gb], al
    mov word [active_base_ptr], base_dist_odd

.bh_eval_start:
    ; Compute pointer to this row's scanline_counts
    mov ax, cx
    shl ax, 5
    add ax, scanline_counts
    mov [bps_counts_ptr], ax

    ; Initialize best total error to max (32-bit)
    mov word [bps_best_err_lo], 0xFFFF
    mov word [bps_best_err_hi], 0xFFFF
    mov byte [bps_best_idx], 0

    ; Evaluate each candidate hero (colors 1-15, skip active globals)
    mov byte [bps_candidate], 1

.bh_eval_hero:
    mov al, [bps_candidate]

    ; Skip active globals
    cmp al, [active_ga]
    je .bh_next_hero
    cmp al, [active_gb]
    je .bh_next_hero

    ; Skip if no pixels on this line
    xor bx, bx
    mov bl, al
    shl bx, 1
    mov si, [bps_counts_ptr]
    cmp word [si + bx], 0
    je .bh_next_hero

    ; --- Compute total error for this hero candidate ---
    mov word [bps_total_lo], 0
    mov word [bps_total_hi], 0

    xor ah, ah
    mov al, [bps_candidate]
    mov [bps_hero_w], ax

    ; Evaluate all colors 1-15
    mov byte [bps_color], 1

.bh_eval_color:
    xor bx, bx
    mov bl, [bps_color]
    shl bx, 1
    mov si, [bps_counts_ptr]
    mov ax, [si + bx]
    or ax, ax
    jz .bh_next_color
    mov [bps_eval_count], ax

    ; Distance from this color to hero candidate
    xor bx, bx
    mov bl, [bps_color]
    mov si, [bps_hero_w]
    call compute_dist_bx_si

    ; Effective distance = min(base_dist[color], dist_to_hero)
    push bx
    xor bx, bx
    mov bl, [bps_color]
    shl bx, 1
    mov si, [active_base_ptr]
    cmp dx, [si + bx]
    jbe .bh_hero_closer
    mov dx, [si + bx]
.bh_hero_closer:
    pop bx

    ; Accumulate: total_error += count × effective_dist (32-bit)
    mov ax, [bps_eval_count]
    mul dx
    add [bps_total_lo], ax
    adc [bps_total_hi], dx

.bh_next_color:
    inc byte [bps_color]
    cmp byte [bps_color], 16
    jb .bh_eval_color

    ; Compare with best (lower = better)
    mov ax, [bps_total_hi]
    cmp ax, [bps_best_err_hi]
    ja .bh_next_hero
    jb .bh_new_best
    mov ax, [bps_total_lo]
    cmp ax, [bps_best_err_lo]
    jae .bh_next_hero

.bh_new_best:
    mov ax, [bps_total_lo]
    mov [bps_best_err_lo], ax
    mov ax, [bps_total_hi]
    mov [bps_best_err_hi], ax
    mov al, [bps_candidate]
    mov [bps_best_idx], al

.bh_next_hero:
    inc byte [bps_candidate]
    cmp byte [bps_candidate], 16
    jb .bh_eval_hero

    ; Store best hero for this row
    mov al, [bps_best_idx]
    or al, al
    jnz .bh_hero_ok
    mov al, [active_ga]         ; Fallback
.bh_hero_ok:
    mov cx, [bps_row]
    mov bx, cx
    mov [scanline_hero + bx], al

    inc cx
    cmp cx, 200
    jb .bh_row_loop

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; build_hero_stream - Build stream: pre-load INACTIVE hero for N+2
; ============================================================================
; FLIP-FIRST APPROACH:
;   Each HBLANK: flip palette FIRST (nanosecond-critical, reveals pre-loaded
;   hero instantly), then write only the NOW-INACTIVE entry with the hero
;   for the next SAME-PARITY line (N+2), pre-loading for 2 flips ahead.
;
;   After flipping to pal 1 (E3 active): write E2 (inactive) = hero[N+2]
;   After flipping to pal 0 (E2 active): write through E2 (passthrough)
;                                         then E3 (inactive) = hero[N+2]
;
;   For E2-only writes: 2 bytes (open at 0x44, 2×OUTSB)
;   For E3 writes: 4 bytes (open at 0x44, 4×OUTSB through E2)
;   E2 passthrough on odd lines: hero[N+1] = what prev even iter wrote
;   to E2 (hero[(N-1)+2] = hero[N+1]). Safe HBLANK same-value rewrite.
;
;   Stream layout: 4 bytes per line (E2_R, E2_GB, E3_R, E3_GB)
;   On even lines (just flipped to pal 1): uses only first 2 bytes (E2)
;   On odd lines (just flipped to pal 0): uses all 4 bytes (E2 pass + E3 new)
;
;   Stream stores hero[N+2] in the inactive slot:
;     Line N: after flip, inactive entry gets hero_{N+2}
;   Passthrough stores hero[N+1] (current E2 value).
;
; Target line clamped to 199.
; ============================================================================
build_hero_stream:
    push ax
    push bx
    push cx
    push dx
    push di

    xor cx, cx
    mov di, palette_stream

.bhs_loop:
    ; After line N's flip, we need to pre-load hero for line N+2
    ; into the now-INACTIVE entry (next same-parity line).
    ; hero[N+1] was already pre-loaded by the previous iteration.

    ; Get inactive hero (line CX+2, clamped to 199)
    mov bx, cx
    add bx, 2
    cmp bx, 200
    jb .bhs_inact_ok
    mov bx, 199
.bhs_inact_ok:
    mov al, [scanline_hero + bx]
    xor ah, ah
    mov bx, ax
    shl bx, 1
    mov ax, [v6355_pal + bx]       ; AX = inactive hero V6355D word
    mov [bhs_inact_w], ax

    ; Get passthrough hero (line CX+1, clamped to 199)
    ; On odd lines, E2 must be written through to reach E3.
    ; E2 currently = hero[N+1] (set by prev even iter: hero[(N-1)+2]).
    mov bx, cx
    inc bx
    cmp bx, 200
    jb .bhs_pass_ok
    mov bx, 199
.bhs_pass_ok:
    mov al, [scanline_hero + bx]
    xor ah, ah
    mov bx, ax
    shl bx, 1
    mov ax, [v6355_pal + bx]       ; AX = passthrough hero V6355D word
    mov [bhs_pass_w], ax

    test cl, 1
    jnz .bhs_odd

    ; Even line N: just flipped to pal 1 (E3 active, E2 inactive)
    ; Write E2 = hero_{N+2} (pre-load for next even line). Only 2 bytes.
    mov ax, [bhs_inact_w]
    mov [di], ax                    ; E2: hero[N+2] (pre-load into inactive)
    mov ax, [bhs_pass_w]
    mov [di + 2], ax                ; E3: hero[N+1] (not written on even lines)
    jmp .bhs_next

.bhs_odd:
    ; Odd line N: just flipped to pal 0 (E2 active, E3 inactive)
    ; Must write through E2 (passthrough) to reach E3.
    ; E2 = hero[N+1] (same-value rewrite, matches current E2, safe in HBLANK)
    ; E3 = hero[N+2] (pre-load for next odd line)
    mov ax, [bhs_pass_w]
    mov [di], ax                    ; E2: hero[N+1] passthrough
    mov ax, [bhs_inact_w]
    mov [di + 2], ax                ; E3: hero[N+2] (pre-load into inactive)

.bhs_next:
    add di, 4
    inc cx
    cmp cx, 200
    jb .bhs_loop

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ============================================================================
; build_stream_lengths - Determine per-line skip/write flag
; ============================================================================
; Compares scanline_hero[N+2] vs scanline_hero[N] (same-parity lines).
; If the inactive entry's hero hasn't changed → stream_len = 0 (skip)
; Otherwise → stream_len for even = 2, odd = 4
; Line 0 always writes.
; ============================================================================
build_stream_lengths:
    push ax
    push bx
    push si

    ; Line 0: always write (even → 2 bytes for E2)
    mov byte [stream_len], 2

    ; Lines 1-199: compare hero[N+2] vs hero[N]
    mov si, 1                       ; Start at line 1

.bsl_loop:
    ; Get hero[N+2] (clamped to 199)
    mov bx, si
    add bx, 2
    cmp bx, 200
    jb .bsl_clamp_ok
    mov bx, 199
.bsl_clamp_ok:
    mov al, [scanline_hero + bx]    ; hero[N+2]
    cmp al, [scanline_hero + si]    ; hero[N]
    jne .bsl_write

    ; Same hero — skip this line
    mov byte [stream_len + si], 0
    jmp .bsl_next

.bsl_write:
    test si, 1
    jnz .bsl_write_odd
    mov byte [stream_len + si], 2   ; even line: 2 bytes (E2 only)
    jmp .bsl_next

.bsl_write_odd:
    mov byte [stream_len + si], 4   ; odd line: 4 bytes (E2 + E3)

.bsl_next:
    inc si
    cmp si, 200
    jb .bsl_loop

    pop si
    pop bx
    pop ax
    ret

; ============================================================================
; render_to_vram - Pass 2: Read BMP, remap pixels, write to CGA VRAM
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

    call build_remap_table

    ; CGA VRAM offset (interlaced)
    mov ax, [current_row]
    push ax
    shr ax, 1
    mov bx, CGA_ROW_BYTES
    mul bx
    mov di, ax
    pop ax
    test al, 1
    jz .rv_even
    add di, 0x2000
.rv_even:

    ; Convert 4bpp → 2bpp
    mov si, row_buffer
    mov bx, remap_table
    mov cx, CGA_ROW_BYTES

.rv_convert:
    xor dh, dh

    ; Pixel 0 (bits 7-6)
    lodsb
    mov dl, al
    shr al, 4
    xlat
    or dh, al
    shl dh, 2

    ; Pixel 1 (bits 5-4)
    mov al, dl
    and al, 0x0F
    xlat
    or dh, al
    shl dh, 2

    ; Pixel 2 (bits 3-2)
    lodsb
    mov dl, al
    shr al, 4
    xlat
    or dh, al
    shl dh, 2

    ; Pixel 3 (bits 1-0)
    mov al, dl
    and al, 0x0F
    xlat
    or dh, al

    mov al, dh
    stosb

    loop .rv_convert

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
; build_remap_table - Parity-aware: even lines use even globals, odd use odd
; ============================================================================
; CGA value mapping:
;   0 = black (entry 0)
;   1 = hero (entry 2 or 3, per-scanline)
;   2 = global A or B (entry 4 or 5, fixed per parity)
;   3 = global C or D (entry 6 or 7, fixed per parity)
; ============================================================================
build_remap_table:
    push ax
    push bx
    push cx
    push dx
    push si

    ; Load this scanline's hero
    mov bx, [current_row]
    mov al, [scanline_hero + bx]
    mov [top3_temp], al

    ; Load parity-appropriate globals
    mov ax, [current_row]
    test al, 1
    jnz .brt_odd

    ; Even line: global_even_a (E4), global_even_b (E6)
    mov al, [global_even_a]
    mov [top3_temp + 1], al
    mov al, [global_even_b]
    mov [top3_temp + 2], al
    jmp .brt_map

.brt_odd:
    ; Odd line: global_odd_a (E5), global_odd_b (E7)
    mov al, [global_odd_a]
    mov [top3_temp + 1], al
    mov al, [global_odd_b]
    mov [top3_temp + 2], al

.brt_map:
    xor cx, cx

.brt_loop:
    or cl, cl
    jz .brt_black

    cmp cl, [top3_temp]
    je .brt_1
    cmp cl, [top3_temp + 1]
    je .brt_2
    cmp cl, [top3_temp + 2]
    je .brt_3

    call find_nearest
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
; find_nearest - Map BMP palette index to nearest available CGA value
; ============================================================================
find_nearest:
    push bx
    push cx
    push dx
    push si

    xor bh, bh
    mov bl, cl

    ; Distance to black
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

    ; Distance to hero (top3[0])
    mov al, [top3_temp]
    or al, al
    jz .fn_done
    xor ah, ah
    mov si, ax
    call compute_dist_bx_si
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
; compute_dist_bx_si - RGB888 Manhattan distance
; Input:  BX = palette index 1, SI = palette index 2
; Output: DX = |R1-R2| + |G1-G2| + |B1-B2|
; ============================================================================
compute_dist_bx_si:
    xor dx, dx

    mov al, [pal_r + bx]
    sub al, [pal_r + si]
    jns .cd_r_pos
    neg al
.cd_r_pos:
    xor ah, ah
    add dx, ax

    mov al, [pal_g + bx]
    sub al, [pal_g + si]
    jns .cd_g_pos
    neg al
.cd_g_pos:
    xor ah, ah
    add dx, ax

    mov al, [pal_b + bx]
    sub al, [pal_b + si]
    jns .cd_b_pos
    neg al
.cd_b_pos:
    xor ah, ah
    add dx, ax

    ret

; ============================================================================
; read_bmp_row
; ============================================================================
read_bmp_row:
    push bx
    push cx
    push dx
    push es

    mov bx, [file_handle]
    mov dx, row_buffer
    mov cx, BMP_ROW_BYTES
    mov ah, 0x3F
    int 0x21

    pop es
    pop dx
    pop cx
    pop bx
    ret

; ============================================================================
; render_frame - FLIP-FIRST Technique (Simone-calibrated)
; ============================================================================
; FLIP FIRST, then write INACTIVE entry:
;
;   Per scanline during HBLANK:
;     1. FLIP palette immediately (nanosecond-critical! Reveals pre-loaded hero)
;     2. Check stream_len — skip write if 0 (inactive hero unchanged)
;     3. Write now-INACTIVE entry with next line's hero (pre-load)
;
;   FLIP-FIRST ensures the palette switch happens at the earliest possible
;   moment in HBLANK. The subsequent write targets the NOW-INACTIVE entry,
;   so any write protocol disruption is invisible.
;
;   Even lines (after flip to pal 1): E2 is inactive → 2×OUTSB (~54 cycles)
;   Odd lines  (after flip to pal 0): E3 is inactive → 4×OUTSB through E2
;     (~82 cycles — E2 gets same-value rewrite during HBLANK, harmless)
;
;   Even lines fit HBLANK entirely. Odd lines are tight but the write
;   targets only inactive entries, so any minor spillover is safe.
;
; Uses BP to index stream_len[]. SS=DS in COM file.
; ============================================================================
render_frame:
    cli
    cld
    push bp

    mov si, palette_stream
    mov cx, SCREEN_HEIGHT
    mov dx, PORT_REG_DATA       ; DX = 0xDE for OUTSB
    mov bl, PAL_ODD             ; First flip → ODD for line 1
    mov bh, PAL_EVEN

    cmp byte [hsync_enabled], 0
    je .rf_no_hsync

    ; ------------------------------------------------------------------
    ; HSYNC-synchronized loop — FLIP FIRST, then write INACTIVE
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

    ; === HBLANK START: FLIP IMMEDIATELY (nanosecond-critical!) ===
    mov al, bl
    out PORT_COLOR, al          ; Flip palette — reveals pre-loaded hero
    xchg bl, bh                 ; Prepare next flip value

    ; === Now write INACTIVE entry with next line's hero ===
    cmp byte [bp], 0
    je .rf_skip

    ; stream_len tells us how many bytes: 2 (even) or 4 (odd)
    cmp byte [bp], 2
    je .rf_write_e2_only

    ; --- Odd line: write E2 (same-value) + E3 (pre-load) = 4 bytes ---
    inc bp
    mov al, 0x44
    out PORT_REG_ADDR, al       ; Open palette at entry 2

    outsb                       ; E2 R (same-value passthrough)
    outsb                       ; E2 GB
    outsb                       ; E3 R (pre-load inactive)
    outsb                       ; E3 GB

    mov al, 0x80
    out PORT_REG_ADDR, al       ; Close palette

    loop .rf_scanline
    jmp .rf_done

.rf_write_e2_only:
    ; --- Even line: write E2 only (inactive) = 2 bytes ---
    inc bp
    mov al, 0x44
    out PORT_REG_ADDR, al       ; Open palette at entry 2

    outsb                       ; E2 R (pre-load inactive)
    outsb                       ; E2 GB

    mov al, 0x80
    out PORT_REG_ADDR, al       ; Close palette
    add si, 2                   ; Skip E3 data (not written)

    loop .rf_scanline
    jmp .rf_done

.rf_skip:
    ; === Inactive hero unchanged — zero palette writes ===
    inc bp
    add si, 4                   ; Advance past this line's stream data

    loop .rf_scanline
    jmp .rf_done

    ; ------------------------------------------------------------------
    ; Non-synchronized loop (for testing with H toggle)
    ; ------------------------------------------------------------------
.rf_no_hsync:
.rf_nosync_line:
    ; Flip first
    mov al, bl
    out PORT_COLOR, al
    xchg bl, bh

    ; Always write 4 bytes
    mov al, 0x44
    out PORT_REG_ADDR, al

    outsb                       ; E2 R
    outsb                       ; E2 GB
    outsb                       ; E3 R
    outsb                       ; E3 GB

    mov al, 0x80
    out PORT_REG_ADDR, al

    loop .rf_nosync_line

.rf_done:
    ; Reset to palette 0
    mov al, PAL_EVEN
    out PORT_COLOR, al

    pop bp
    sti
    ret

; ============================================================================
; program_initial_palette - Set all 8 V6355D entries for first frame
; ============================================================================
; Called once before enabling video. Sets:
;   Entry 0 = black (bg/border)
;   Entry 1 = black (unused)
;   Entry 2 = hero for line 0 (even palette, pixel 1)
;   Entry 3 = hero for line 1 (odd palette, pixel 1)
;   Entry 4 = global_even_a (even palette, pixel 2)
;   Entry 5 = global_odd_a (odd palette, pixel 2)
;   Entry 6 = global_even_b (even palette, pixel 3)
;   Entry 7 = global_odd_b (odd palette, pixel 3)
; ============================================================================
program_initial_palette:
    push ax
    push bx
    push si

    cli

    mov al, 0x40
    out PORT_REG_ADDR, al
    jmp short $+2

    ; Entries 0-1: black
    xor al, al
    out PORT_REG_DATA, al       ; E0 R
    jmp short $+2
    out PORT_REG_DATA, al       ; E0 GB
    jmp short $+2
    out PORT_REG_DATA, al       ; E1 R
    jmp short $+2
    out PORT_REG_DATA, al       ; E1 GB
    jmp short $+2

    ; Entry 2 = hero for line 0
    xor bx, bx
    mov bl, [scanline_hero]     ; Line 0 hero
    shl bx, 1
    mov al, [v6355_pal + bx]
    out PORT_REG_DATA, al       ; E2 R
    jmp short $+2
    mov al, [v6355_pal + bx + 1]
    out PORT_REG_DATA, al       ; E2 GB
    jmp short $+2

    ; Entry 3 = hero for line 1
    xor bx, bx
    mov bl, [scanline_hero + 1] ; Line 1 hero
    shl bx, 1
    mov al, [v6355_pal + bx]
    out PORT_REG_DATA, al       ; E3 R
    jmp short $+2
    mov al, [v6355_pal + bx + 1]
    out PORT_REG_DATA, al       ; E3 GB
    jmp short $+2

    ; Entry 4 = global_even_a
    xor bx, bx
    mov bl, [global_even_a]
    shl bx, 1
    mov al, [v6355_pal + bx]
    out PORT_REG_DATA, al       ; E4 R
    jmp short $+2
    mov al, [v6355_pal + bx + 1]
    out PORT_REG_DATA, al       ; E4 GB
    jmp short $+2

    ; Entry 5 = global_odd_a
    xor bx, bx
    mov bl, [global_odd_a]
    shl bx, 1
    mov al, [v6355_pal + bx]
    out PORT_REG_DATA, al       ; E5 R
    jmp short $+2
    mov al, [v6355_pal + bx + 1]
    out PORT_REG_DATA, al       ; E5 GB
    jmp short $+2

    ; Entry 6 = global_even_b
    xor bx, bx
    mov bl, [global_even_b]
    shl bx, 1
    mov al, [v6355_pal + bx]
    out PORT_REG_DATA, al       ; E6 R
    jmp short $+2
    mov al, [v6355_pal + bx + 1]
    out PORT_REG_DATA, al       ; E6 GB
    jmp short $+2

    ; Entry 7 = global_odd_b
    xor bx, bx
    mov bl, [global_odd_b]
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

    cmp ah, 0x01
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
    mov cx, 32

.scp_loop:
    lodsb
    out PORT_REG_DATA, al
    jmp short $+2
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

msg_info    db 'PC1-BMP9 v1.0 - Flip Hero Technique', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'Displays 320x200 4-bit BMP images using', 0x0D, 0x0A
            db 'palette flip + HBLANK hero update.', 0x0D, 0x0A
            db '4 globals + 1 hero/line, near-zero blink.', 0x0D, 0x0A
            db 0x0D, 0x0A
            db 'Usage: PC1-BMP9 filename.bmp', 0x0D, 0x0A
            db '  ESC=exit H=hsync V=vsync', 0x0D, 0x0A
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

; BMP palette RGB888 components
pal_r:          times 16 db 0
pal_g:          times 16 db 0
pal_b:          times 16 db 0

; V6355D format palette (16 colors × 2 bytes)
v6355_pal:      times 32 db 0

; Per-scanline analysis workspace
top3_temp:      times 3 db 0
nn_best_dist:   dw 0
nn_best_cga:    db 0

; 4 global colors (interleaved: #1,#3 → even; #2,#4 → odd)
global_even_a:  db 0            ; E4: most frequent (even palette, pixel 2)
global_odd_a:   db 0            ; E5: 2nd most frequent (odd palette, pixel 2)
global_even_b:  db 0            ; E6: 3rd most frequent (even palette, pixel 3)
global_odd_b:   db 0            ; E7: 4th most frequent (odd palette, pixel 3)

; Global image analysis
global_pixel_count: times 32 db 0   ; 16 words: total pixel counts per color

; Per-scanline hero color index (200 bytes)
scanline_hero:  times 200 db 0

; Color frequency workspace (16 words = 32 bytes)
color_count:    times 32 db 0

; Pixel remap table (16 bytes, rebuilt per scanline)
remap_table:    times 16 db 0

; Per-line stream length (200 bytes): 0 = skip, 4 = write
stream_len:     times 200 db 0

; Base distances for even and odd palettes (16 words each)
base_dist_even: times 32 db 0
base_dist_odd:  times 32 db 0

; Active globals for current hero evaluation
active_ga:      db 0
active_gb:      db 0
active_base_ptr: dw 0

; Temp variables for precompute_base_dists
pbd_ga:         db 0
pbd_gb:         db 0
pbd_dst:        dw 0

; Temp variables for find_4_global_colors
fgc_temp_counts: times 32 db 0

; Temp variables for build_heroes (total-error hero selection)
bps_row:         dw 0
bps_best_err_lo: dw 0
bps_best_err_hi: dw 0
bps_best_idx:    db 0
bps_candidate:   db 0
bps_total_lo:    dw 0
bps_total_hi:    dw 0
bps_hero_w:      dw 0
bps_color:       db 0
bps_eval_count:  dw 0
bps_counts_ptr:  dw 0

; Temp variables for build_hero_stream
bhs_inact_w:     dw 0
bhs_pass_w:      dw 0

; Per-scanline full color counts (200 lines × 16 colors × 2 bytes = 6400)
scanline_counts: times 6400 db 0

; Palette stream for render loop (200 lines × 4 bytes = 800 bytes)
; Each line: E2_R, E2_GB, E3_R, E3_GB (interleaved heroes)
palette_stream: times 800 db 0

; File I/O buffers
bmp_header:     times 128 db 0
row_buffer:     times 164 db 0

; ============================================================================
; END OF PROGRAM
; ============================================================================
