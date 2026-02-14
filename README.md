# PC1-BMP

**BMP Image Viewer Suite for Olivetti Prodest PC1**

A collection of BMP image viewers for the Olivetti Prodest PC1, ranging from a simple native-mode viewer to advanced versions that use real-time V6355D palette reprogramming to display more colors than the hardware normally allows.

![Olivetti Prodest PC1](https://img.shields.io/badge/Platform-Olivetti%20Prodest%20PC1-blue)
![License](https://img.shields.io/badge/License-MIT-green)

## Programs

### PC1-BMP — Native 160×200×16 Viewer

The baseline viewer. Displays 4-bit BMP images using the PC1's hidden 160×200×16 graphics mode. Supports 160×200 (native) and 320×200 (auto-downscaled) BMPs.

- C64-style loading effects (border flashing, color cycling, completion flash)
- PERITEL.COM compatible (preserves horizontal position)
- Press any key to exit

```
PC1-BMP filename.bmp
```

### PC1-BMP2 — Hero Technique (Original)

Uses CGA 320×200×4 mode with per-scanline V6355D palette reprogramming ("Hero technique"). Two global colors are fixed across the entire image; a third "hero" color changes every scanline during HBLANK.

- 2 global colors (most frequent across image) + 1 per-scanline hero + black
- 8 individual OUTs per scanline (~99 cycles, spills ~19 cycles past HBLANK)
- **Known issue**: left-edge flicker from HBLANK spillover
- Controls: ESC=exit, H=toggle HSYNC, V=toggle VSYNC

```
PC1-BMP2 filename.bmp
```

### PC1-BMP3 — Simone Technique + OUTSB

Named after Simone, who first demonstrated palette-flip reprogramming in his Monkey Island conversion on the Olivetti PC1. Uses CGA palette flip (port 0xD9 bit 5) to alternate between two palettes each scanline. The inactive palette's entries are pre-loaded with the next line's colors via OUTSB streaming from register 0x44.

- 3 independent colors per scanline (no global colors needed)
- OUTSB streaming (12 bytes E2–E7, ~198 cycles, starts at 0x44)
- **Known artifact**: visible-area palette writes cause blinking
- Controls: ESC=exit, H=toggle HSYNC, V=toggle VSYNC

```
PC1-BMP3 filename.bmp
```

### PC1-BMP4 — Simone Technique + Adaptive Streaming

Same Simone technique as BMP3, plus stability-based color reordering and skip optimization. Colors are sorted so the most globally stable color occupies the highest palette entry. Lines where all entries match the previous line are skipped entirely (zero palette writes = zero blinking on those lines).

- Stability reorder: most stable color → slot C (entries 6/7)
- Binary skip: stream_len = 0 (skip) or 12 (full write)
- Can eliminate blinking on 30–50% of scanlines for stable images
- Controls: ESC=exit, H=toggle HSYNC, V=toggle VSYNC

```
PC1-BMP4 filename.bmp
```

### PC1-BMP5 — Hero Technique + OUTSB/Skip (Best Single-Method)

Optimized Hero technique. Starting at register 0x44 + OUTSB reduces per-scanline palette update to ~48 cycles — fits entirely within the ~80 cycle HBLANK with zero spillover. Lines where the hero color doesn't change are skipped entirely.

- 0x44 start + 2×OUTSB + close = ~48 cycles (fits in HBLANK)
- Skip-unchanged: zero palette writes on lines with same hero
- **98% flicker-free** on real hardware
- Controls: ESC=exit, H=toggle HSYNC, V=toggle VSYNC

```
PC1-BMP5 filename.bmp
```

### PC1-BMP7 — Error-Minimization Hero

Same optimized Hero technique as BMP5, but with error-minimization hero selection. Instead of picking the most frequent non-global color, scores each candidate by `pixel_count × (distance_to_nearest_global >> 2)` — prioritizing colors that would be badly approximated without the hero slot.

- Reduces visible error on lines with many competing colors
- Tends to wash out colors toward globals on some images
- Controls: ESC=exit, H=toggle HSYNC, V=toggle VSYNC

```
PC1-BMP7 filename.bmp
```

### PC1-BMP8 — 3-Method Switchable Hero (Latest)

Combines all three hero selection strategies in one viewer, switchable live via keyboard. Uses smart global selection (Global B chosen by error-weighting, not just frequency). All three hero streams are pre-computed at load time.

- **Method 1** (key 1): Frequency-based hero (BMP5-style)
- **Method 2** (key 2): Individual-need hero (BMP7-style)
- **Method 3** (key 3): Total-error minimization (default)
- Live switching: re-renders VRAM from BMP file on method change
- Controls: ESC=exit, 1/2/3=method, H=toggle HSYNC, V=toggle VSYNC

```
PC1-BMP8 filename.bmp
```

## Techniques Overview

| Technique | Programs | Colors/Line | Flicker | Notes |
|-----------|----------|-------------|---------|-------|
| Native 160×200×16 | BMP | 16 | None | No palette tricks |
| Hero (original) | BMP2 | 3+black | Left edge | 8 OUTs spill past HBLANK |
| Hero (optimized) | BMP5, BMP7, BMP8 | 3+black | ~None | 0x44+OUTSB fits in HBLANK |
| Simone (OUTSB) | BMP3 | 3+black | Moderate | Palette flip, visible-area writes |
| Simone (adaptive) | BMP4 | 3+black | Reduced | Skip-unchanged lines |

## Building

All programs are NASM COM files targeting the NEC V40 (80186 compatible):

```bash
nasm -f bin -o PC1-BMP.com PC1-BMP.asm
nasm -f bin -o PC1-BMP2.com PC1-BMP2.asm
nasm -f bin -o PC1-BMP3.com PC1-BMP3.asm
nasm -f bin -o PC1-BMP4.com PC1-BMP4.asm
nasm -f bin -o PC1-BMP5.com PC1-BMP5.asm
nasm -f bin -o PC1-BMP7.com PC1-BMP7.asm
nasm -f bin -o PC1-BMP8.com PC1-BMP8.asm
```

## Supported BMP Format

- **Color Depth**: 4-bit (16 colors)
- **Compression**: Uncompressed (BI_RGB)
- **Resolution**: 320×200 (BMP2–BMP8) or 160×200 / 320×200 (BMP)

## Requirements

- **Hardware**: Olivetti Prodest PC1 (or compatible with V6355D video chip)
- **CPU**: NEC V40 (80186 compatible) @ 8 MHz
- **Assembler**: NASM (Netwide Assembler)

## Technical Details

### V6355D Palette Protocol

The Yamaha V6355D uses a sequential write protocol:
- **Open**: write start register (0x40 for entry 0, 0x44 for entry 2) to port 0xDD
- **Data**: write 2 bytes per entry (R, then G|B) to port 0xDE — auto-increments
- **Close**: write 0x80 to port 0xDD

Key constraints discovered during development:
- **OUTSB works**: ~14 cycles/byte with natural inter-byte gap for latching
- **OUTSW fails**: V6355D can't latch 2 bytes within one word I/O cycle
- **REP OUTSB fails**: no inter-byte gap between repetitions — too fast
- **0x44 start proven**: skip entries 0–1, begin writing at entry 2

### CGA Memory Layout

320×200×4 mode uses CGA-style interlaced memory at segment 0xB800:
- Even rows: offset 0x0000 + (row/2) × 80
- Odd rows: offset 0x2000 + (row/2) × 80

### HBLANK Budget

At 8 MHz, HBLANK provides ~80 cycles:
- Hero optimized (0x44+OUTSB): ~48 cycles → fits with margin
- Hero original (0x40+8 OUTs): ~99 cycles → ~19 cycle spillover
- Simone (0x44+12×OUTSB): ~198 cycles → ~109 cycle spillover (accepted)

## Author

**RetroErik** — 2026

Created using VS Code with GitHub Copilot

## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.
