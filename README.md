# PC1-BMP

**BMP Image Viewer for Olivetti Prodest PC1**

A utility to display BMP images on the Olivetti Prodest PC1 using its hidden 160x200x16 graphics mode. It only accepts 160x200 and 320x200 in 4-bit color.
320x200 images are downscaled by removing every other horizontal pixel.

![Olivetti Prodest PC1](https://img.shields.io/badge/Platform-Olivetti%20Prodest%20PC1-blue)
![License](https://img.shields.io/badge/License-MIT-green)

## Features

- Displays 4-bit (16-color) uncompressed BMP images
- Supports both 160x200 and 320x200 resolution images
- Automatic downscaling of 320-pixel wide images to 160 pixels
- Custom palette support from BMP files
- Clean exit with CGA palette restoration
- **C64-style loading effects:**
  - Pilot/search phase with cyan/red border flashing
  - Border color cycling during image loading
  - White flash on loading completion
- **PERITEL.COM compatible** (v1.1): Preserves horizontal position setting

## Version History

### v1.2 (January 2026)
- **Fixed**: No longer overrides horizontal position set by PERITEL.COM
- Removed BIOS INT 10h call that was resetting V6355D registers
- Removed register 0x67 writes to preserve PERITEL's setting
- Run `PERITEL.COM` before `PC1-BMP` for optimal screen centering

### v1.0 (January 2026)
- Initial release
- BMP loading with C64-style effects

## Requirements

- **Hardware**: Olivetti Prodest PC1 (or compatible with V6355D video chip)
- **CPU**: NEC V40 (80186 compatible)
- **Assembler**: NASM (Netwide Assembler)

## Building

```bash
nasm -f bin -o pc1-bmp.com PC1-BMP.asm
```

## Usage

```
PC1-BMP filename.bmp
```

Press any key to exit and return to DOS.

### Help

```
PC1-BMP /?
PC1-BMP /h
```

## Supported BMP Format

- **Color Depth**: 4-bit (16 colors)
- **Compression**: Uncompressed (BI_RGB)
- **Resolution**: 160x200 (native) or 320x200 (auto-downscaled)

## Technical Details

This program utilizes the Olivetti Prodest PC1's hidden graphics mode:

- **Video Segment**: 0xB000
- **Resolution**: 160x200 pixels
- **Colors**: 16 colors with programmable palette
- **Video Chip**: V6355D LCD Controller

### Memory Layout

The PC1's graphics mode uses CGA-style interlaced memory:
- Even rows: offset 0x0000 + (row/2) × 80
- Odd rows: offset 0x2000 + (row/2) × 80

### Palette Format

The V6355D uses a 32-byte palette (16 colors × 2 bytes):
- Byte 1: Red (bits 0-2, values 0-7)
- Byte 2: Green (bits 4-6) | Blue (bits 0-2)

## Author

**RetroErik** - 2026

Created using VS Code with GitHub Copilot

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Olivetti for the unique Prodest PC1 hardware
- The retro computing community for documentation and preservation efforts
