# ACV-BMP hardware test plan

## Test media

Use known-good 4-bit, uncompressed BMP files at 160x200 and 320x200. Include images containing all 16 palette indices, a black border, and high-contrast pixels on rows 0, 1, 198, and 199.

## CGA/TTL output

1. Connect the ACV-1030 to a CGA/TTL display and boot DOS.
2. Run `ACV-BMP 160TEST.BMP` and confirm the default path displays the BMP
	indexes through the fixed CGA/TTL IRGB palette.
3. Confirm the display enters 160x200x16 mode, with even rows from the first VRAM bank and odd rows from the bank at offset `2000h`; check for no row displacement or interlace tearing.
4. Run `ACV-BMP /CGA 160TEST.BMP`. Confirm the image is remapped to the nearest fixed CGA/TTL RGB colors and that no composite palette programming is required.
5. Run the 320-pixel source image and confirm it is reduced to 160 pixels by selecting source pixels 0 and 2 from each input pair.
6. Press a key and confirm the program restores DOS text mode and returns to the command prompt.
7. Run with no filename, `/h`, and `/CGA` without a filename; confirm usage text appears without changing the video state.

## Composite output

1. Connect the ACV-1030 composite output to an NTSC composite monitor or capture device.
2. Run the 160x200 test image and confirm register `65h=01h` produces 200-line NTSC color timing.
3. Confirm the image is centered, stable, and has composite colors corresponding directly to the BMP's 16 palette entries. This viewer does not perform global best-16 palette selection.
4. Test several palette entries with saturated red, green, and blue values. Confirm palette programming is visible on composite output while the CGA/TTL result remains fixed IRGB.
5. Repeat with the 320-pixel source and verify the same downscale and row placement as on CGA/TTL.
6. Press a key and confirm the composite signal returns cleanly to DOS text mode.

## Port and CPU checks

On the listing or with a logic probe, verify that every write uses `DX` with these full ports: `3D8h`, `3D9h`, `3DDh`, and `3DEh`. Confirm initialization writes register `67h=18h`, register `65h=01h`, then `80h` to `3DDh` before `4Ah` to `3D8h`.

Run the COM on the 12 MHz 286 target. It must not depend on V40/80186-only instructions. Record display type, BMP filename, visible palette result, row alignment, and exit result for each test.
