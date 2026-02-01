@ECHO OFF
REM Slideshow batch file for BMP images
REM Requires pc1-bmp.com to exit on keypress or have built-in timeout

ECHO BMP Slideshow - Press any key in pc1-bmp to advance
ECHO.

FOR %%F IN (*.BMP) DO (
    ECHO Showing: %%F
    pc1-bmp.com %%F
)

ECHO.
ECHO Slideshow complete!
