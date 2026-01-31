@ECHO OFF
REM Slideshow batch file for BMP images
REM Requires loadbmp.com to exit on keypress or have built-in timeout

ECHO BMP Slideshow - Press any key in loadbmp to advance
ECHO.

FOR %%F IN (*.BMP) DO (
    ECHO Showing: %%F
    loadbmp.com %%F
)

ECHO.
ECHO Slideshow complete!
