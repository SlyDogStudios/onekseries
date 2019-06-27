ca65 for_points.asm
ld65 -C for_points.cfg -o for_points.prg for_points.o
copy /b for_points.hdr+for_points.prg "For Points.nes"
pause
