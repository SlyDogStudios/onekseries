ca65 no_points.asm
ld65 -C no_points.cfg -o no_points.prg no_points.o
copy /b no_points.hdr+no_points.prg "No Points.nes"
pause
