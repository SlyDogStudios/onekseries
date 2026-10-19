ca65 save_points.asm
ld65 -C save_points.cfg -o save_points.prg save_points.o
copy /b save_points.hdr+save_points.prg "The Chrilogy Loop (Part 3) - Save Points.nes"
pause
