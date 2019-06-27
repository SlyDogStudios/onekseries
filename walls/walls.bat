ca65 walls.asm
ld65 -C walls.cfg -o walls.prg walls.o
copy /b walls.hdr+walls.prg "The One with the Walls.nes"
pause
