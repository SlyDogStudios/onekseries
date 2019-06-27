ca65 bomb.asm
ld65 -C bomb.cfg -o bomb.prg bomb.o
copy /b bomb.hdr+bomb.prg "Bomb Array.nes"
pause
