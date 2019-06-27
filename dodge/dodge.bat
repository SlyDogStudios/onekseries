ca65 dodge.asm
ld65 -C dodge.cfg -o dodge.prg dodge.o
copy /b dodge.hdr+dodge.prg "Debris Dodger.nes"
pause
