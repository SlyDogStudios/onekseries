ca65 simone.asm
ld65 -C simone.cfg -o simone.prg simone.o
copy /b simone.hdr+simone.prg "Simone Says.nes"
pause
