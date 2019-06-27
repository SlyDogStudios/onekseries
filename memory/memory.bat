ca65 memory.asm
ld65 -C memory.cfg -o memory.prg memory.o
copy /b memory.hdr+memory.prg Memory.nes
pause
