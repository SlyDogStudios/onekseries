ca65 dream.asm
ld65 -C dream.cfg -o dream.prg dream.o
copy /b dream.hdr+dream.prg "Tangerine Dream.nes"
pause
