ca65 invasion.asm
ld65 -C invasion.cfg -o invasion.prg invasion.o
copy /b invasion.hdr+invasion.prg "The Invasion.nes"
pause
