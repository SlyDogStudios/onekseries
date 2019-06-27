ca65 munch.asm
ld65 -C munch.cfg -o munch.prg munch.o
copy /b munch.hdr+munch.prg "Ninja Muncher.nes"
pause
