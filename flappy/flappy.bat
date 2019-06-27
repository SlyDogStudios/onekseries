ca65 flappy.asm
ld65 -C flappy.cfg -o flappy.prg flappy.o
copy /b flappy.hdr+flappy.prg "Flappy Block.nes"
pause
