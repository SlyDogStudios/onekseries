ca65 obstacle.asm
ld65 -C obstacle.cfg -o obstacle.prg obstacle.o
copy /b obstacle.hdr+obstacle.prg "Obstacle Trek.nes"
pause
