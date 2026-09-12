ca65 roshambo.asm
ld65 -C roshambo.cfg -o roshambo.prg roshambo.o
copy /b roshambo.hdr+roshambo.prg Roshambo.nes
pause
