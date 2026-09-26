ca65 easter.asm
ld65 -C easter.cfg -o easter.prg easter.o
copy /b easter.hdr+easter.prg "Your First Easter.nes"
pause
