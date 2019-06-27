ca65 gsm.asm
ld65 -C gsm.cfg -o gsm.prg gsm.o
copy /b gsm.hdr+gsm.prg "GSM.nes"
pause
