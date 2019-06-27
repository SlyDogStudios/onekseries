ca65 mguard.asm
ld65 -C mguard.cfg -o mguard.prg mguard.o
copy /b mguard.hdr+mguard.prg "Meteor Guard.nes"
pause
