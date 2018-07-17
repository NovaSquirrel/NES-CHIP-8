@echo off
ca65 chip.s -o chip.o -l chip.lst
ld65 -C nrom128.x chip.o -o chip.nes
pause