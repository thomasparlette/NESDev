COMPILE=ca65
LINK=ld65

objects = shooter.o

/src/%.o: /src/%.asm
	$(COMPILE) -v -t nes -o $@ $<

shooter.nes: ./src/$(objects)
	$(LINK) -v -t nes -o $@ $<

run: shooter.nes
	../fceux64/fceux64.exe $<

.PHONY: clean

clean:
	rm -f $(objects) shooter.nes shooter.nes.deb