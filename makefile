COMPILE=ca65
LINK=ld65

objects = shooter.o

%.o: %.asm
	$(COMPILE) -o $@ $<

shooter.nes: $(objects)
	$(LINK) -t nes -o $@ $<

run: shooter.nes
	../fceux64/fceux64.exe $<

.PHONY: clean

clean:
	rm -f $(objects) output/shooter.nes 