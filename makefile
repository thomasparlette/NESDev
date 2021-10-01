COMPILE=./Tools/cc65/bin/ca65
LINK=./Tools/cc65/bin/ld65

objects = ./bin/shooter.o

./bin/%.o: ./src/%.asm
	$(COMPILE)  -t nes -o $@ $<

shooter.nes: $(objects)
	$(LINK)  -t nes -o ./bin/$@ $<

run: shooter.nes
	./Tools/fceux/fceux.exe ./bin/$<

.PHONY: clean

clean:
	rm -f $(objects) ./bin/shooter.nes ./bin/shooter.nes.deb