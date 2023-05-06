.PHONY: all clean run

NES_IMAGE = "bin/psychedelia.nes"
FCEUX = fceux

all: clean run

psychedelia.nes:
	ca65 -g src/psychedelia.asm -l bin/psychedelia.lst -o bin/psychedelia.o
	ld65 -o $(NES_IMAGE) -C psychedelia.cfg -m bin/psychedelia.map.txt bin/psychedelia.o -Ln bin/psychedelia.labels.txt --dbgfile bin/psychedelia.nes.test.dbg
	python3 fceux_symbols.py

run: psychedelia.nes
	$(FCEUX) $(NES_IMAGE)

clean:
	-rm $(NES_IMAGE)
	-rm bin/*.txt
	-rm bin/*.o
	-rm bin/*.nl
	-rm bin/*.lst
	-rm bin/*.dbg
