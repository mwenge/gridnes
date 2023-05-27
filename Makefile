.PHONY: all clean run

NES_IMAGE = "bin/gridrunner.nes"
ONLINE_NES_IMAGE = "docs/gridrunner.nes"
FCEUX = fceux

all: clean run

gridrunner.nes:
	ca65 -g src/gridrunner.asm -l bin/gridrunner.lst -o bin/gridrunner.o
	ld65 -o $(NES_IMAGE) -C gridrunner.cfg -m bin/gridrunner.map.txt bin/gridrunner.o -Ln bin/gridrunner.labels.txt --dbgfile bin/gridrunner.nes.test.dbg
	cp ${NES_IMAGE} ${ONLINE_NES_IMAGE}
	#python3 fceux_symbols.py

run: gridrunner.nes
	$(FCEUX) $(NES_IMAGE)

clean:
	-rm $(NES_IMAGE)
	-rm bin/*.txt
	-rm bin/*.o
	-rm bin/*.nl
	-rm bin/*.lst
	-rm bin/*.dbg
