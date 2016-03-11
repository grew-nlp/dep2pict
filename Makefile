PREFIX?=/usr/local
BINDIR=$(PREFIX)/bin
DATA_DIR=$(PREFIX)/share/dep2pict/

OCB_FLAGS = -use-ocamlfind -I src
OCB = ocamlbuild $(OCB_FLAGS)

VERSION = `cat VERSION`

all: native

native: src/dep2pict_glade.ml datadir
	sed -iback 's|src/dep2pict.glade|$(DATA_DIR)dep2pict.glade|g' src/dep2pict_glade.ml
	rm -f src/dep2pict_glade.mlback
	$(OCB) main.native

datadir:
	echo $(DATA_DIR) > DATA_DIR

install: native
	cp main.native $(BINDIR)/dep2pict
	mkdir -p $(DATA_DIR)
	cp src/dep2pict.glade $(DATA_DIR)
	cp -r examples $(DATA_DIR)

uninstall:
	rm -f $(BINDIR)/dep2pict
	rm -f $(DATA_DIR)/dep2pict.glade

.PHONY:	all clean byte native install uninstall

clean:
	$(OCB) -clean
	rm -f src/dep2pict_glade.ml

info:
	@echo "BINDIR   = $(BINDIR)" 
	@echo "DATA_DIR = $(DATA_DIR)" 

# glade file are not handle by ocamlbuild
src/dep2pict_glade.ml : src/dep2pict.glade
	lablgladecc2 $< > $@
