PREFIX?=/usr/local
BINDIR=$(PREFIX)/bin
DATA_DIR=$(PREFIX)/share/dep2pict/

OCB_FLAGS = -use-ocamlfind -I src
OCB = ocamlbuild $(OCB_FLAGS)

VERSION = `cat VERSION`

all: native

native: datadir
	$(OCB) main.native

datadir:
	echo $(DATA_DIR) > DATA_DIR

install: native
	cp main.native $(BINDIR)/dep2pict
	mkdir -p $(DATA_DIR)/examples
	cp examples/*.dep examples/*.conll $(DATA_DIR)

uninstall:
	rm -f $(BINDIR)/dep2pict

.PHONY:	all clean byte native install uninstall

clean:
	$(OCB) -clean
	rm -f DATA_DIR

info:
	@echo "BINDIR   = $(BINDIR)"
	@echo "DATA_DIR = $(DATA_DIR)"
