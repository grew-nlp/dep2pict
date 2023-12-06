PREFIX?=/usr/local
BINDIR=$(PREFIX)/bin

OCB_FLAGS = -use-ocamlfind -I src
OCB = ocamlbuild $(OCB_FLAGS)

VERSION = `cat VERSION`

all: native

native:
	$(OCB) main.native

install: native
	cp main.native $(BINDIR)/dep2pict

uninstall:
	rm -f $(BINDIR)/dep2pict

.PHONY:	all clean byte native install uninstall

clean:
	$(OCB) -clean

info:
	@echo "BINDIR   = $(BINDIR)"
