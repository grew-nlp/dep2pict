PREFIX?=/usr/local
BINDIR=$(PREFIX)/bin
DATA_DIR=$(PREFIX)/share/dep2pict/

OCB_FLAGS = -use-ocamlfind -I src
OCB = ocamlbuild $(OCB_FLAGS)

GUI = -pkgs 'lablgtk2, lablgtk2.rsvg, lablgtk2.glade, lablwebkit'

VERSION = `cat VERSION`

all: native

native: datadir
ifeq ($(shell ocamlfind query lablwebkit),)
	@make native_nogui
else
	@make native_gui
endif

native_nogui:
	echo "no" > GUI
	$(OCB) main.native

native_gui: src/dep2pict_glade.ml
	echo "yes" > GUI
	sed -iback 's|src/dep2pict.glade|$(DATA_DIR)dep2pict.glade|g' src/dep2pict_glade.ml
	rm -f src/dep2pict_glade.mlback
	$(OCB) $(GUI) main.native

datadir:
	echo $(DATA_DIR) > DATA_DIR

install:
ifeq ($(shell ocamlfind query lablwebkit),)
	@make install_nogui
else
	@make install_gui
endif

install_gui: native_gui
	cp main.native $(BINDIR)/dep2pict
	mkdir -p $(DATA_DIR)/examples
	cp src/dep2pict.glade $(DATA_DIR)
	cp examples/*.dep examples/*.conll $(DATA_DIR)

install_nogui: native_nogui
	cp main.native $(BINDIR)/dep2pict
	mkdir -p $(DATA_DIR)/examples
	cp examples/*.dep examples/*.conll $(DATA_DIR)

uninstall:
	rm -f $(BINDIR)/dep2pict
	rm -f $(DATA_DIR)/dep2pict.glade

.PHONY:	all clean byte native install uninstall

clean:
	$(OCB) -clean
	rm -f src/dep2pict_glade.ml
	rm -f GUI DATA_DIR

info:
	@echo "BINDIR   = $(BINDIR)" 
	@echo "DATA_DIR = $(DATA_DIR)" 

# glade file are not handle by ocamlbuild
src/dep2pict_glade.ml : src/dep2pict.glade
	lablgladecc2 $< > $@
