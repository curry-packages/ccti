########################################################################
# Makefile for Curry Concolic Testing interpreter
########################################################################

# binary of curry package manager
export CPM = cypm

# folder with local curry packages
export LOCALPKGS = $(HOME)/curry-packages/

# Build only ccti without its dependencies
.PHONY: all
all:
	$(CPM) curry :l src/ccti.curry :save :q
	mv -f src/ccti bin/

# Build ccti with all its dependencies
.PHONY: build
build:
	$(CPM) update                           # Update/Initialize curry package db
	$(CPM) install                          # Install package dependencies
	$(CPM) curry :l src/ccti.curry :save :q

# Build and install ccti with all its dependencies
.PHONY: install
install: build
	mkdir -p bin
	mv -f src/ccti bin/

# cleanup everything (including cpm folder, generated curry files for examples as well as any smt-lib dumps)
.PHONY: cleanall
cleanall:
	rm -rf bin/
	rm -rf src/.curry
	rm -rf .cpm
	rm -rf examples/.curry
	rm -rf examples/.smt

