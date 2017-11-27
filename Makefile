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

# Build ccti with all its dependencies
.PHONY: build
build:
	$(CPM) install                                     # Install package dependencies
	$(CPM) curry :l src/ccti.curry :save :q

# cleanup everything (including cpm folder, generated curry files for examples as well as any smt-lib dumps)
.PHONY: cleanall
cleanall:
	rm -rf src/.curry
	rm -f src/ccti
	rm -rf .cpm
	rm -rf examples/.curry
	rm -rf examples/.smt

