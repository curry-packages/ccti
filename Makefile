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
	$(CPM) link $(LOCALPKGS)/flatcurry-2.0.0           # Remove as soon as flatcurry packages were updated (pretty dependency)
	$(CPM) link $(LOCALPKGS)/flatcurry-annotated-2.0.0
	$(CPM) install                                     # Install package dependencies
	$(CPM) curry :l src/ccti.curry :save :q

# clean everything (including cpm folder and generated curry files for examples)
.PHONY: cleanall
cleanall:
	rm -rf src/.curry
	rm src/ccti
	rm -rf .cpm
	rm -rf examples/.curry

