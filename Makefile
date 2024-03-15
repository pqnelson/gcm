TEX=pdflatex
BIB=bibtex
GFORT_FLAGS=-fdefault-real-8
FC=gfortran

all: doc

extract_tex:
	noweave -delay -index -latex nw/field.nw > tex/field.tex
	noweave -delay -index -latex nw/utils.nw > tex/utils.tex
	noweave -delay -index -latex nw/boussinesq.nw > tex/boussinesq.tex

.PHONY: src
src:
	notangle -Rscalar-field-class.f90 nw/field.nw > src/scalar_field_class.f90
	notangle -Rboussinesq.f90 nw/boussinesq.nw > src/boussinesq.f90
	notangle -Rutils.f90 nw/utils.nw > src/utils.f90

doc: extract_tex
	noweave -delay -index -latex gcm.nw > gcm.tex
	$(TEX) gcm
	$(BIB) gcm
	$(TEX) gcm
	$(TEX) gcm

