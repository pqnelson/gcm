TEX=pdflatex
BIB=bibtex
GFORT_FLAGS=-fdefault-real-8
FC=gfortran

all: doc

extract_tex:
	noweave -delay -index -latex nw/field.nw > tex/field.tex
	noweave -delay -index -latex nw/utils.nw > tex/utils.tex

doc: extract_tex
	noweave -delay -index -latex gcm.nw > gcm.tex
	$(TEX) gcm
	$(BIB) gcm
	$(TEX) gcm
	$(TEX) gcm
