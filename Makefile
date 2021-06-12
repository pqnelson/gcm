TEX=pdflatex
BIB=bibtex

all: doc

extract:
	noweave -delay -index -latex nw/field.nw > tex/field.tex

doc: extract
	noweave -delay -index -latex gcm.nw > gcm.tex
	$(TEX) gcm
	$(BIB) gcm
	$(TEX) gcm
	$(TEX) gcm
