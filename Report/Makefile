#!/bin/bash

ifeq ($(LATEX_CMD),)
LATEX_CMD=pdflatex
endif


all: pdf clean

once:
	"$(LATEX_CMD)" main

pdf:
	"$(LATEX_CMD)" main
	~/texmf/scripts/glossaries/makeglossaries main
	-bibtex main
	"$(LATEX_CMD)" main
	"$(LATEX_CMD)" main

clean:
	rm -f *.synctex.gz *.dvi *.log *.out *.gli *.glo *.gls *.ist

cleanAll: clean
	rm -f main.pdf
