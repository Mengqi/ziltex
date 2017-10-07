all: hmwk10.pdf

.PHONY: all clean report

hmwk10.dvi:	hmwk10.tex
		latex hmwk10.tex

hmwk10.ps:	hmwk10.dvi
		dvips -Ppdf -G0 hmwk10.dvi
		# the -G0 fixes the ligature problem.

hmwk10.pdf:	hmwk10.ps
		ps2pdf hmwk10.ps

PDF: 		hmwk10.pdf
		xpdf hmwk10.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.ps *.pdf *.bbl *.blg *.dvi
