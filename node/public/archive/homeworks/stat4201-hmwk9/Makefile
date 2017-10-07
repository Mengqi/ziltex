all: hmwk9.pdf

.PHONY: all clean report

hmwk9.dvi:	hmwk9.tex
		latex hmwk9.tex

hmwk9.ps:	hmwk9.dvi
		dvips -Ppdf -G0 hmwk9.dvi
		# the -G0 fixes the ligature problem.

hmwk9.pdf:	hmwk9.ps
		ps2pdf hmwk9.ps

PDF: 		hmwk9.pdf
		xpdf hmwk9.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.ps *.pdf *.bbl *.blg *.dvi
