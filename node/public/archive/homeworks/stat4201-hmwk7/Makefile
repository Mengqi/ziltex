all: hmwk7.pdf

.PHONY: all clean report

hmwk7.dvi:	hmwk7.tex
		latex hmwk7.tex

hmwk7.ps:	hmwk7.dvi
		dvips -Ppdf -G0 hmwk7.dvi
		# the -G0 fixes the ligature problem.

hmwk7.pdf:	hmwk7.ps
		ps2pdf hmwk7.ps

PDF: 		hmwk7.pdf
		xpdf hmwk7.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.ps *.pdf *.bbl *.blg *.dvi
