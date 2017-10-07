all: hmwk8.pdf

.PHONY: all clean report

hmwk8.dvi:	hmwk8.tex
		latex hmwk8.tex

hmwk8.ps:	hmwk8.dvi
		dvips -Ppdf -G0 hmwk8.dvi
		# the -G0 fixes the ligature problem.

hmwk8.pdf:	hmwk8.ps
		ps2pdf hmwk8.ps

PDF: 		hmwk8.pdf
		xpdf hmwk8.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.ps *.pdf *.bbl *.blg *.dvi
