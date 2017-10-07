all: hmwk4.pdf

.PHONY: all clean report

hmwk4.dvi:	hmwk4.tex
		latex hmwk4.tex

hmwk4.ps:	hmwk4.dvi
		dvips -Ppdf -G0 hmwk4.dvi
		# the -G0 fixes the ligature problem.

hmwk4.pdf:	hmwk4.ps
		ps2pdf hmwk4.ps

PDF: 		hmwk4.pdf
		xpdf hmwk4.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.pdf *.bbl *.blg *.dvi
