all: hmwk1.pdf

.PHONY: all clean report

hmwk1.dvi:	hmwk1.tex
		latex hmwk1.tex

hmwk1.ps:	hmwk1.dvi
		dvips -Ppdf -G0 hmwk1.dvi
		# the -G0 fixes the ligature problem.

hmwk1.pdf:	hmwk1.ps
			ps2pdf hmwk1.ps

PDF: 		hmwk1.pdf
		xpdf hmwk1.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.pdf *.bbl *.blg *.dvi
