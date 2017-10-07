all: hmwk2.pdf

.PHONY: all clean report

hmwk2.dvi:	hmwk2.tex
		latex hmwk2.tex

hmwk2.ps:	hmwk2.dvi
		dvips -Ppdf -G0 hmwk2.dvi
		# the -G0 fixes the ligature problem.

hmwk2.pdf:	hmwk2.ps
			ps2pdf hmwk2.ps

PDF: 		hmwk2.pdf
		xpdf hmwk2.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.ps *.pdf *.bbl *.blg *.dvi
