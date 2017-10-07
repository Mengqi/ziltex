all: hmwk5.pdf

.PHONY: all clean report

hmwk5.dvi:	hmwk5.tex
		latex hmwk5.tex

hmwk5.ps:	hmwk5.dvi
		dvips -Ppdf -G0 hmwk5.dvi
		# the -G0 fixes the ligature problem.

hmwk5.pdf:	hmwk5.ps
			ps2pdf hmwk5.ps

PDF: 		hmwk5.pdf
		xpdf hmwk5.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.pdf *.ps *.bbl *.blg *.dvi *~
