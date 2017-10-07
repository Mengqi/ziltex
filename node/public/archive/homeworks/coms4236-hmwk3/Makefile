all: hmwk3.pdf

.PHONY: all clean report

hmwk3.dvi:	hmwk3.tex
		latex hmwk3.tex

hmwk3.ps:	hmwk3.dvi
		dvips -Ppdf -G0 hmwk3.dvi
		# the -G0 fixes the ligature problem.

hmwk3.pdf:	hmwk3.ps
			ps2pdf hmwk3.ps

PDF: 		hmwk3.pdf
		xpdf hmwk3.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.pdf *.ps *.bbl *.blg *.dvi *~
