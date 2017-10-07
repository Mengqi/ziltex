all: hmwk6.pdf

.PHONY: all clean report

hmwk6.dvi:	hmwk6.tex
		latex hmwk6.tex

hmwk6.ps:	hmwk6.dvi
		dvips -Ppdf -G0 hmwk6.dvi
		# the -G0 fixes the ligature problem.

hmwk6.pdf:	hmwk6.ps
			ps2pdf hmwk6.ps

PDF: 		hmwk6.pdf
		xpdf hmwk6.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.pdf *.ps *.bbl *.blg *.dvi *~
