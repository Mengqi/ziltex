all: hmwk11.pdf

.PHONY: all clean report

hmwk11.dvi:	hmwk11.tex
		latex hmwk11.tex

hmwk11.ps:	hmwk11.dvi
		dvips -Ppdf -G0 hmwk11.dvi
		# the -G0 fixes the ligature problem.

hmwk11.pdf:	hmwk11.ps
		ps2pdf hmwk11.ps

PDF: 		hmwk11.pdf
		xpdf hmwk11.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.ps *.pdf *.bbl *.blg *.dvi
