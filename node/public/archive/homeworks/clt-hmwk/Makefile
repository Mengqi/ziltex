all: clt-hmwk.pdf

.PHONY: all clean report

clt-hmwk.dvi:	clt-hmwk.tex
		latex clt-hmwk.tex

clt-hmwk.ps:	clt-hmwk.dvi
		dvips -Ppdf -G0 clt-hmwk.dvi
		# the -G0 fixes the ligature problem.

clt-hmwk.pdf:	clt-hmwk.ps
		ps2pdf clt-hmwk.ps

PDF: 		clt-hmwk.pdf
		xpdf clt-hmwk.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.ps *.pdf *.bbl *.blg *.dvi
