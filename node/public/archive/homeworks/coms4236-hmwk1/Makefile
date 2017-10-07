all: coms4236-hmwk1.pdf

.PHONY: all clean report

coms4236-hmwk1.dvi:	coms4236-hmwk1.tex
		latex coms4236-hmwk1.tex

coms4236-hmwk1.ps:	coms4236-hmwk1.dvi
		dvips -Ppdf -G0 coms4236-hmwk1.dvi
		# the -G0 fixes the ligature problem.

coms4236-hmwk1.pdf:	coms4236-hmwk1.ps
			ps2pdf coms4236-hmwk1.ps

PDF: 		coms4236-hmwk1.pdf
		xpdf coms4236-hmwk1.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.pdf *.bbl *.blg *.dvi
