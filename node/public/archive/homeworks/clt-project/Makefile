all: clt-project.pdf

.PHONY: all clean report

clt-project.dvi:	clt-project.tex
		latex clt-project.tex

clt-project.ps:	clt-project.dvi
		dvips -Ppdf -G0 clt-project.dvi
		# the -G0 fixes the ligature problem.

clt-project.pdf:	clt-project.ps
		ps2pdf clt-project.ps

PDF: 		clt-project.pdf
		xpdf clt-project.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.ps *.pdf *.bbl *.blg *.dvi
