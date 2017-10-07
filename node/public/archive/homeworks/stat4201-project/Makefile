all: project.pdf

.PHONY: all clean report

project.dvi:	project.tex
		latex project.tex

project.ps:	project.dvi
		dvips -Ppdf -G0 project.dvi
		# the -G0 fixes the ligature problem.

project.pdf:	project.ps
		ps2pdf project.ps

PDF: 		project.pdf
		xpdf project.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.ps *.pdf *.bbl *.blg *.dvi
