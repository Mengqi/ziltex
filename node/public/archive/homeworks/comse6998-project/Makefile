all: nlp-project.pdf

.PHONY: all clean report

nlp-project.dvi:	nlp-project.tex
		latex nlp-project.tex

nlp-project.ps:	nlp-project.dvi
		dvips -Ppdf -G0 nlp-project.dvi
		# the -G0 fixes the ligature problem.

nlp-project.pdf:	nlp-project.ps
			ps2pdf nlp-project.ps

PDF: 		nlp-project.pdf
		xpdf nlp-project.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.pdf *.ps *.bbl *.blg *.dvi
