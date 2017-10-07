all: report

.PHONY: all clean report

report: report.tex
	pdflatex $@
	bibtex $@
	pdflatex $@
	pdflatex $@

clean:
	rm -rf *.aux *.log *.out *.toc *.pdf *.bbl *.blg *.dvi
