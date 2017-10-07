all: lec10_scribe.pdf

.PHONY: all clean report

lec10_scribe.dvi:	lec10_scribe.tex
		latex lec10_scribe.tex

lec10_scribe.ps:	lec10_scribe.dvi
		dvips -Ppdf -G0 lec10_scribe.dvi
		# the -G0 fixes the ligature problem.

lec10_scribe.pdf:	lec10_scribe.ps
		ps2pdf lec10_scribe.ps

PDF: 		lec10_scribe.pdf
		xpdf lec10_scribe.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.ps *.pdf *.bbl *.blg *.dvi
