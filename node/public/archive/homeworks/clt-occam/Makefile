all: occam.pdf

.PHONY: all clean report

occam.dvi:	occam.tex
		latex occam.tex

occam.ps:	occam.dvi
		dvips -Ppdf -G0 occam.dvi
		# the -G0 fixes the ligature problem.

occam.pdf:	occam.ps
		ps2pdf occam.ps

PDF: 		occam.pdf
		xpdf occam.pdf

clean:
	rm -rf *.aux *.log *.out *.toc *.ps *.pdf *.bbl *.blg *.dvi
