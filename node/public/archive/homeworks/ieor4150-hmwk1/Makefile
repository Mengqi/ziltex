# The LaTeX Makefile

#
# To modify: Modify macro "PROGRAM"
#
# ----------------------------------------------------------

#
# The PROGRAM macro defines the name of the program or project. It
# allows the program name to be changed by editing in only one
# location.
#

PROGRAM = ieor4150_hmwk1

#
# The DVI macro defines the LaTeX compiler
#
# The PS macro defines how to generate the ps file.
#
# The PSFLAGS macro contains a list of options to be passed
# for generating the PS file.
#
# The PDF macro defines how to generate the pdf file.
#

DVI	= latex

PS	= dvips
PSFLAGS	= -Ppdf -G0
# The -G0 fixes the ligature problem

PDF	= ps2pdf

#
# Default target: the first target is the default target.
#

all: $(PROGRAM).pdf

#
# DVI target: automatically builds the dvi file.
#

$(PROGRAM).dvi:	$(PROGRAM).tex
		$(DVI) $(PROGRAM).tex

#
# PS target: automatically builds the ps file.
#

$(PROGRAM).ps:	$(PROGRAM).dvi
		$(PS) $(PSFLAGS) $(PROGRAM).dvi

#
# PDF target: builds pdf file from the ps file.
#

$(PROGRAM).pdf:	$(PROGRAM).ps
		$(PDF) $(PROGRAM).ps

#
# Clean target: "make clean" to remove unwanted files.
#

.PHONY: clean

clean:
	rm -rf	$(PROGRAM).aux $(PROGRAM).log $(PROGRAM).out \
		$(PROGRAM).toc $(PROGRAM).dvi $(PROGRAM).ps  \
		$(PROGRAM).pdf $(PROGRAM).bbl $(PROGRAM).blg \
		*~
