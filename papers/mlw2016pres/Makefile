all: presentation.pdf

presentation.pdf: presentation.tex
	pdflatex -interaction=nonstopmode $< && xdg-open $@

clean:
	$(RM) *.pdf *.dvi *.log

