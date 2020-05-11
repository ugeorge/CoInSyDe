SRC := $(wildcard assets/*.tex)
TRG := $(patsubst assets/%.tex, assets/%.png, $(SRC))

all: $(TRG)
	pandoc manual.md > manual.html

assets/%.png: assets/%.tex
	@cd assets
	pdflatex --shell-escape assets/$(*F)
	pdf2svg $(*F).pdf $(*F).svg
	inkscape -d 150 $(*F).svg -e $(*F).png
	mv $(*F).png $@
	rm -f $(*F).svg $(*F).log $(*F).aux
	@cd ..
