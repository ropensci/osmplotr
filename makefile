#LFILE = ./vignettes/making-maps
LFILE = README

all: knit open 

knit: $(LFILE).Rmd
	echo "rmarkdown::render('$(LFILE).Rmd',output_file='$(LFILE).md')" | R --no-save -q
	echo "rmarkdown::render('$(LFILE).Rmd',output_file='$(LFILE).html')" | R --no-save -q

open: $(LFILE).html
	xdg-open $(LFILE).html &

clean:
	rm -f vignettes/*.png