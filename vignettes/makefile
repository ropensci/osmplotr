MAPFILE = basic-maps
DATFILE = data-maps

all: mapsh mapsr datah datar

mapsr: mapsmaker

mapsh: mapsmakeh mapsopen

datar: datamaker

datah: datamakeh dataopen

mapsmaker: $(MAPFILE).Rmd
	echo "rmarkdown::render('$(MAPFILE).Rmd',rmarkdown::md_document(variant='markdown_github'))" | R --no-save -q

mapsmakeh: $(MAPFILE).Rmd
	echo "rmarkdown::render('$(MAPFILE).Rmd',output_file='$(MAPFILE).html')" | R --no-save -q

datamaker: $(DATFILE).Rmd
	echo "rmarkdown::render('$(DATFILE).Rmd',rmarkdown::md_document(variant='markdown_github'))" | R --no-save -q

datamakeh: $(DATFILE).Rmd
	echo "rmarkdown::render('$(DATFILE).Rmd',output_file='$(DATFILE).html')" | R --no-save -q

mapsopen: $(MAPFILE).html 
	xdg-open $(MAPFILE).html &

dataopen: $(DATFILE).html
	xdg-open $(DATFILE).html &

clean:
	rm -f *.png *.html
