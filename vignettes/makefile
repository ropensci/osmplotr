MAPFILE = making-maps
DATFILE = downloading-data

all: maps data 

maps: mapsmake mapsopen

data: datamake dataopen

mapsmake: $(MAPFILE).Rmd
	echo "rmarkdown::render('$(MAPFILE).Rmd',rmarkdown::md_document(variant='markdown_github'))" | R --no-save -q
	echo "rmarkdown::render('$(MAPFILE).Rmd',output_file='$(MAPFILE).html')" | R --no-save -q

datamake: $(DATFILE).Rmd
	echo "rmarkdown::render('$(DATFILE).Rmd',rmarkdown::md_document(variant='markdown_github'))" | R --no-save -q
	echo "rmarkdown::render('$(DATFILE).Rmd',output_file='$(DATFILE).html')" | R --no-save -q

mapsopen: $(MAPFILE).html 
	xdg-open $(MAPFILE).html &

dataopen: $(DATFILE).html
	xdg-open $(DATFILE).html &

clean:
	rm -f *.png *.html
