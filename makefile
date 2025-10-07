RFILE = README
VIGNETTE = basic-maps

all: help

doc: ## Update package documentation with `roxygen2`
	Rscript -e "roxygen2::roxygenise()"; \

init: ## Initialize pkgdown site
	echo "pkgdown::init_site()" | R --no-save -q

pkgdown: ## Build entire pkgdown site
	echo "pkgdown::build_site()" | R --no-save -q

vignette: ## Build pkgdown article
	echo "pkgdown::build_article('$(VIGNETTE)',quiet=FALSE)" | R --no-save -q

knith: $(RFILE).Rmd ## Render README as HTML
	echo "rmarkdown::render('$(RFILE).Rmd',output_file='$(RFILE).html')" | R --no-save -q

knitr: $(RFILE).Rmd ## Render README as markdown
	echo "rmarkdown::render('$(RFILE).Rmd',output_file='$(RFILE).md')" | R --no-save -q

open: ## Open main HTML vignette in browser
	xdg-open docs/articles/$(VIGNETTE).html &

allcon: ## Run 'allcontributors::add_contributors'
	Rscript -e 'allcontributors::add_contributors()'

check: ## Run `rcmdcheck`
	Rscript -e 'rcmdcheck::rcmdcheck()'

test: ## Run test suite
	Rscript -e 'testthat::test_local()'

pkgcheck: ## Run `pkgcheck` and print results to screen.
	Rscript -e 'library(pkgcheck); checks <- pkgcheck(); print(checks); summary (checks)'

clean: ## Clean all junk files, including all pkgdown docs
	rm -rf *.html *.png README_cache docs/

help: ## Show this help
	@printf "Usage:\033[36m make [target]\033[0m\n"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
