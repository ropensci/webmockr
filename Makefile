PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

.PHONY: docs

all: move rmd2md

move:
	cp inst/vign/vcr.md vignettes;\
	cp inst/vign/configuration.md vignettes

rmd2md:
	cd vignettes;\
	mv vcr.md vcr.Rmd;\
	mv configuration.md configuration.Rmd

install: doc build
	R CMD INSTALL . && rm *.tar.gz

build:
	R CMD build .

doc:
	${RSCRIPT} -e "devtools::document()"

docs:
	${RSCRIPT} -e "pkgdown::build_site()"

eg:
	${RSCRIPT} -e "devtools::run_examples(run_dontrun = TRUE)"

test:
	${RSCRIPT} -e "devtools::test()"

check: build
	_R_CHECK_CRAN_INCOMING_=FALSE R CMD CHECK --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

check_windows:
	${RSCRIPT} -e "devtools::check_win_devel(); devtools::check_win_release()"

readme:
	${RSCRIPT} -e "rmarkdown::render('details.md')"
	${RSCRIPT} -e "knitr::knit('README.Rmd')"
