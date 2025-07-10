PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file
FILE_TARGET := "R/${FILE}.R"

.PHONY: docs

all: move rmd2md

install: doc build
	R CMD INSTALL . && rm *.tar.gz

build:
	R CMD build .

doc:
	${RSCRIPT} -e "devtools::document()"

docs:
	${RSCRIPT} -e "pkgdown::build_site()"

eg:
	${RSCRIPT} -e "devtools::run_examples()"

test:
	${RSCRIPT} -e "devtools::test()"

check:
	_R_CHECK_SYSTEM_CLOCK_=0 NOT_CRAN=true _R_CHECK_CRAN_INCOMING_=FALSE \
	${RSCRIPT} -e "rcmdcheck::rcmdcheck()"

check_windows:
	${RSCRIPT} -e "devtools::check_win_devel(); devtools::check_win_release()"

readme:
	${RSCRIPT} -e "rmarkdown::render('details.md')"
	${RSCRIPT} -e "knitr::knit('README.Rmd')"

lint_check:
	air format --check .

lint_fix:
	air format .

# use: `make style_file FILE=stuff`
# ("R/" is prepended); accepts 1 file only
style_file:
	${RSCRIPT} -e 'styler::style_file(${FILE_TARGET})'
