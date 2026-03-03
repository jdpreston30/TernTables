# GitHub commits
devtools::document()
Rscript -e "devtools::build_manual(pkg = '.', path = 'development/manual')"
source("development/run-vignette/run-vignette.R")
rmarkdown::render("vignettes/getting-started.Rmd")
devtools::check()

# CRAN submission
#!!! update cran-comments.md
devtools::check_win_devel()
devtools::submit_cran()
