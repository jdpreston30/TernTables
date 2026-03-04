# GitHub commits
devtools::document()
source("development/manual/build-manual.R")
source("development/run-vignette/run-vignette.R")
rmarkdown::render("vignettes/getting-started.Rmd")
devtools::check()

# CRAN submission
#!!! update cran-comments.md
devtools::check_win_devel()
devtools::submit_cran()
