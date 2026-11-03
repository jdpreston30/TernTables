# GitHub commits
devtools::document()
source("development/tools/manual/build-manual.R")
source("development/tools/run-vignette/run-vignette.R")
rmarkdown::render("vignettes/getting-started.Rmd")
devtools::check()

# CRAN submission
#!!! update cran-comments.md
# devtools::check_win_devel()
# devtools::submit_cran()
