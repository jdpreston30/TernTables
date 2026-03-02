#* ANY NEW DEPLOYMENT
# Required for GitHub commits
rmarkdown::render("vignettes/getting-started.Rmd")
devtools::document()
devtools::check()

# Optional for GitHub commits
Rscript -e "devtools::build_manual(pkg = '.', path = 'development/manual')"
source("development/run-vignette/run-vignette.R")

# Run these in addition for CRAN submission
#!!! update cran-comments.md
devtools::check_win_devel()
devtools::submit_cran()

