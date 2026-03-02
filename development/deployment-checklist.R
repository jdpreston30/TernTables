#* ANY NEW DEPLOYMENT
# Run the following for GitHub commits
rmarkdown::render("vignettes/getting-started.Rmd")
devtools::document()
devtools::check()

# Run these in addition for CRAN submission
#!!! update cran-comments.md
devtools::check_win_devel()
devtools::submit_cran()

