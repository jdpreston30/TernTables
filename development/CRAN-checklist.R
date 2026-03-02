#* ANY NEW CRAN COMMIT:
# 1. update cran-comments.md
# 2. Run the following:
rmarkdown::render("vignettes/getting-started.Rmd")
devtools::document()
devtools::check()
devtools::check_win_devel()
devtools::submit_cran()

