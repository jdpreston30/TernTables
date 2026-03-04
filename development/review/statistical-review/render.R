# HTML rendering — paths resolved dynamically from this script's location.

# -- Dynamic path resolution: works whether source()'d or run directly --------
script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE)),
  error = function(e) getwd()
)
# ---------------------------------------------------------------------------

rmarkdown::render(file.path(script_dir, "statistical-review-request.Rmd"))
browseURL(file.path(script_dir, "statistical-review-request.html"))
