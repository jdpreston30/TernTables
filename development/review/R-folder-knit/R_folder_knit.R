## R_folder_knit.R
## Concatenates every .R / .r file in R/ into a single output file,
## with a plain-text header before each file's contents.
##
## Output: R_folder_knit_output.R in the same folder as this script (overwritten on each run)
## Run:    source("development/review/R-folder-knit/R_folder_knit.R")

# -- Dynamic path resolution: works whether source()'d or run directly --------
script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE)),
  error = function(e) getwd()
)
pkg_root <- local({
  d <- script_dir
  repeat {
    if (length(list.files(d, pattern = "\\.Rproj$")) > 0) break
    p <- dirname(d)
    if (p == d) stop("Cannot find package root (.Rproj file)")
    d <- p
  }
  d
})
# ---------------------------------------------------------------------------

input_dir   <- file.path(pkg_root, "R")
output_file <- file.path(script_dir, "R_folder_knit_output.R")

files <- list.files(input_dir, pattern = "\\.[Rr]$", full.names = TRUE)

lines_out <- character(0)

for (f in files) {
  fname    <- basename(f)
  contents <- readLines(f, warn = FALSE)

  lines_out <- c(
    lines_out,
    paste0("# Here is the ", fname, " file"),
    "",
    contents,
    rep("", 20)   # 20 blank lines between files
  )
}

writeLines(lines_out, output_file)
message("Written: ", output_file,
        "  (", length(files), " files, ", length(lines_out), " lines total)")
