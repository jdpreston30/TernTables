# Build the TernTables reference manual PDF.
# Deletes any existing PDF in this folder first, then regenerates.
# Can be sourced from the project root or run directly from its own folder.

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

manual_dir <- script_dir  # PDFs are written alongside this script

old_pdfs <- list.files(manual_dir, pattern = "\\.pdf$", full.names = TRUE)
if (length(old_pdfs) > 0) {
  file.remove(old_pdfs)
  message("Removed: ", paste(basename(old_pdfs), collapse = ", "))
}

# Use a login shell (-l) so ~/.zprofile is sourced and TinyTeX is on PATH.
system(paste0(
  "zsh -l -c 'Rscript -e \"devtools::build_manual(pkg = \\\"", pkg_root,
  "\\\", path = \\\"", manual_dir, "\\\")\"'"
))

pdfs <- list.files(manual_dir, pattern = "\\.pdf$", full.names = TRUE)
if (length(pdfs) > 0) {
  message("Manual built: ", paste(basename(pdfs), collapse = ", "))
} else {
  stop("No PDF found in ", manual_dir, " — check the output above for errors.")
}
