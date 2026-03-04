# Build the TernTables reference manual PDF.
# Deletes any existing PDF in this folder first, then regenerates.
# Run from the project root: source("development/manual/build-manual.R")

manual_dir <- "development/manual"

old_pdfs <- list.files(manual_dir, pattern = "\\.pdf$", full.names = TRUE)
if (length(old_pdfs) > 0) {
  file.remove(old_pdfs)
  message("Removed: ", paste(basename(old_pdfs), collapse = ", "))
}

# Use a login shell (-l) so ~/.zprofile is sourced and TinyTeX is on PATH.
system("zsh -l -c 'Rscript -e \"devtools::build_manual(pkg = \\\".\\\", path = \\\"development/manual\\\")\"'")

pdfs <- list.files(manual_dir, pattern = "\\.pdf$", full.names = TRUE)
if (length(pdfs) > 0) {
  message("Manual built: ", paste(basename(pdfs), collapse = ", "))
} else {
  stop("No PDF found in ", manual_dir, " — check the output above for errors.")
}
