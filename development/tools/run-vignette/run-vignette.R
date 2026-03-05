## Run the getting-started vignette, write all outputs to the development/tools/run-vignette folder.
##
## source() this from the package root, or run it directly in R Interactive
## with the working directory set to the package root.
##
## Everything — install, render, and screenshots — runs in a fresh Rscript
## subprocess so there is no conflict with TernTables already being loaded
## in the interactive session.

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

pkg_root    <- normalizePath(pkg_root, mustWork = TRUE)
outdir      <- normalizePath(script_dir, mustWork = FALSE)
libreoffice <- "/Applications/LibreOffice.app/Contents/MacOS/soffice"

child_script <- tempfile(fileext = ".R")
writeLines(con = child_script, text = sprintf('
pkg_root        <- %s
outdir          <- %s
tables_dir      <- file.path(outdir, "tables")
screenshots_dir <- file.path(outdir, "screenshots")
libreoffice     <- %s

# Install from source in this clean session (no loaded namespace to conflict)
message("Installing TernTables from source...")
devtools::install(pkg_root, quick = TRUE, upgrade = "never", quiet = TRUE)
message("Install complete. Rendering vignette...")

dir.create(outdir,          recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir,      recursive = TRUE, showWarnings = FALSE)
dir.create(screenshots_dir, recursive = TRUE, showWarnings = FALSE)

# Vignette writes Word docs to tables/
options(TernTables.vignette_outdir = tables_dir)

# HTML renders to run-vignette root
rmarkdown::render(
  input       = file.path(pkg_root, "vignettes", "getting-started.Rmd"),
  output_file = "getting-started.html",
  output_dir  = outdir
)
message("Word outputs written to: ", tables_dir)

docs <- c(
  tern_descriptive = "Tern_descriptive.docx",
  tern_2_group     = "Tern_2_group.docx",
  tern_3_group     = "Tern_3_group.docx"
)

for (stem in names(docs)) {
  docx_path  <- file.path(tables_dir, docs[[stem]])
  pdf_path   <- file.path(screenshots_dir, sub("\\\\.docx$", ".pdf", docs[[stem]]))
  png_prefix <- file.path(screenshots_dir, stem)
  final_png  <- file.path(screenshots_dir, paste0(stem, ".png"))

  message("  [1/3] ", docs[[stem]], " -> PDF")
  system2(libreoffice,
          args = c("--headless", "--convert-to", "pdf",
                   "--outdir", shQuote(screenshots_dir), shQuote(docx_path)))

  message("  [2/3] PDF -> PNG (600dpi)")
  system2("pdftoppm",
          args = c("-r", "600", "-png", "-singlefile",
                   shQuote(pdf_path), shQuote(png_prefix)))

  tmp_png <- paste0(png_prefix, ".png")
  if (file.exists(tmp_png)) {
    message("  [3/3] Trimming and saving")
    system2("magick", args = c(shQuote(tmp_png), "-trim", "+repage",
                               "-bordercolor", "white", "-border", "35x35",
                               shQuote(tmp_png)))
    # Descriptive table has only one data column so it renders narrower at
    # full size relative to the grouped tables — resize to 75%% to match.
    if (stem == "tern_descriptive") {
      system2("magick", args = c(shQuote(tmp_png), "-resize", "75%%",
                                 shQuote(tmp_png)))
    }
    file.rename(tmp_png, final_png)
    message("        Saved -> ", final_png)
  } else {
    warning("PNG not found: ", tmp_png, " -- check ", screenshots_dir)
  }
  if (file.exists(pdf_path)) { file.remove(pdf_path); message("        Temp PDF removed.") }
}
message("\\nAll outputs written to: ", outdir)
',
  deparse(pkg_root), deparse(outdir), deparse(libreoffice)
))

message("Spawning fresh R process (install + render + screenshots)...")
status <- system2("Rscript", args = child_script)
file.remove(child_script)

if (status == 0L) {
  message("\nDone. All outputs in: ", outdir)
} else {
  warning("Subprocess exited with status ", status, " — check output above.")
}