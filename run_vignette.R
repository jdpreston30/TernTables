## Run the getting-started vignette, write Word outputs to ~/Desktop/Tables,
## and regenerate the 300dpi screenshot PNGs in vignettes/figures/.
##
## source() this from the package root, or run it directly in R Interactive
## with the working directory set to the package root.

# Build and install current source before rendering so the vignette reflects
# any unsaved R/ changes made since the last devtools::install().
devtools::install(quick = TRUE, upgrade = "never", quiet = TRUE)

outdir      <- normalizePath("~/Desktop/Tables", mustWork = FALSE)
figdir      <- normalizePath("vignettes/figures", mustWork = FALSE)
libreoffice <- "/Applications/LibreOffice.app/Contents/MacOS/soffice"

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
dir.create(figdir, recursive = TRUE, showWarnings = FALSE)

# ── Render vignette ────────────────────────────────────────────────────────────
options(TernTables.vignette_outdir = outdir)

rmarkdown::render(
  input       = "vignettes/getting-started.Rmd",
  output_file = "getting-started.html",
  output_dir  = outdir
)

message("Word outputs written to: ", outdir)

# ── Screenshot pipeline ────────────────────────────────────────────────────────
# Word docs (kept) → PDF via LibreOffice → PNG at 300dpi via pdftoppm
# Temp PDFs are deleted; PNGs are moved into vignettes/figures/

docs <- c(
  tern_descriptive = "Tern_descriptive.docx",
  tern_2_group     = "Tern_2_group.docx",
  tern_3_group     = "Tern_3_group.docx"
)

for (stem in names(docs)) {
  docx_path  <- file.path(outdir, docs[[stem]])
  pdf_path   <- file.path(outdir, sub("\\.docx$", ".pdf", docs[[stem]]))
  png_prefix <- file.path(outdir, stem)
  final_png  <- file.path(figdir, paste0(stem, ".png"))

  # Step 1: Word → PDF
  message("  [1/3] ", docs[[stem]], " → PDF")
  system2(
    libreoffice,
    args = c("--headless", "--convert-to", "pdf",
             "--outdir", shQuote(outdir),
             shQuote(docx_path))
  )

  # Step 2: PDF → PNG at 300dpi  (-singlefile writes <prefix>.png directly)
  message("  [2/3] PDF → PNG (300dpi)")
  system2(
    "pdftoppm",
    args = c("-r", "300", "-png", "-singlefile",
             shQuote(pdf_path),
             shQuote(png_prefix))
  )

  # Step 3: trim whitespace, move PNG into repo; clean up temp PDF
  tmp_png <- paste0(png_prefix, ".png")
  if (file.exists(tmp_png)) {
    message("  [3/3] Trimming whitespace")
    system2("magick", args = c(shQuote(tmp_png), "-trim", "+repage",
                               "-bordercolor", "white", "-border", "35x35",
                               shQuote(tmp_png)))
    file.rename(tmp_png, final_png)
    message("        Saved → ", final_png)
  } else {
    warning("Expected PNG not found: ", tmp_png,
            "\n  pdftoppm may have appended '-1'; check ", outdir)
  }

  if (file.exists(pdf_path)) {
    file.remove(pdf_path)
    message("        Temp PDF removed.")
  }
}

message("\nAll figures updated in: ", figdir)

