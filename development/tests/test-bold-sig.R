devtools::load_all(quiet = TRUE)
library(tibble)

cox_tbl <- tibble(
  Variable           = c("Surgery", "GTR", "STR/Biopsy", "Molecular Subtype", "IDH-mutant", "IDH-wildtype"),
  `Uni HR (95% CI)`  = c("", "Reference", "2.63 (1.03-6.70)", "", "Reference", "3.11 (1.12-8.60)"),
  `Uni p`            = c("0.022", "-", "0.043", "0.008", "-", "0.031"),
  `Multi HR (95% CI)` = c("", "Reference", "2.13 (0.77-5.88)", "", "Reference", "2.54 (0.88-7.32)"),
  `Multi p`          = c("0.117", "-", "0.143", "0.055", "-", "0.087")
)

out <- ternStyle(
  tbl              = cox_tbl,
  filename         = file.path(tempdir(), "cox_test.docx"),
  subheader_rows   = c("Surgery", "Molecular Subtype"),
  bold_rows        = c(1L, 4L),
  bold_sig         = list(
    p_cols    = c("Uni p", "Multi p"),
    hr_cols   = c("Uni HR (95% CI)", "Multi HR (95% CI)"),
    threshold = 0.05
  ),
  manual_italic_indent = c("GTR", "STR/Biopsy", "IDH-mutant", "IDH-wildtype"),
  open_doc  = FALSE,
  citation  = FALSE
)

cat("Done OK\n")

meta <- attr(out, "ternB_meta")
cat("bold_sig in meta:", !is.null(meta[["bold_sig"]]), "\n")
cat("p_cols:", paste(meta[["bold_sig"]][["p_cols"]], collapse = ", "), "\n")

# Test with NULL bold_sig (no regression)
out2 <- ternStyle(
  tbl      = cox_tbl,
  filename = file.path(tempdir(), "cox_test2.docx"),
  open_doc = FALSE,
  citation = FALSE
)
cat("NULL bold_sig: OK\n")

# Test < 0.001 style p-value parsing
cox_tbl2 <- tibble(
  Variable  = c("Age", "Sex"),
  `HR`      = c("1.23 (1.01-1.50)", "0.88 (0.60-1.30)"),
  `p`       = c("< 0.001", "0.520")
)
out3 <- ternStyle(
  tbl      = cox_tbl2,
  filename = file.path(tempdir(), "cox_test3.docx"),
  bold_sig = list(p_cols = "p", hr_cols = "HR", threshold = 0.05),
  open_doc = FALSE,
  citation = FALSE
)
cat("< 0.001 parsing: OK\n")
