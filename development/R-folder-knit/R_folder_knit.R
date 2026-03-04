## R_folder_knit.R
## Concatenates every .R / .r file in R/ into a single output file,
## with a plain-text header before each file's contents.
##
## Output: development/R_folder_knit_output.R  (overwritten on each run)
## Run:    source("development/R_folder_knit.R")

input_dir   <- "R"
output_file <- "development/R-folder-knit/R_folder_knit_output.R"

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
