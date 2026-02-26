#' Format a P value for reporting
#'
#' @param p Numeric P value in the range [0, 1]. \code{NA} values are returned as \code{NA_character_}.
#'   Values >= 1 (or rounding to >= 1) are returned as e.g. \code{">0.999"}.
#' @param digits Integer; number of decimal places for reported P values. Default is 3.
#'   Note: for p < 0.001, the value is reported in scientific notation with 1 significant figure
#'   regardless of \code{digits} (e.g., \code{8E-4}).
#' @return A character string. Values < 0.001 are formatted in scientific notation with 1 significant
#'   figure (e.g., \code{"8E-4"}). All other values use fixed-point notation rounded to \code{digits}
#'   decimal places.
#' @export
val_p_format <- function(p, digits = 3) {
  if (is.na(p)) {
    return(NA_character_)
  }

  # Handle p-values >= 1 or very close to 1 (due to rounding/floating point issues)
  if (p >= 1.0 || round(p, digits) >= 1.0) {
    return(paste0(">0.", paste(rep("9", digits), collapse = "")))
  }

  # Use proper rounding (5 rounds up, not banker's rounding)
  p_rounded <- round(p + .Machine$double.eps, digits)

  if (p < 0.001) {
    base <- signif(p, 1)
    sci <- format(base, scientific = TRUE, digits = 1)
    # Capitalize E and remove leading zeros from exponent
    sci <- gsub("e", "E", sci)
    sci <- gsub("E-0", "E-", sci)
    sci <- gsub("E\\+0", "E+", sci)
    return(sci)
  }

  if (p >= 0.001 && p < 0.1) {
    return(sprintf(paste0("%.", digits, "f"), p_rounded))
  }

  if (p >= 0.94 && p < 1) {
    return(sprintf(paste0("%.", digits, "f"), p_rounded))
  }

  # For all other values, use the specified digits
  return(sprintf(paste0("%.", digits, "f"), p_rounded))
}

#' Format a mean +/- SD string
#'
#' @param mean Numeric mean value. Formatted to 1 decimal place.
#' @param sd Numeric standard deviation. Formatted to 1 decimal place.
#' @return A character string of the form \code{"X.X  +-  Y.Y"} where both values are
#'   rendered to 1 decimal place using fixed-point notation.
#' @export
val_format <- function(mean, sd) {
  paste0(
    formatC(mean, format = "f", digits = 1),
    "  +-  ",
    formatC(sd, format = "f", digits = 1)
  )
}

export_to_excel <- function(tbl, filename) {
  # Simple Excel export without formatting
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  writexl::write_xlsx(tbl, path = filename)
}
