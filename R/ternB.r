#' Combine multiple ternD/ternG tables into a single Word document
#'
#' Takes a list of tibbles previously created by \code{ternD()} or \code{ternG()}
#' and writes them all into one \code{.docx} file, one table per page, preserving
#' the exact formatting settings that were used when each table was built.
#'
#' @param tables A \strong{list} of tibbles created by \code{ternD()} or \code{ternG()}.
#'   Must be constructed with \code{list()}, not \code{c()} (e.g.
#'   \code{list(T1, T2, T3)}).  Each tibble must have been produced in the
#'   \emph{current R session}; the metadata is stored in memory, not in the
#'   tibble columns.
#' @param output_docx Output file path ending in \code{.docx}.
#' @param page_break Logical; if \code{TRUE} (default), inserts a page break
#'   between each consecutive table.
#' @param methods_doc Logical; if \code{TRUE}, writes a single methods section
#'   Word document that covers all tables in the list. Statistical test details
#'   are pooled across all tables. Default is \code{FALSE}.
#' @param methods_filename Output file path for the methods document. Defaults
#'   to \code{"TernTables_methods.docx"} in the working directory.
#'
#' @details
#' \code{ternB()} works by replaying the exact \code{word_export()} call that
#' \code{ternD()} / \code{ternG()} would have made -- using stored metadata
#' attached as an attribute to each returned tibble -- but directing all output
#' into a single combined document instead of separate files.
#'
#' Table captions (\code{table_caption}) and footnotes (\code{table_footnote}) specified in the original
#' \code{ternD()} / \code{ternG()} call are reproduced automatically.  You can
#' override them by modifying the \code{"ternB_meta"} attribute before calling
#' \code{ternB()}, though in practice it is easier to set captions and footnotes when you
#' first build each table.
#'
#' @return Invisibly returns the path to the written Word file.
#'
#' @examples
#' \dontrun{
#' data(tern_colon)
#'
#' T1 <- ternD(tern_colon,
#'             exclude_vars = "ID",
#'             table_caption = "Table 1. Overall patient characteristics.",
#'             methods_doc = FALSE)
#'
#' T2 <- ternG(tern_colon,
#'             group_var    = "Recurrence",
#'             exclude_vars = "ID",
#'             table_caption = "Table 2. Characteristics by recurrence status.",
#'             methods_doc = FALSE)
#'
#' ternB(list(T1, T2),
#'       output_docx = file.path(tempdir(), "combined_tables.docx"))
#' }
#' @export
ternB <- function(tables, output_docx, page_break = TRUE,
                  methods_doc = FALSE,
                  methods_filename = "TernTables_methods.docx") {

  # ── Input validation ──────────────────────────────────────────────────────
  if (!is.list(tables) || inherits(tables, "data.frame")) {
    stop(
      "'tables' must be a list of ternD/ternG output tibbles.\n",
      "Use list(T1, T2, T3) rather than c(T1, T2, T3).",
      call. = FALSE
    )
  }
  if (length(tables) == 0) {
    stop("'tables' is empty -- nothing to export.", call. = FALSE)
  }
  if (!grepl("\\.docx$", output_docx, ignore.case = TRUE)) {
    stop("'output_docx' must end with .docx", call. = FALSE)
  }

  # ── Write each table to its own temp file ─────────────────────────────────
  temp_files <- character(length(tables))
  on.exit(unlink(temp_files[nchar(temp_files) > 0]), add = TRUE)

  for (i in seq_along(tables)) {
    meta <- attr(tables[[i]], "ternB_meta")

    if (is.null(meta)) {
      stop(
        "Table ", i, " does not carry ternB metadata.\n",
        "Make sure it was created by ternD() or ternG() in the current R session.",
        call. = FALSE
      )
    }

    temp_files[i] <- tempfile(fileext = ".docx")

    word_export(
      tbl                  = meta$tbl,
      filename             = temp_files[i],
      round_intg           = meta$round_intg,
      font_size            = meta$font_size,
      category_start       = meta$category_start,
      manual_italic_indent = meta$manual_italic_indent,
      manual_underline     = meta$manual_underline,
      table_caption        = meta$table_caption,
      table_footnote       = meta$table_footnote
    )
  }

  # ── Assemble the combined document ────────────────────────────────────────
  doc <- read_docx()

  for (i in seq_along(temp_files)) {
    if (i > 1 && page_break) {
      doc <- doc %>% body_add_break()
    }
    doc <- doc %>% body_add_docx(src = temp_files[i])
  }

  dir.create(dirname(output_docx), recursive = TRUE, showWarnings = FALSE)
  print(doc, target = output_docx)

  # ── Optional unified methods document ─────────────────────────────────────
  if (methods_doc) {
    all_metas <- lapply(tables, function(t) attr(t, "ternB_meta"))
    all_tests <- unlist(lapply(all_metas, function(m) {
      if (!is.null(m$tbl) && "test" %in% colnames(m$tbl)) m$tbl[["test"]] else character(0)
    }))
    combined_tbl    <- data.frame(test = all_tests, stringsAsFactors = FALSE)
    max_n_levels    <- max(vapply(all_metas, function(m) if (is.null(m$n_levels)) 1L else m$n_levels, integer(1)))
    any_or_col      <- any(vapply(all_metas, function(m) isTRUE(m$OR_col), logical(1)))
    combined_source <- if (any(vapply(all_metas, function(m) identical(m$source, "ternG"), logical(1)))) "ternG" else "ternD"
    write_methods_doc(combined_tbl, methods_filename,
                      n_levels = max_n_levels, OR_col = any_or_col, source = combined_source)
  }

  invisible(output_docx)
}
