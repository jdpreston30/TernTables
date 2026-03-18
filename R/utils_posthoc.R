# Internal post-hoc helpers used by ternG()

# Convert a CLD letter string like "ab" to Unicode superscript characters.
.superscript_letters <- function(s) {
  sup <- c(
    a = "\u1d43", b = "\u1d47", c = "\u1d9c", d = "\u1d48",
    e = "\u1d49", f = "\u1da0", g = "\u1d4d", h = "\u02b0",
    i = "\u2071", j = "\u02b2", k = "\u1d4f", l = "\u02e1",
    m = "\u1d50", n = "\u207f", o = "\u1d52", p = "\u1d56",
    r = "\u02b3", s = "\u02e2", t = "\u1d57", u = "\u1d58",
    v = "\u1d5b", w = "\u02b7", x = "\u02e3", y = "\u02b8",
    z = "\u1dbb"
  )
  chars <- strsplit(s, "")[[1]]
  paste(sapply(chars, function(ch) if (ch %in% names(sup)) sup[[ch]] else ch),
        collapse = "")
}

# Compute compact letter display (CLD) from pairwise post-hoc results.
#
# group_names: character vector of group names in any order.
# pairwise_df: data.frame with columns group1, group2, p.adj.
# alpha:       significance threshold (default 0.05).
#
# Returns a named character vector of CLD letter strings (e.g. "a", "ab", "b"),
# in the original group_names order. Letter assignment follows the default
# multcompLetters() ordering (alphabetical by group name), consistent with
# standard CLD conventions.
#
# Uses multcompView::multcompLetters() for CLD computation (Piepho 2004).
.compute_cld <- function(group_names, pairwise_df, alpha = 0.05) {
  k <- length(group_names)
  if (k <= 1L) return(setNames(rep("", k), group_names))

  # Build named p-value vector required by multcompLetters
  pvec <- setNames(pairwise_df$p.adj,
                   paste(pairwise_df$group1, pairwise_df$group2, sep = "-"))

  # Compute CLD via multcompView (Piepho 2004) and align to original order
  raw <- multcompView::multcompLetters(pvec, threshold = alpha)$Letters
  raw[group_names]
}
