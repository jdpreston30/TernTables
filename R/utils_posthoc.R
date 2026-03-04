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
# centers:     named numeric vector of center statistics (mean or median) per group,
#              named by group_names.
# pairwise_df: data.frame with columns group1, group2, p.adj.
# alpha:       significance threshold (default 0.05).
#
# Returns a named character vector of CLD letter strings (e.g. "a", "ab", "b"),
# in the original group_names order, with "a" assigned to the group with the
# highest center (conventional clinical ordering).
#
# Uses multcompView::multcompLetters() for CLD computation (Piepho 2004), then
# re-labels letters so "a" = highest-center group, "b" = next, etc.
.compute_cld <- function(group_names, centers, pairwise_df, alpha = 0.05) {
  k <- length(group_names)
  if (k <= 1L) return(setNames(rep("", k), group_names))

  # Build named p-value vector required by multcompLetters
  pvec <- setNames(pairwise_df$p.adj,
                   paste(pairwise_df$group1, pairwise_df$group2, sep = "-"))

  # Compute CLD via multcompView (Piepho 2004)
  raw <- multcompView::multcompLetters(pvec, threshold = alpha)$Letters
  raw <- raw[group_names]  # align to original order

  # Re-label: walk groups from highest to lowest center and assign "a", "b", ...
  # to raw letter symbols in the order they are first encountered, so that
  # "a" always lands on the group with the highest center.
  ord          <- order(centers[group_names], decreasing = TRUE)
  sorted_names <- group_names[ord]
  letter_map   <- character(0)
  next_idx     <- 1L
  for (g in sorted_names) {
    for (ch in strsplit(raw[g], "")[[1]]) {
      if (!ch %in% names(letter_map)) {
        letter_map[ch] <- letters[next_idx]
        next_idx <- next_idx + 1L
      }
    }
  }

  # Apply re-mapping, sorting letters within each cell alphabetically
  result <- sapply(group_names, function(g) {
    chars    <- strsplit(raw[g], "")[[1]]
    remapped <- sapply(chars, function(ch) letter_map[[ch]])
    paste(sort(remapped), collapse = "")
  })

  result
}
