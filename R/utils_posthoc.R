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

  # Build named p-value vector required by multcompLetters.
  # multcompLetters always splits pair names on "-", so group names that
  # contain hyphens (e.g. "0-180", "181-365") would be mis-parsed.
  # Work-around: proxy-rename groups to single uppercase letters / short safe
  # tokens for the CLD step, then map results back to the real group names.
  proxy <- setNames(paste0("G", seq_along(group_names)), group_names)
  p1_proxy <- proxy[pairwise_df$group1]
  p2_proxy <- proxy[pairwise_df$group2]
  pvec <- setNames(pairwise_df$p.adj,
                   paste(p1_proxy, p2_proxy, sep = "-"))

  # Compute CLD via multcompView (Piepho 2004)
  raw_proxy <- multcompView::multcompLetters(pvec, threshold = alpha)$Letters

  # Map proxy names back to real group names and align to original order
  inv_proxy <- setNames(names(proxy), unname(proxy))
  raw <- setNames(raw_proxy, inv_proxy[names(raw_proxy)])
  raw[group_names]
}
