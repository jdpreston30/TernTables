# Documentation: About Page — Normality Section Restructure + Both Tables

## What changed in the package

- Added **excess kurtosis > 7** as a second shape criterion in Gate 2 of the
  ROBUST normality algorithm (previously Gate 2 checked only skewness > 2).
- The algorithm is definitively **four-gate** (not three-gate as previously
  described in some places).
- Both the README and vignette now include:
  1. A **statistical test selection table** (variable type × group count × post-hoc)
  2. A **ROBUST gate table** (four-row decision tree with conditions and notes)
- The normality description has been updated to reflect the four-gate structure
  and the new kurtosis criterion everywhere in the package.

---

## Web app integration required

### About page — "Automatic Statistical Test Selection" section

The current about page has a highlighted/callout blue box beneath the
"Automatic Statistical Test Selection" heading that contains a brief narrative
description of the ROBUST algorithm. That description currently says
**three gates**, which is incorrect — the algorithm has **four gates** and now
also checks excess kurtosis in Gate 2. The entire blue box should be replaced
with a dedicated subsection and updated content as described below.

---

### Change 1 — Add the statistical test selection table

Directly under the "Automatic Statistical Test Selection" heading (before the
normality content), include the following table verbatim:

| Variable type | 2 groups | 3+ groups | Post-hoc (3+ groups, `post_hoc = TRUE`, omnibus *p* < 0.05) |
|---|---|---|---|
| Binary / Categorical | Fisher's exact or Chi-squared | Fisher's exact or Chi-squared | — |
| Numeric, normal | Welch's *t*-test | Welch ANOVA | Games-Howell |
| Numeric, non-normal† | Wilcoxon rank-sum | Kruskal-Wallis | Dunn's + Holm |

†Includes variables designated as ordinal via `force_ordinal`, which bypass
normality testing and always use non-parametric methods.

This table is already in the README and vignette. It should appear on the
about page in this position so users understand what test will be chosen before
reading how normality is decided.

---

### Change 2 — Replace the blue callout box with a "How Normality is Assessed" subsection

Remove the existing blue callout/highlight box. In its place, add a subsection
(visually a level-3 or level-4 heading, whatever fits the about page's heading
hierarchy) titled:

**"How Normality is Assessed"**

The subsection should contain the following narrative paragraph, followed by
the gate table.

#### Narrative paragraph (use this text verbatim or rephrase minimally for style)

> By default, TernTables uses the `ROBUST` normality strategy — a four-gate
> decision tree applied to each continuous variable before a test is selected.
> Gate 1 checks whether any group is too small (n < 3); if so, the variable is
> routed non-parametric as a conservative fail-safe. Gate 2 checks distribution
> shape: if absolute skewness exceeds 2 or absolute excess kurtosis exceeds 7
> in any group, the distribution is too far from symmetric/mesokurtic for
> parametric methods regardless of sample size. Gate 3 applies the Central
> Limit Theorem: when all groups have n ≥ 30, parametric methods are used
> without formal testing. Gate 4 runs Shapiro-Wilk (valid for 3 ≤ n ≤ 5,000);
> a p-value > 0.05 in all groups routes to parametric, otherwise non-parametric.
> Setting `consider_normality = TRUE` skips Gates 1–3 and uses Shapiro-Wilk
> alone (original behavior).

#### Gate table (include verbatim below the narrative paragraph)

| Gate | Condition checked | Routes to | Note |
|---|---|---|---|
| 1 | Any group n < 3 | Non-parametric | Conservative fail-safe; insufficient data to assess |
| 2 | \|skewness\| > 2 **or** \|excess kurtosis\| > 7 in any group | Non-parametric | Distribution shape precludes parametric assumptions regardless of n |
| 3 | All groups n ≥ 30 | Parametric | Central Limit Theorem |
| 4 | Shapiro-Wilk p > 0.05 in **all** groups | Parametric (pass) / Non-parametric (fail) | Valid only when 3 ≤ n ≤ 5,000; n outside this range routes non-parametric |

---

### Plumber API changes (R/plumber.R)

No API changes needed. This is a documentation/display-only update to the
about page.

### Frontend API changes (client/src/api/client.js)

No changes needed.

### State changes (App.jsx / Step2.jsx)

No state changes needed.

---

## Notes / open questions

- The about page blue box text that says "three gates" or "three-gate" must be
  removed or corrected — the algorithm has always had four gates; this was a
  documentation error.
- The kurtosis criterion (|excess kurtosis| > 7) is new as of v1.6.3.9023.
  The narrative paragraph above reflects the updated algorithm accurately.
- The two tables should also appear on any static documentation page, FAQ, or
  tooltip that currently references the normality algorithm or test selection
  logic.
- The Normality Preview step in Step 2 of the app shows users a
  `classify_normality()` result. Consider adding a "?" tooltip or "learn more"
  link from that step pointing to this "How Normality is Assessed" subsection
  on the about page.
