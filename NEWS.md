# TernTables 1.6.4

## Bug fixes / CRAN compliance

* Removed all uses of `<<-` in `ternD()`, `ternG()`, and `ternP()`. Counter
  and tracker variables are now accumulated via a local environment created
  with `new.env(parent = emptyenv())` rather than super-assignment, which
  avoids unintended side-effects on parent frames (CRAN policy).
* Replaced bare `set.seed()` inside `ternG()` with
  `withr::with_seed()` so the global RNG state is never modified
  (CRAN policy). Added **withr** to `Imports`.

# TernTables 1.6.3

* CRAN resubmission addressing reviewer comments on v1.3.1.
* All `\dontrun{}` blocks replaced with `\donttest{}` across all exported
  functions. Examples that write files to disk use `\donttest{}` because the
  'Word' export operations may exceed 5 seconds on slower machines; no missing
  software or API keys are required.
* Software and package names now quoted in the `Description` field of
  `DESCRIPTION` per CRAN policy: 'Word', 'Excel', 'tibble', 'officer',
  'flextable', 'writexl', 'rstatix'.
* Package-level documentation (`?TernTables`) updated: normality routing
  description corrected to four-gate algorithm; `.onLoad` now explicitly
  initialises `TernTables.line_break_header` option.
* `val_format()` now uses the Unicode plus-minus symbol (±) for consistency
  with `ternG()` and `ternD()` output.
* `utils_naming.R`: single-word variable names (e.g. `age`, `sex`, `race`)
  now correctly flow through the abbreviation map and capitalisation rules
  in `.apply_cleaning_rules()`.

# TernTables 1.6.2

* Added odds ratio (OR) support for two-level categorical variables in
  `ternG()` (e.g. Male/Female shown with `1.00 (ref.)` and computed OR with
  95% CI). Previously only Y/N and 0/1 binary variables received ORs.
* Added `factor_order = "mixed"` option to `ternG()` and `ternD()`.
* Post-hoc compact letter display (CLD) superscripts implemented in
  `utils_posthoc.R` with center-based letter ordering (highest center = "a").
* Fallback to Monte Carlo simulation for Fisher's exact test when the exact
  algorithm cannot complete due to workspace limits (B = 10,000; seed fixed
  via `getOption("TernTables.seed")`).
* Liberalized PHI column name detection in `utils_preprocess.R` to reduce
  false positives on common research variable names.

# TernTables 1.6.1

* Integrated tern-tables.com web app into all public-facing documentation:
  README, vignette, and package help page (`?TernTables`).
* Added `URL:` and `BugReports:` fields to `DESCRIPTION`.

# TernTables 1.6.0

* Added `ternP()`: preprocessing function for raw CSV/XLSX data. Handles
  string NA conversion, whitespace trimming, empty column and blank row
  removal, and case normalisation. Hard-stops on PHI column name patterns
  and unnamed columns with data.
* Added `write_cleaning_doc()`: writes a 'Word' audit document recording every
  transformation applied by `ternP()`.
* Added bundled example data `tern_colon_messy.csv` to `inst/extdata/csv/` for
  use in `ternP()` examples.
* `line_break_header` parameter refined: improved behaviour for edge-case
  column widths in `ternG()`, `ternD()`, and `word_export()`.

# TernTables 1.5.0

* Added ROBUST normality routing (`consider_normality = "ROBUST"`, now the
  default). Four-gate algorithm applied per group: (1) n < 3 fail-safe to
  non-parametric; (2) |skewness| > 2 to non-parametric; (3) all groups
  n ≥ 30 to parametric via CLT; (4) Shapiro-Wilk p > 0.05 to parametric.
  Implemented in both `ternG()` and `ternD()`.

# TernTables 1.4.0

* Added `ternB()`: combines multiple TernTables result tibbles into a single
  formatted 'Word' document.
* Added `table_caption` parameter to `ternG()`, `ternD()`, and `word_export()`:
  places a bold caption above the table in the 'Word' output.
* Added `table_footnote` parameter to `ternG()`, `ternD()`, and `word_export()`:
  adds a merged footer row below the table.
* Added Welch ANOVA for 3+ group continuous comparisons (parametric path);
  previously only Kruskal-Wallis was available for 3+ groups.
* Added `line_break_header` parameter to `ternG()`, `ternD()`, and
  `word_export()`: wraps long column headers onto two lines in 'Word' output.
* `write_methods_doc()` now generates a methods paragraph tailored to `ternB()`
  multi-table output.
* Fixed bug where P values were not displaying in three-group comparisons.
* Fixed dynamic bolding/non-bolding of table captions.

# TernTables 1.3.1

* Initial CRAN submission.
* `ternG()`: grouped comparison table for 2- and 3-level group variables,
  with optional odds ratios (`OR_col`), normality testing, and post-hoc
  test framework.
* `ternD()`: descriptive summary table with no group comparisons.
* `word_export()`: exports any TernTables tibble to a formatted 'Word' document.
* `write_methods_doc()`: generates a boilerplate statistical methods paragraph.
* `val_format()` and `val_p_format()`: formatting utilities for publication-ready
  numeric and P value display.

