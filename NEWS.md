# TernTables 1.6.4

## New features

* **`ternStyle()`**: New exported function that applies full TernTables Word
  formatting to any user-supplied tibble, enabling custom or manually assembled
  tables to be exported with the same font, shading, border, and footnote
  styling as `ternG()` / `ternD()` output. The returned tibble carries a
  `ternB_meta` attribute so it can be passed directly to `ternB()` for
  inclusion in combined multi-table documents.

* **`classify_normality()`**: New exported function that runs the same
  normality assessment used internally by `ternG()` and `ternD()`, returning a
  tidy tibble with per-variable × per-group statistics (n, skewness, excess
  kurtosis, Shapiro-Wilk p), the decision gate, a plain-language `gate_reason`,
  and the final routing outcome. Designed for manuscript auditing and responding
  to reviewer questions about normality testing.

* **`font_family` parameter** (`ternG`, `ternD`, `ternB`, `ternStyle`,
  `word_export`, `write_methods_doc`, `write_cleaning_doc`): New argument
  controlling the font used throughout all Word output. Defaults to
  `getOption("TernTables.font_family", "Arial")`. Package-wide default can be
  set once with `options(TernTables.font_family = "Times New Roman")`.

* **`plain_header` parameter** (`ternG`, `ternD`, `word_export`): When `TRUE`,
  the first column header cell is rendered without the standard dark background
  and white text, producing a plain/white header consistent with some journal
  styles.

* **`show_p` parameter** (`ternG`): Suppresses the P value column and all
  associated columns (OR, test, normality) when `FALSE`. Produces a
  descriptive-only grouped table; useful for baseline characteristic tables
  where hypothesis testing is not the intent.

* **`show_missing` parameter** (`ternG`, `ternD`): When `TRUE`, appends a
  `Missing: n (%)` sub-row beneath each variable showing per-group missing
  counts. A footnote explaining the format is added automatically.

* **`force_continuous` parameter** (`ternG`, `ternD`): Character vector of
  variable names that should bypass automatic binary-numeric detection and
  always be analysed as continuous (mean ± SD / median [IQR]). Useful when a
  `{0, 1}` column represents a measurement or dose rather than a category.
  `force_ordinal` takes priority if a variable appears in both.

* **`force_normal` parameter** (`ternG`, `ternD`): Character vector of
  variables that bypass all normality assessment and are always summarised as
  mean ± SD and compared with Welch's *t*-test / Welch ANOVA. The
  per-variable parametric counterpart to `force_ordinal`.

* **`zero_to_dash` parameter** (`ternG`, `ternD`): When `TRUE`, replaces
  `"0 (0%)"` cells in categorical output with `"-"`. Also replaces
  structurally impossible `"0 (NaN%)"` cells (present in any setting).

* **`percentage_compute` parameter** (`ternG`): Controls the denominator for
  categorical percentages. `"column"` (default) uses within-group column
  totals; `"row"` uses the row total, showing how each category level is
  distributed across groups. When `"row"`, the Total column shows 100% for
  every level.

* **`round_decimal` parameter** (`ternG`, `ternD`, `ternStyle`, `word_export`,
  `ternB`): Integer number of decimal places for all continuous summary values.
  Overrides the default of 1 decimal place. Ignored when `round_intg = TRUE`.
  Default is `NULL` (preserves existing behavior).

* **`p_adjust` and `p_adjust_display` parameters** (`ternG`): `p_adjust = TRUE`
  applies Benjamini-Hochberg FDR correction to all omnibus P values.
  `p_adjust_display = "fdr_only"` (default) renames the P column to
  `"P value (FDR corrected)"`; `"both"` retains raw values alongside the
  corrected column.

* **`citation` parameter** (`ternG`, `ternD`, `ternB`, `word_export`,
  `write_methods_doc`): When `TRUE` (default), embeds a full citation as a Word
  page footer in every exported `.docx`. Set `FALSE` to suppress.

* **`open_doc` parameter** (`ternG`, `ternD`, `word_export`,
  `write_methods_doc`): When `TRUE` (default), opens the written `.docx` in the
  system default application after saving. Set `FALSE` to suppress (default for
  web-app use).

* **`variable_footnote`, `abbreviation_footnote`, `index_style` parameters**
  (`ternG`, `ternD`, `word_export`, `ternB`): Structured footnote system.
  `abbreviation_footnote` prints first. `variable_footnote` (named character
  vector) auto-assigns `*`, `†`, `‡` … as Word superscripts to named variables
  in column 1 and appends definitions below the table. `index_style =
  "alphabet"` uses Unicode superscript letters instead. Pipe-separated keys
  (`"Var A|Var B"`) assign one shared symbol and footnote to multiple variables.

* **`write_methods_doc()` redesign**: Replaced three-section boilerplate with a
  single dynamic paragraph tailored to the actual run (descriptive, two-group,
  or three-or-more-group). `write_methods_doc(boilerplate = TRUE)` writes a
  comprehensive reference document covering all five standard configurations.

* **`ternB()` per-table methods paragraphs**: `ternB(methods_doc = TRUE)` now
  generates one labeled methods section per table, deduplicating tables with
  identical configurations.

## Bug fixes

* **CLD superscript bug**: Fixed `rstatix` / `multcompView` dependency
  resolution that caused compact letter display (CLD) to silently fail or
  produce incorrect superscripts when those packages were in `Suggests` rather
  than `Imports`. Both are now hard `Imports`.

* **Word export line-break header crash**: Fixed error thrown when
  `line_break_header = FALSE` and the table had a specific column count
  combination.

* **Blank page between `ternB()` tables**: The default blank paragraph created
  by `read_docx()` in each temp file was carried into the combined document
  after the manual page break, pushing the subsequent table to the next page.
  Fixed by stripping the initial blank paragraph in `word_export()`.

* **Citation footer bleed between `ternB()` tables**: Page footer from one
  table's temp document was bleeding into the next in combined output. Fixed.

* **`category_start` anchors**: Now match case-insensitively against display
  variable names, so anchors written in title case no longer silently fail
  when `smart_rename = TRUE` produces sentence-case output.

* **Wide-table page overflow**: `word_export()` now applies `fit_to_width(6.5)`
  after `autofit()` only when the table exceeds Letter page width (6.5 in),
  preventing column truncation in Word and PDF exports.

* **Name-cleaning false positives**: Fixed several `.apply_cleaning_rules()`
  patterns that incorrectly transformed variable names containing medical
  abbreviations and unit suffixes (e.g. `"Gy"` in dose variables).

## Internal changes

* **CRAN compliance — `<<-` eliminated**: All accumulator patterns using `<<-`
  inside nested closures (`lapply` callbacks, `tryCatch` handlers) in
  `ternG.R`, `ternD.R`, and `ternP.R` have been replaced with environment-based
  counters (`new.env(parent = emptyenv())`) and explicit `for`-loop restructuring.
  No change to user-visible behavior.

* **`set.seed()` replaced**: The bare `set.seed()` call inside the Monte Carlo
  Fisher's exact fallback has been replaced with `withr::with_seed()`, scoping
  the seed locally and restoring the user's RNG state after the call.
  `withr` added to `Imports`.

* **ROBUST normality Gate 2 — kurtosis**: Excess kurtosis (|kurtosis| > 7) now
  triggers non-parametric routing alongside skewness in Gate 2 of the four-gate
  ROBUST algorithm. The normality decision logic has been extracted to a
  dedicated `utils_normality.R` helper.

* **CLD letter ordering**: Removed center-based letter re-mapping from
  `.compute_cld()`. CLD letters now follow the default `multcompLetters()`
  alphabetical ordering rather than being re-labeled so that "a" = highest
  center. Aligns with standard CLD conventions.

* **Independence-of-observations documentation**: Added explicit scope notes to
  the package help page, `ternG()` description, vignette, and README stating
  that all tests assume independent observations and that repeated-measures or
  clustered data require different approaches.

* **PHI detection tightening**: `utils_preprocess.R` updated to remove
  `patient_id`, `subject_id`, `participant_id`, and clinical-event date
  patterns from the PHI flag list. Only personal-identity date patterns (DOB,
  DOD) remain flagged, reducing false positives on common research variable names.

---

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

