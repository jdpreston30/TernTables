# TernTables 1.6.3.9024 (development)

## New features

* **`force_normal` parameter**: `ternG()` and `ternD()` now accept a
  `force_normal` character vector. Variables listed here bypass all normality
  assessment (Gates 1–4 under ROBUST, or Shapiro-Wilk under `consider_normality = TRUE`)
  and are always summarized as mean ± SD and compared with Welch's *t*-test (2 groups)
  or Welch ANOVA (3+ groups). This is the per-variable parametric counterpart to the
  existing `force_ordinal` argument. `force_ordinal` takes priority if a variable
  appears in both. Default is `NULL`.

---

# TernTables 1.6.3.9021 (development)

## New features

* **Bold-convention footnote in grouped tables**: `ternG()` now automatically
  appends an explanatory note to the footnote block of every grouped table
  describing the bold formatting convention. When a P value column is present,
  the note reads *"Bold p-values indicate statistical significance (p < 0.05)."*
  When an OR column is also present, it reads *"Bold values indicate statistical
  significance (p < 0.05); bold OR indicates 95% CI excludes 1."* The note is
  prepended to `abbreviation_footnote` and stored in `ternB_meta` so `ternB()`
  replays it correctly in combined documents.

## Bug fixes

* **Wide-table page overflow**: `word_export()` now measures the total table
  width via `flextable::flextable_dim()` after `autofit()`. If the width exceeds
  6.5 inches (Letter page minus 1-inch margins), `fit_to_width(max_width = 6.5)`
  is applied to proportionally scale all columns down to fit the page. Tables
  that fit within 6.5 inches are unaffected. This prevents columns from being
  cut off in Word, PDF exports, and the web app preview PNG for datasets with
  many variables or long variable names.

* **Multi-page preview PNG crash**: The preview PNG pipeline in the Plumber API
  used `img[[i]]` (double-bracket) to index into a magick image vector when
  stacking multiple pages. Magick image vectors require single-bracket `[i]`
  subsetting — double-bracket returns the raw internal object, not a valid
  magick image, causing the error *"The 'image' argument is not a magick image
  object"* for any table that rendered to more than one page. Fixed by
  processing each page individually with `img[i]` inside a `lapply` loop before
  stacking with `image_append`. Single-page tables were unaffected.

---

# TernTables 1.6.3.9020 (development)

## New features

* **`force_continuous` parameter** — `ternG()` and `ternD()` now accept a
  `force_continuous` character vector. Variables listed here bypass the automatic
  binary numeric detection that would otherwise convert a `{0, 1}` column to a
  categorical Y/N variable. Instead, they are analysed as continuous measurements
  (mean ± SD and parametric tests, subject to the `consider_normality` setting).
  This is useful when a numeric variable with only two unique values represents
  a measurement or dose level rather than a dichotomous category. If a variable
  appears in both `force_continuous` and `force_ordinal`, `force_ordinal` wins.
  The parameter is stored in the `ternB_meta` attribute and is emitted in the
  generated `_reproduce.R` script. In the web app, users can override the detected
  type for any numeric variable via a dropdown in the Table Layout step.

---

# TernTables 1.6.3.9009 (development)

## New features

* **`citation` parameter** — `ternD()`, `ternG()`, `word_export()`,
  `write_methods_doc()`, and `ternB()` now accept `citation = TRUE` (default).
  When enabled, a full citation line is embedded as a true Word document page
  footer (bold italic, 8 pt Arial, black) in every exported `.docx` — tables,
  methods documents, and combined `ternB()` bundles alike. The citation text is
  generated dynamically: the package version is resolved at runtime with the
  development suffix stripped (e.g. `1.6.3.9009` → `1.6.3`). Set
  `citation = FALSE` to suppress. Citation format:
  *Preston JD, Abadiotakis H, Tang A, Rust CJ, Chan JL.
  TernTables: Publication-ready summary tables and statistical testing for
  clinical research. R package version 1.6.3, 2026. Available at:
  https://github.com/jdpreston30/TernTables (Web interface:
  https://tern-tables.com).*

## Internal changes

* Added `.tern_pkg_version()` and `.tern_citation_line()` internal helpers to
  `utils_format.R`. These centralise version-stripping logic and citation text
  generation, replacing duplicated inline code in `write_methods_doc()` and
  `ternB()`.
* Added `body_set_default_section`, `prop_section`, and `block_list` to
  `@importFrom officer` for page-footer support.
* Added `as_chunk` to `@importFrom flextable`; was previously used but not
  formally imported (triggered R CMD check NOTE).

## Bug fixes / check clean-up

* `\u` Unicode escape sequences in roxygen `@param` comments for `ternD()`,
  `ternG()`, and `word_export()` replaced with literal Unicode characters.
  The escapes were valid in R string literals but not in `.Rd` source, causing
  repeated `unknown macro '\u'` warnings during `R CMD check` and build.
* `open_doc = FALSE` added to all live-executing code chunks in the vignette
  to prevent `.docx` files from auto-opening during `devtools::check()` and
  `rmarkdown::render()`.
* `feature.R` added to `.Rbuildignore` to suppress the non-standard top-level
  file NOTE in `R CMD check`.

---

# TernTables 1.6.3.9003 (development)

## New features

* **BH FDR correction (`p_adjust`)** — `ternG()` now accepts `p_adjust = TRUE`
  to apply Benjamini-Hochberg false discovery rate correction (Benjamini &
  Hochberg, 1995) to all omnibus *P* values. Correction pool is one *P* per
  variable; sub-rows of multi-level categoricals share the parent *P* and are
  not double-counted; post-hoc pairwise *P* values (which already carry Holm
  correction) are excluded. `p_adjust_display = "fdr_only"` (default) renames
  the *P* column to `"P value (FDR corrected)"`; `"both"` retains raw values
  and inserts the corrected column immediately to the right. When `p_adjust =
  TRUE`, the auto-generated methods paragraph is updated automatically to
  include the BH procedure sentence and restates the significance threshold as
  FDR-corrected p < 0.05.

* **`p_adjust` single-test warning** — when `p_adjust = TRUE` and only one
  variable's *P* value is in the correction pool, a `cli_alert_info` message
  notifies the user that BH adjustment requires multiple tests to be meaningful
  and the reported value is unchanged from the raw *P* value.

* **`write_methods_doc()` single-paragraph redesign** — replaced the previous
  three-section boilerplate with a single dynamic paragraph tailored to the
  actual run (descriptive, two-group, or three-or-more-group). Paragraph is
  preceded by a bold "Statistical Methods" header and followed by an italic
  attribution footer. Version number in the footer strips the dev tag so the
  public release version is always shown. `write_methods_doc()` now returns the
  paragraph text invisibly for programmatic testing.

* **`write_methods_doc(boilerplate = TRUE)`** — new parameter (default `FALSE`;
  existing behaviour unchanged). When `TRUE`, writes a comprehensive reference
  document covering all five standard configurations (ternD descriptive; ternG
  2-group no OR; ternG 2-group with OR; ternG 3+-group no post-hoc; ternG
  3+-group with post-hoc), always saved to
  `comprehensive_boilerplate_methods.docx` in the working directory.

* **`open_doc` parameter** — `ternD()`, `ternG()`, `word_export()`, and
  `write_methods_doc()` now accept `open_doc = TRUE` (default). The written
  `.docx` is automatically opened in the system default application after
  saving. Set to `FALSE` to suppress. Has no effect on web-app usage.

* **`variable_footnote`, `abbreviation_footnote`, `index_style` parameters** —
  `ternD()`, `ternG()`, `word_export()`, and `ternB()` now support structured
  footnotes. `abbreviation_footnote` (character string) is printed first.
  `variable_footnote` (named character vector: `name` = display variable,
  `value` = definition text) auto-assigns `*`, `†`, `‡`, `§`, `¶`, `∥` (then
  doubled `**`, `††`, …) to named variables in column 1, appending the symbol
  as a true Word superscript (except `*` which appends as plain text). 
  `index_style = "alphabet"` uses Unicode superscript letters (ᵃ, ᵇ, ᶜ, …)
  instead. Symbols appear as plain text in the footnote body per journal
  convention. Legacy `table_footnote` lines follow last.

* **`ternB()` per-table methods paragraphs** — `ternB(methods_doc = TRUE)` now
  generates one labeled section per table (using `table_caption` as the
  heading, falling back to "Table 1", "Table 2", etc.) instead of a single
  pooled paragraph. Tables with identical configurations are deduplicated into
  a single section with combined labels (e.g. "Table 1 / Table 3") to avoid
  redundant boilerplate. Footer updated to note consolidation behavior.

## Bug fixes

* `category_start` anchors now match case-insensitively against the display
  variable names in the table. Previously, anchors written in title case (e.g.
  `"Colonic Obstruction"`) silently failed to insert the header when
  `smart_rename = TRUE` (the default) produced sentence-case output (e.g.
  `"Colonic obstruction"`), leaving sandwiched headers absent from the Word
  output. First word and abbreviation-driven labels (e.g. `"Age (yr)"`) were
  unaffected. Fixed in `word_export()`.

* `OR_method` was not stored in `ternB_meta`; `ternB(methods_doc = TRUE)` could
  never produce the Wald-only OR description. Fixed.
* `post_hoc` was not stored in `ternB_meta`; `ternB(methods_doc = TRUE)` always
  wrote "post-hoc comparisons were not performed" even when `post_hoc = TRUE`
  was used on a pooled table. Fixed.
* `or_sentence` was missing from `sec3_body`; OR content was silently dropped
  from three-or-more-group paragraphs in `ternB` bundles. Fixed.
* `OR_method` was not forwarded from `ternG()` to `write_methods_doc()`; Wald-only
  users received the wrong dynamic Fisher/Wald description. Fixed.
* Skewness gate phrasing now context-aware: `ternG()` correctly says "any
  comparison group had absolute skewness exceeding 2.0"; `ternD()` says
  "variables with absolute skewness exceeding 2.0".
* `p.adjust` was called without a formal import, triggering an R CMD check NOTE
  (`no visible global function definition for 'p.adjust'`). Added
  `importFrom("stats", "p.adjust")` to `imports.R`.
* `ternB(methods_doc = TRUE)` section headings used the full `table_caption`
  text as the heading label, producing multi-sentence headings when long
  clinical captions were supplied. Labels now extract only the leading
  `"Table N"` token (e.g. `"Table 2 Statistical Methods"` instead of the
  entire caption). Tables with identical configurations are still consolidated
  under a combined label (e.g. `"Table 2 / Table 3 Statistical Methods"`).
* `ternB()` blank page and displaced caption between assembled tables: the
  default blank paragraph created by `read_docx()` in each temp file was
  carried into the combined document after the manual page break, leaving
  insufficient room for the subsequent table and causing Word to push the
  table to the next page while the caption remained behind on what appeared
  as a blank page. Fixed by stripping the initial blank paragraph in
  `word_export()` immediately after `read_docx()`.

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

