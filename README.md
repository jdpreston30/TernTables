# TernTables

Clinical researchers spend a disproportionate amount of time on the mechanics of generating summary tables: choosing the right test, formatting *P* values, cleaning variable names, writing the methods paragraph, and wrestling with Word. For descriptive statistics and standard univariate comparisons, this work is largely procedural — it follows well-established rules — yet it consistently becomes a bottleneck that slows down manuscript preparation.

**TernTables** handles that procedural layer automatically. Variable type detection, statistical test selection, *P* value formatting, Word document export, and methods text generation are all handled in a single function call. The output tables are ready to paste directly into a submission.

The time savings come entirely from automating the administrative work (formatting, test selection bookkeeping, and methods writing), not from bypassing statistical rigour. Every decision follows established criteria: normality is assessed with the Shapiro-Wilk test applied per group; the Fisher's exact / Chi-squared switch follows the Cochran (1954) expected-cell criterion; odds ratios are unadjusted, with the first factor level of the grouping variable as the reference; and the auto-generated methods paragraph covers the statistical approach used, ready to serve as a starting draft for a manuscript's statistical methods section. The output is appropriate for submission to peer-reviewed clinical journals.

For analyses that go beyond descriptive statistics and standard group comparisons, a biostatistician remains the right resource. TernTables is designed to free up that time for the work that actually requires it.

Descriptive summaries (Table 1), two-group comparisons (with optional odds
ratios), and three-group comparisons are all supported, for continuous,
binary, and categorical variables. Numeric variables can be designated as
ordinal via `force_ordinal`, bypassing normality testing in favour of
median/IQR and nonparametric tests.

## Installation

Install from R-universe (no additional tools required):

```r
install.packages("TernTables",
                 repos = c("https://jdpreston30.r-universe.dev",
                           "https://cloud.r-project.org"))
```

Or install the development version directly from GitHub:

```r
# install.packages("devtools")
devtools::install_github("jdpreston30/TernTables", build_vignettes = TRUE)
```

A full walkthrough is available in the package vignette (built automatically when installing from R-universe; use `build_vignettes = TRUE` with the GitHub install):

```r
vignette("getting-started", package = "TernTables")
```

## Example Data

The bundled `tern_colon` dataset is derived from `survival::colon`
([Moertel et al., 1990](https://doi.org/10.1056/NEJM199002083220602)). It is a reformatted subset
restricted to the recurrence endpoint (`etype == 1`), providing one row per
patient (n = 929) with clinically labelled factor levels and descriptive column
names. See `?tern_colon` and `data-raw/tern_colon.R` for the full
pre-processing pipeline.

```r
library(TernTables)
data(tern_colon)
```

## Functions

### `ternD()` — Descriptive summary table

Generates a single-column summary of all variables without group comparisons.

```r
tbl_descriptive <- ternD(
  data               = tern_colon,
  exclude_vars       = c("ID"),
  output_docx        = "descriptive.docx"
)
```

- Continuous variables: mean ± SD or median [IQR] based on Shapiro-Wilk test
- Binary / categorical: n (%) with optional hierarchical sub-rows
- Use `force_ordinal` to treat specific numeric variables as ordinal

### `ternG()` — Grouped comparison table

Use `ternG()` to compare variables between two or more groups. Set `OR_col = TRUE` to add unadjusted odds ratios with 95% CI for binary variables in two-group comparisons (Fisher's exact or Wald method, chosen automatically based on expected cell counts; Cochran criterion).

**Two-group comparison:**

```r
tbl_2group <- ternG(
  data               = tern_colon,
  exclude_vars       = c("ID"),
  group_var          = "Recurrence",
  consider_normality = TRUE,
  OR_col             = TRUE,
  output_docx        = "two_group.docx"
)
```

**Three-group comparison:**

```r
tbl_3group <- ternG(
  data               = tern_colon,
  exclude_vars       = c("ID"),
  group_var          = "Treatment_Arm",
  group_order        = c("Observation", "Levamisole", "Levamisole + 5FU"),
  consider_normality = TRUE,
  output_docx        = "three_group.docx"
)
```

Omnibus *P* values are reported for 3+ group comparisons; pairwise post-hoc
comparisons are not performed. Odds ratios are not available for 3+ groups.

Statistical tests applied automatically:

| Variable type | 2 groups | 3+ groups |
|---|---|---|
| Continuous, normal | Welch's *t*-test | ANOVA |
| Continuous, non-normal | Wilcoxon rank-sum | Kruskal-Wallis |
| Binary / Categorical | Fisher's exact or Chi-squared | Fisher's exact or Chi-squared |
| Ordinal (`force_ordinal`) | Wilcoxon rank-sum | Kruskal-Wallis |

Fisher's exact is used when any expected cell count is < 5 (Cochran criterion).
Normality is assessed with the Shapiro-Wilk test per group; a variable is treated
as normally distributed only if all groups pass (p > 0.05). If any group has fewer
than 3 observations, normality cannot be evaluated and the nonparametric test is
used (conservative fail-safe).

### `word_export()` — Format and export to Word

Formats any TernTables result tibble as a flextable and writes to `.docx`.
Use `category_start` to insert bold section-header rows between variable groups.

```r
word_export(
  tbl      = tbl_2group,
  filename = "two_group.docx",
  category_start = c(
    "Patient Demographics"  = "Age (yr)",
    "Surgical Findings"     = "Colonic Obstruction",
    "Tumor Characteristics" = "Positive Lymph Nodes (n)"
  )
)
```

### `write_methods_doc()` — Auto-generated methods paragraph

Inspects the results tibble and writes a Word document containing a standard
statistical methods paragraph covering the normality assessment approach,
test types used, significance threshold, and (when odds ratios are reported)
the estimation method and reference group. Suitable as a starting draft for
a manuscript methods section.

```r
write_methods_doc(
  tbl      = tbl_2group,
  filename = "methods.docx"
)
```

### `val_p_format()` / `val_format()` — Formatting utilities

```r
val_p_format(0.0432)       # "0.043"
val_p_format(0.000012)     # "1E-5"
val_format(72.4, 8.1)  # "72.4  +-  8.1"
```

## Output

Every `ternD()` and `ternG()` call returns a tibble and, when `output_docx` is specified, writes a publication-ready `.docx` file directly. The Word
output uses Arial, consistent padding, bold significant *P* values, and optional
bold category-section headers. No post-processing is required before pasting
into a manuscript.

The tibble can also be:

- Passed to `word_export()` for additional formatting control (e.g. `category_start`)
- Written to Excel with `writexl::write_xlsx()`
- Inspected or further manipulated in R

## License

[![License: MIT](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/jdpreston30/TernTables/blob/main/LICENSE)

This project is licensed under the MIT License.

---

**Developed and maintained by:**

- [Joshua D. Preston](https://orcid.org/0000-0001-9834-3017) [![ORCID](https://img.shields.io/badge/ORCID-0000--0001--9834--3017-brightgreen?logo=orcid)](https://orcid.org/0000-0001-9834-3017)
- [Helen Abadiotakis](https://orcid.org/0009-0002-8268-927X) [![ORCID](https://img.shields.io/badge/ORCID-0009--0002--8268--927X-brightgreen?logo=orcid)](https://orcid.org/0009-0002-8268-927X)
- [Ailin Tang](https://orcid.org/0009-0007-8715-1678) [![ORCID](https://img.shields.io/badge/ORCID-0009--0007--8715--1678-brightgreen?logo=orcid)](https://orcid.org/0009-0007-8715-1678)
- [Maria V. Aslam](https://orcid.org/0000-0002-0036-3851) [![ORCID](https://img.shields.io/badge/ORCID-0000--0002--0036--3851-brightgreen?logo=orcid)](https://orcid.org/0000-0002-0036-3851)
- [Clayton J. Rust](https://orcid.org/0000-0001-5929-0733) [![ORCID](https://img.shields.io/badge/ORCID-0000--0001--5929--0733-brightgreen?logo=orcid)](https://orcid.org/0000-0001-5929-0733)
- [Joshua L. Chan](https://orcid.org/0000-0001-7220-561X) [![ORCID](https://img.shields.io/badge/ORCID-0000--0001--7220--561X-brightgreen?logo=orcid)](https://orcid.org/0000-0001-7220-561X)

Feedback and contributions are welcome.

