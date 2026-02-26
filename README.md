# TernTables

**TernTables** is designed for clinical researchers who need publication-ready
summary statistics tables without manual formatting. Given a data frame and an
optional grouping variable, a single function call handles variable type
detection, statistical test selection, p-value formatting, Word export, and
auto-generated methods text — producing output that is ready to paste directly
into a manuscript.

Descriptive summaries (Table 1), two-group comparisons (with optional odds
ratios), and three-group comparisons are all supported, for continuous,
binary, categorical, and ordinal variables.

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("jdpreston30/TernTablesR")
```

## Example Data

The bundled `tern_colon` dataset is derived from `survival::colon`
(Moertel et al., 1990; Laurie et al., 1989). It is a reformatted subset
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
TernDesc <- ternD(
  data               = tern_colon,
  exclude_vars       = c("ID"),
  consider_normality = TRUE,
  output_docx        = "descriptive.docx"
)
```

- Continuous variables: mean ± SD or median [IQR] based on Shapiro-Wilk test
- Binary / categorical: n (%) with optional hierarchical sub-rows
- Use `force_ordinal` to treat specific numeric variables as ordinal

### `ternG()` — Grouped comparison table

Use `ternG()` to compare variables between two or more groups. Set `OR_col = TRUE` to add\nunadjusted odds ratios with 95% CI for binary variables in two-group comparisons\n(Fisher's exact or Wald method, chosen automatically based on expected cell counts —\nCochran criterion).

**Two-group comparison:**

```r
Tern2v <- ternG(
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
Tern3v <- ternG(
  data               = tern_colon,
  exclude_vars       = c("ID"),
  group_var          = "Treatment_Arm",
  group_order        = c("Observation", "Levamisole", "Levamisole + 5FU"),
  consider_normality = TRUE,
  output_docx        = "three_group.docx"
)
```

Omnibus p-values are reported for 3+ group comparisons; pairwise post-hoc
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
used (conservative fail-safe). For 3+ group comparisons, omnibus p-values are
reported; pairwise post-hoc comparisons are not performed.

### `word_export()` — Format and export to Word

Formats any TernTables result tibble as a flextable and writes to `.docx`.
Use `category_start` to insert bold section-header rows between variable groups.

```r
word_export(
  tbl      = Tern2v,
  filename = "two_group.docx",
  category_start = c(
    "Patient Demographics"  = "Age (yr)",
    "Surgical Findings"     = "Colonic Obstruction",
    "Tumor Characteristics" = "Positive Lymph Nodes (n)"
  )
)
```

### `write_methods_doc()` — Auto-generated methods paragraph

Inspects the results tibble and writes a Word document containing a complete
statistical methods paragraph — listing every test that was actually applied,
the normality assessment approach used, the significance threshold, and (when
odds ratios are reported) the estimation method and reference group. The output
is ready to copy directly into a manuscript methods section with little or no
editing.

```r
write_methods_doc(
  tbl      = Tern2v,
  filename = "methods.docx"
)
```

### `val_p_format()` / `val_format()` — Formatting utilities

```r
val_p_format(0.0432)       # "0.043"
val_p_format(0.000012)     # "1E-5"
val_format(72.4, 8.1)  # "72.4 ± 8.1"
```

## Output

Every `ternD()` and `ternG()` call returns a tibble and — when `output_docx`
is specified — writes a publication-ready `.docx` file directly. The Word
output uses Arial, consistent padding, bold significant p-values, and optional
bold category-section headers. No post-processing is required before pasting
into a manuscript.

The tibble can also be:

- Passed to `word_export()` for additional formatting control (e.g. `category_start`)
- Written to Excel with `writexl::write_xlsx()`
- Inspected or further manipulated in R

## License

[![License: MIT](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)

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

