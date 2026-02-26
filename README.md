# TernTables
**TernTables** is a lightweight R package for generating clean summary tables with appropriate statistical tests. It supports two-level and three-level group comparisons for binary, continuous, and ordinal variables, and includes options for exporting tables to Word and Excel.

## 🚀 Installation

You can install the development version of TernTables from GitHub:

```r
# install.packages("devtools")
devtools::install_github("jdpreston30/TernTables")
```

### 🏥 Example Data

Examples and the vignette use the `colon` dataset from the `survival` package, which ships with every R installation. No additional packages are required to run the examples. The dataset contains 929 patients from a colon cancer adjuvant chemotherapy trial (Moertel et al., 1990).

## 📦 Functions

### `ternG()`

Generates summary tables for either **binary** or **3-level categorical** grouping variables. Automatically applies appropriate statistical tests based on variable type and number of groups:

- **Continuous variables**:
  - **2 groups**: Welch’s *t*-test or Wilcoxon rank-sum (based on Shapiro-Wilk test for normality)
  - **3+ groups**: ANOVA or Kruskal-Wallis, based on both normality (Shapiro-Wilk) and homogeneity of variance (Levene’s test)

- **Categorical variables**:
  - Chi-squared test or Fisher’s exact test (based on expected cell counts)

- **Ordinal variables** (defined via `force_ordinal`):
  - Wilcoxon rank-sum (2 groups) or Kruskal-Wallis (3+ groups)

## 📝 Examples

### Two-level comparison

```r
library(TernTables)
```

### Three-level comparison

```r
library(TernTables)
ternG(
  data = your_data,
  group_var = "grade",  # 3-level variable (e.g., 3, 4, 5)
  exclude_vars = c("ID"),
  force_ordinal = c("ISS", "GCS"),
  group_order = c(3, 4, 5),
  output_xlsx = "summary_3v.xlsx",
  output_docx = "summary_3v.docx"
)
```

---

### `ternD()`

Generates **descriptive-only** summary tables without group comparisons. Uses a single "Summary" column format for clean, publication-ready output. Useful for baseline cohort description or single-group studies.

**Summary format behavior**:
- **Default behavior** (`consider_normality = FALSE`): Mean ± SD for numeric variables
- **Normality-aware** (`consider_normality = TRUE`): Shapiro-Wilk test determines mean ± SD vs median [IQR]
- **Force ordinal** (`force_ordinal`): Always shows median [IQR] for specified variables, regardless of other settings
- **Categorical variables**: Counts (%) with optional hierarchical formatting

## 📝 Example

```r
ternD(
  data = your_data,
  exclude_vars = c("ID"),
  force_ordinal = c("severity_score", "stage"),
  consider_normality = TRUE,
  output_xlsx = "summary_descriptive.xlsx",
  output_docx = "summary_descriptive.docx"
)
```

---

## 📤 Output

- Returns a tibble with:
  - Variable name with appropriate indentation
  - Summary statistics in a single "Summary" column (per group if using `ternG()`, single overall summary if using `ternD()`)  
  - p-value and test name (`ternG()` only)
  - Odds ratios with 95% confidence intervals (for Chi squared and Fisher's) (Optional via OR_col argument)
- Optionally exports to `.xlsx` and `.docx` files.


## 📄 License

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

Feedback and contributions are welcome!
