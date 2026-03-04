# Post-CRAN Acceptance To-Do List

Tasks to complete after TernTables v1.6.3 is accepted and live on CRAN.
Check the package page at https://CRAN.R-project.org/package=TernTables to
confirm it is live before doing any of the below.

---

## README.md — Badges

Add these three badges directly after the existing Web App and License badges:

```markdown
[![CRAN status](https://www.r-pkg.org/badges/version/TernTables)](https://CRAN.R-project.org/package=TernTables)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/TernTables)](https://cran.r-project.org/package=TernTables)
[![CRAN total downloads](https://cranlogs.r-pkg.org/badges/grand-total/TernTables)](https://cran.r-project.org/package=TernTables)
```

The version badge is dynamic — it updates automatically with each new CRAN
release. The download badges are cosmetic and take a few days to show real
numbers after acceptance.

---

## DESCRIPTION — URL field

Add the CRAN package page to the URL field:

```
URL: https://tern-tables.com/, https://CRAN.R-project.org/package=TernTables, https://github.com/jdpreston30/TernTables
```

---

## README.md — Installation section

Update the installation section to lead with the standard CRAN install command,
which is the simplest and most trusted path for new users:

```r
install.packages("TernTables")
```

Place this above the R-universe and GitHub install options. The R-universe
install can remain as the "latest features" path but should be secondary.

---

## CITATION file (inst/CITATION)

Check and update the CITATION file to reference the CRAN package page. A
standard CRAN citation looks like:

```r
citEntry(
  entry    = "Manual",
  title    = "TernTables: Publication-Ready Summary Tables and Statistical Testing for Clinical Research",
  author   = personList(as.person("Joshua D. Preston"), ...),
  year     = "2026",
  note     = "R package version 1.6.3",
  url      = "https://CRAN.R-project.org/package=TernTables"
)
```

Also consider adding a `citation("TernTables")` call to the README so users
know how to cite the package in manuscripts.

---

## GitHub Release

Tag a GitHub release for v1.6.3 with the NEWS.md entry for this version as
the release notes. This is standard practice and gives a clean version history
on GitHub.

Steps:
1. Go to https://github.com/jdpreston30/TernTables/releases/new
2. Tag: `v1.6.3`
3. Title: `TernTables 1.6.3`
4. Body: paste the `# TernTables 1.6.3` section from NEWS.md

---

## tern-tables.com — Web App

Update the web app footer/about page to note that the package is now
"Available on CRAN" with a link. This adds credibility to the web app and
provides a clear attribution chain for IRB/grant documentation.

---

## pkgdown Site (Optional but recommended)

Consider setting up a pkgdown site hosted on GitHub Pages. It gives a polished
documentation website at e.g. `jdpreston30.github.io/TernTables/` with:
- Function reference pages generated from Rd files
- Vignette rendered as a searchable article
- Changelog from NEWS.md
- Automatic CRAN badge in the sidebar

Setup is a single devtools call:
```r
# install.packages("pkgdown")
pkgdown::build_site()
usethis::use_pkgdown_github_pages()
```

If you add a pkgdown site, update `URL:` in DESCRIPTION to include it.

---

## future Submissions — Version Bump Protocol

For every future CRAN submission:
1. Increment `Version:` in DESCRIPTION (e.g. 1.6.3 → 1.6.4 or 1.7.0)
2. Add a new `# TernTables X.Y.Z` section at the top of NEWS.md
3. Update cran-comments.md — change "Resubmission" header to "New submission"
   or keep "Resubmission" if responding to reviewer comments
4. Run `devtools::document()`, `devtools::check()`, `devtools::check_win_devel()`
5. Commit, then `devtools::submit_cran()`

---

## R-Universe

R-universe auto-syncs from GitHub — no action needed. But after CRAN
acceptance, check https://jdpreston30.r-universe.dev to confirm both the
CRAN and dev versions are listed correctly.
