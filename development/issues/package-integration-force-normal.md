# Package Integration: force_ordinal and force_normal — Per-Variable Normality Override Arguments

Package version: 1.6.3.9024

---

## What changed in the package

### `force_ordinal` (existing — name unchanged)

`force_ordinal` has always existed and its name is not changing. This note
documents it here for completeness so the APP integration is described in full.

- **Function(s):** `ternG()`, `ternD()`
- **Type:** `character vector`, default `NULL`
- **Behavior:** Any variable listed here bypasses all normality assessment and
  is always summarized as median [IQR] and compared with non-parametric tests
  (Wilcoxon rank-sum or Kruskal-Wallis). Takes priority over `force_normal` if
  a variable appears in both.

### `force_normal` (new — added v1.6.3.9024)

- **Function(s):** `ternG()`, `ternD()`
- **Type:** `character vector`, default `NULL`
- **Behavior:** Any variable listed here bypasses all normality assessment
  (Gates 1–4 under ROBUST, or Shapiro-Wilk under `consider_normality = TRUE`)
  and is always summarized as mean ± SD and compared with parametric tests
  (Welch's *t*-test for 2 groups, Welch ANOVA for 3+ groups). This is the
  per-variable parametric counterpart to `force_ordinal`. `force_ordinal` wins
  if a variable appears in both.
- **Intended use:** For R users who have reviewed the normality routing and
  want to manually override a variable that was routed to non-parametric back
  to parametric. This is an advanced/expert argument — the APP does not need to
  expose it directly, but the UI override mechanism described below maps onto it.

### No changes to `consider_normality` or any other argument.

---

## Web app integration required

### Step 2 — Variable Summary Preview: override column redesign

The current Variable Summary Preview table in Step 2 has a column labelled
**"Use Median [IQR]"** with a checkbox per variable. When checked, the variable
is added to `force_ordinal`. This column and its behavior need to be updated to
support bidirectional overrides now that `force_normal` exists.

#### Rename the column

**Old:** `Use Median [IQR]`
**New:** `Override Default Choice`

#### Change the checkbox to a contextual selector

Instead of a simple checkbox, each row in the Override column should show a
control that reflects the current default routing for that variable and allows
the user to flip it. The logic:

- If the variable's default routing (from `classify_normality()` / ROBUST) is
  **non-parametric** (median [IQR]):
  - Show a checkbox or toggle labelled **"Change to Mean ± SD (parametric)"**
  - If checked → add variable to `force_normal`

- If the variable's default routing is **parametric** (mean ± SD):
  - Show a checkbox or toggle labelled **"Change to Median [IQR] (non-parametric)"**
  - If checked → add variable to `force_ordinal`

This way the label is always the *opposite* of the current default — the user
is explicitly choosing to override, and the label tells them exactly what the
override will do.

**Implementation note:** The `classify_normality()` API response already
provides the per-variable routing result. The "default" label can be read
directly from that response — no new API call needed.

#### Add a footnote/note below the override table

Below the Variable Summary Preview table, add a brief note (styled as a muted
info block or tooltip-accessible footnote) with the following text or close
equivalent:

> **About these overrides:** TernTables automatically selects between mean ± SD
> (parametric) and median [IQR] (non-parametric) based on sample size and the
> shape of your data. For most variables, this choice has limited impact on
> statistical conclusions. For variables where it matters most — such as length
> of stay or cost data — the algorithm is specifically designed to detect and
> handle them correctly. If you disagree with the automatic choice for a
> specific variable, you can override it here. When in doubt, consult a
> statistician before changing the default.

---

### Plumber API changes (`R/plumber.R`)

#### `/api/analyze` and `/api/analyze-descriptive` endpoints

Both endpoints need to read `force_normal` from `req$body` and pass it through
to `ternG()` / `ternD()` alongside the existing `force_ordinal`.

```r
# Read from request body (alongside existing force_ordinal)
force_normal  <- req$body$force_normal   # character vector or NULL
force_ordinal <- req$body$force_ordinal  # character vector or NULL (already exists)

# Pass to ternG()
ternG(..., force_ordinal = force_ordinal, force_normal = force_normal, ...)

# Pass to ternD()
ternD(..., force_ordinal = force_ordinal, force_normal = force_normal, ...)
```

Both the main analysis call and the citation-free preview call need `force_normal`.

---

### Frontend API changes (`client/src/api/client.js`)

Add `force_normal` to the POST body of `analyzeData()` (and the descriptive
equivalent), alongside the existing `force_ordinal`:

```js
force_ordinal: state.forceOrdinal,   // already exists
force_normal:  state.forceNormal,    // new
```

---

### State changes (`App.jsx` / `Step2.jsx`)

Add a new state variable for `force_normal` alongside the existing
`force_ordinal` state:

```js
const [forceNormal, setForceNormal] = useState([]);
```

The Variable Summary Preview component should:
1. Read each variable's default routing from the `classify_normality()` result
2. Render the override column as described above (contextual label showing the
   opposite of the default)
3. On check: add the variable to either `forceNormal` or `forceOrdinal`
   depending on which direction the override goes — never both
4. On uncheck: remove from whichever list it was added to

---

### Reproducible R script (`R/helpers/rscript.R`)

If `force_ordinal` is already emitted in the generated `_reproduce.R` script
when non-empty, add `force_normal` under the same condition:

```r
if (!is.null(force_normal) && length(force_normal) > 0) {
  lines <- c(lines, paste0('  force_normal = c("', paste(force_normal, collapse = '", "'), '"),'))
}
```

---

## Notes / open questions

- The control in the override column can be a checkbox, a small toggle, or a
  select dropdown — whichever fits the existing Step 2 UI style. The key
  requirement is that the label dynamically reflects what the override will do
  (i.e., the opposite of the current default), not a static label.
- If a variable is in `force_ordinal` from a previous session or pre-set
  config, the override column should show it as already checked with the
  "Change to Median [IQR]" label. Same logic applies if `force_normal` is
  pre-populated.
- The footnote text is intentionally non-technical. It is aimed at clinical
  researchers who may not know what "parametric" means. Do not use the word
  "heuristic" in the UI-facing text.
- `force_normal` is intentionally not surfaced as a prominently labelled
  feature — it is an expert escape hatch. The override column UI is the primary
  mechanism; the argument name itself stays in the background.
