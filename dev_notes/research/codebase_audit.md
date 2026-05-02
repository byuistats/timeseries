# Codebase Audit — Oddities & Cleanup Needs

_Audited: 2026-05-01_

---

## Critical Issues (Potential Bugs)

### 1. Syntax error in `common_functions.R` — lines 710, 712, 714
Variable names use hyphens (`last-actual_t`) instead of underscores (`last_actual_t`). In R, hyphens in variable names are interpreted as subtraction operators, so these lines will produce runtime errors.

### 2. Two broken functions left in `common_functions.R`
- `color_specific_row()` (~line 357) — marked `# -------- THIS ONE IS NOT WORKING ----------`
- `color_last_row()` (~line 365) — same marker

Neither function is removed or replaced. They should either be fixed or deleted.

### 3. `autoround_df()` marked non-functional — `common_functions.R` line 78
Comment reads `# THIS IS NOT WORKING RIGHT>>>>`. The function remains in the file with no resolution.

---

## Files to Delete

| File / Directory | Reason |
|---|---|
| `probablyCanDelete/` | Name says it all; contains only `temp.csv` (524 KB) |
| `_deleteme.qmd` | Explicitly marked for deletion |
| `archive/chapter_5_lesson_3.qmd` | Old version archived in a directory with no other purpose |
| `Visuals_Plotly_Copy/chapter_6_copy.qmd` | **Empty file — 0 bytes** |
| `homework/homework_1_2old.qmd` | Superseded old version |
| `homework/homework_1_5_solution-old.qmd` | Superseded old version |
| `NewSequence/Chapter_2_New_old.qmd` | Explicitly named old version |
| `docs/NewSequence/Chapter_2_New_old.qmd` | Duplicate of above in docs mirror |

---

## Naming Issues

### Misspelled directory
`ShinyApps/4_Simluation/` — should be `4_Simulation/`. The two adjacent directories (`5_Simulation/`, `6_Simulation/`) are spelled correctly, making this inconsistency easy to miss.

### Inconsistent file/folder naming conventions
- Main lessons use `snake_case`: `chapter_1_lesson_1.qmd`
- `NewSequence/` uses `CamelCase`: `Chapter_2_New.qmd`
- Generated HTML mixes patterns: `_chapter_6_lesson_2.html` vs `_ch6_lesson2_chocolate.html`

### Inconsistent R code style in `common_functions.R`
- Modern native pipe (`|>`) used in lines 71–227
- Legacy `%>%` pipe still used in lines 268–277

---

## Duplicate / Redundant Content

### `Visuals_Plotly_Copy/` directory (~37 files)
Parallel copies of main lesson `.qmd` files with minor Plotly-specific modifications, including a near-duplicate `common_functions_copy.R`. If these represent a real alternative visualization track, they should be on a branch or managed via a config flag rather than duplicated files.

### Two versions of the same Shiny app
- `ShinyApps/6_Simulation/chapter_6_lesson_1.R`
- `ShinyApps/6_Simulation/chapter_6_lesson_1_new/app.R`

One should be canonical; the other removed.

### Near-identical Shiny apps
`ShinyApps/4_Simluation/AR1.R` and `RandomWalk.R` are over 90% identical (same UI, same server skeleton). Only the simulation logic differs. A shared template or base function would eliminate the duplication.

### `docs/` mirrors all source content
The `docs/` directory holds rendered output (HTML, `_files/`, data mirrors). This is expected for a Quarto publish workflow, but it means most source files have a corresponding rendered duplicate in `docs/`. Confirm this is intentional (GitHub Pages deployment) and not an accidental commit of build artifacts.

---

## Dead / Commented-Out Code in `common_functions.R`

| Lines | Description |
|---|---|
| 253–263 | Large block of commented-out example usage for `format_cells()` |
| 325–334 | Commented-out testing code for `compute_moving_average()` |
| 302 | Call to `convert_df_to_char(decimals)` inside `insert_blank_last_row()` — result not assigned (dead expression) |
| 662–675 | Large commented-out block for alternate Holt-Winters scenarios |
| 569 | `#### NOTE THIS ISSUE!!!` — unresolved flag with no explanation |

---

## Fragile Package Load Order (`common_functions.R`)

Several packages must be loaded in a specific order to avoid namespace conflicts, and this is only enforced by comment:

- `plotly` must be loaded before `tidyverse` (comment: "High conflict potential")
- `MASS` must be loaded before `tidyverse`
- `nlme` must be loaded before `feasts` to avoid `ACF()` being masked

These fragile orderings are undocumented in any README. If the load order ever shifts (e.g., someone adds a package at the top), behavior breaks silently.

---

## Likely Unused Functions in `common_functions.R`

The following functions have no apparent call sites in the main codebase:

- `right()` / `left()` — string helpers, seemingly unused
- `deg2rad()` / `rad2deg()` — no usage found in `.qmd` or `.R` files
- `get_toy_data()` — only appears in isolated example/scratch files

These should be confirmed unused and removed, or moved to a utilities file if genuinely needed.

---

## Very Large Files (Consider Splitting)

| File | Lines |
|---|---|
| `chapter_5_lesson_2.qmd` | ~1,630 |
| `chapter_4_lesson_4.qmd` | ~1,596 |
| `chapter_4_lesson_2.qmd` | ~1,532 |
| `chapter_6_lesson_2.qmd` | ~1,374 |
| `_chapter_6_lesson_2.qmd` | ~1,237 |
| `harmonics_part_2.qmd` | ~1,202 |
| `NewSequence/Chapter_2_New.qmd` | ~1,173 |
| `chapter_1_lesson_5.qmd` | ~1,144 |

---

## Near-Empty Placeholder Files

The following appear to be stubs with no real content yet:

- `resources/tidyr/tidyr.qmd` (178 bytes)
- `resources/tsbox/tsbox.qmd` (178 bytes)
- `resources/base-r/base-r.qmd` (182 bytes)
- `resources/quarto/quarto.qmd` (182 bytes)
- `resources/rstudio/rstudio.qmd` (194 bytes)
- `resources/timeseries/timeseries.qmd` (195 bytes)
- `outcomes/` files (~218 bytes each — appear to be copy-paste templates)
- Several `NewSequence/Chapter_*_New.qmd` files (243–760 bytes)

If these are intentional placeholders, add a comment to that effect. If they are stale scaffolding, remove them.

---

## Minor / Low Priority

- `chapter_2_lesson_1_nonshinycode.R` lines 153–164: 11 consecutive "start editing again here" comment lines — should be collapsed to one.
- `testpackage.R` line 38: Commented-out alternative date line with no explanation.
- Magic numbers in `ShinyApps/4_Simluation/RandomWalk.R` line 90–91: `sd(df$x) * nrow(data) * 120 / 2000` — the constants 120 and 2000 are unexplained.
- `rsconnect/` directory committed to repo — deployment metadata; arguably should be in `.gitignore`.
