# Old vs. New File Analysis — Assumptions Challenged

_Audited: 2026-05-01_

---

## The Assumption Under Review

> "Anything 'old' should be removed; naming conventions should be normalized to 'new'."

That assumption is **partially right but wrong in three important ways.** The details follow.

---


## File-by-File Verdict

### Safe to delete — no review needed

| File | Reason |
|---|---|
| `homework/homework_1_2old.qmd` | New version is an intentional redesign. Nothing in the old version is missing from current coursework — the scope change (3 datasets → 1) was deliberate. |
| `_deleteme.qmd` | Incomplete fragment (~90% overlap with `archive/chapter_5_lesson_3.qmd`). Named for deletion. The content it contains is preserved elsewhere. |
| `ShinyApps/6_Simulation/chapter_6_lesson_1.R` (the root-level one) | The `chapter_6_lesson_1_new/app.R` version is a proper superset: active package installs, PACF visualization added, cleaner layout. The old file is strictly superseded. |
| `docs/NewSequence/Chapter_2_New_old.qmd` | Rendered artifact of a source file that itself needs human review (see below). The `docs/` copy is downstream. |
| `outcomes/new/` (entire directory) | Byte-for-byte duplicate of `outcomes/`. Not a staging area — zero differences across all 29 files. |
| `homework/Archive/homework_3_3.html` | Rendered artifact of the archived `homework_3_3.qmd` below; no independent value. |

---

### Requires human review before any action

| File | Why human judgment is needed |
|---|---|
| `NewSequence/Chapter_2_New_old.qmd` | Contains ~700 lines of content (stochastic processes, dice example, derivations) not present in the new version. Only the author can confirm whether this content was deliberately removed or accidentally lost during the rewrite. If it was cut on purpose, the file is safe to delete. If it was lost, it needs to be recovered. |
| `homework/homework_1_5_solution-old.qmd` | No replacement file exists. Before deleting, confirm: (a) is `homework_1_5.qmd` still assigned? (b) is the solution intentionally unpublished, or was a new solution file simply never created? If the assignment is active, this file should be renamed (drop the `-old` suffix) rather than deleted. |
| `archive/chapter_5_lesson_3.qmd` | Unique content with no root-level counterpart. Confirm whether this lesson was intentionally removed from the course sequence or just accidentally left in archive rather than promoted back to root. |
| `homework/Archive/homework_3_3.qmd` | Topic changed wholesale — archive uses US Unemployment data (2 questions); current uses CDC self-harm data with a pastoral note (3 questions). These are different assignments, not revisions. Confirm: retain as an alternative version (rename to `_alt`) or delete. |
| `homework/Archive/homework_5_2.qmd` | Filename reused for a completely different assignment. Archive = harmonic seasonal models with derivations; current = model selection (AIC/BIC/AICc) on retail data. Confirm whether harmonic model homework was intentionally dropped from the course. |
| `homework/Archive/homework_5_3.qmd` | Only copy; no counterpart in active `homework/`. Covers CO2 data, three trend types (linear/quadratic/exponential), and harmonic seasonality. Confirm whether Chapter 5 was intentionally shortened to 2 homeworks or this needs to be re-integrated. |
| `homework/Archive/homework_5_4.qmd` | Only copy. Covers non-linear exponential models and log-transformation on retail data. Same question as 5_3. |
| `homework/Archive/homework_5_5.qmd` | Only copy. Covers log-transform bias correction (empirical vs. log-normal adjustment). Same question as 5_3 and 5_4. |

---

### The `NewSequence/` stub files — leave them alone for now

`Chapter_1_New.qmd`, `Chapter_3_New.qmd` through `Chapter_7_New.qmd`, and `Chapter_A1_New.qmd` are scaffolding for an in-progress restructuring. They should neither be deleted (someone is working toward them) nor renamed (the `_New` suffix signals "this is the new organizational scheme" — it's intentional, not sloppy). Once each chapter is complete and replaces the lesson files, the suffix can be dropped and the old lessons deleted.

---

## What the Naming Pattern Actually Tells You

| Pattern | What it signals | Action |
|---|---|---|
| `*_old.qmd` or `*-old.qmd` | Replaced by a sibling file *in the same directory* | Verify the sibling exists and covers the same content, then delete |
| `archive/*` | Removed from active course but not confirmed deletable | Check whether content is covered elsewhere before deleting |
| `NewSequence/*_New.qmd` | Part of an in-progress reorganization track | Do not touch until the reorganization is complete |
| `*_new/app.R` | Shiny app iteratively improved; old `.R` sits alongside | Old `.R` is the one to delete once the `_new/` version is confirmed stable |
| `_deleteme.qmd` | Explicit deletion marker | Safe to delete |
| `outcomes/new/` | Exact duplicate of parent directory | Delete entirely |
| `homework/Archive/*` | Previous or dropped assignment versions; may differ substantially from current | Check for current counterpart and whether content is still needed |

---

## Recommended Order of Operations

1. **Immediately safe:** Delete `homework_1_2old.qmd`, `_deleteme.qmd`, `ShinyApps/6_Simulation/chapter_6_lesson_1.R` (old flat-file version), `outcomes/new/` (entire directory), and `homework/Archive/homework_3_3.html`.
2. **Rename, don't delete:** If `homework_1_5.qmd` is still an active assignment, rename `homework_1_5_solution-old.qmd` → `homework_1_5_solution.qmd`.
3. **Human review required:** (a) Open `Chapter_2_New_old.qmd` alongside `Chapter_2_New.qmd` and confirm the dropped content was intentional. (b) Decide whether `homework/Archive/homework_3_3.qmd` is a useful alternative or dead draft. (c) Confirm whether Chapter 5 was intentionally shortened — this determines the fate of `homework_5_2.qmd` (archive), `homework_5_3.qmd`, `homework_5_4.qmd`, and `homework_5_5.qmd`.
4. **Defer:** Leave `NewSequence/` stubs and `archive/chapter_5_lesson_3.qmd` until the restructuring direction is clearer.
5. **Do not normalize `NewSequence/` naming** until the reorganization is complete — the `_New` suffix is load-bearing as a track identifier, not a sloppy naming choice.

