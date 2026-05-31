### Code Revision Timeline

**1. Package Dependency Clarity** (Target: **Saturday, May 30**)
* [x] Reduce to minimal common packages
* [x] Render and change any lesson that fails rendering
* [x] Rinse and repeat until website fully rendered
* [x] Update the resource blog
* [x] Change packages in homeworks 
* [ ] ...and keys

**2. GLS to ARIMA Migration** (Target: **Thursday, June 4**)
* **Essential Files:** `5.1`, `5.2`, `6.2`, `5.1_homework`, `_chapter_5_lesson_1_outcomes.qmd`, `resource_blog`

**3. Polyroot to gg_arma Migration** (Target: **Thursday, June 18**)
* **Essential Files:** `4.3`, `4.4`, `6.1`, `4.3_homework` (x2), `4.4_homework` (x2)

**4. Code Style Standardization & Rollout** (Hard Deadline: **Wednesday, July 15**)
* **Audit** (Target: **Saturday, June 27**): Skim student submissions to identify common anti-patterns.
* **Draft Guide** (Target: **Thursday, July 2**): Pull examples to finalize and lock in the official style guide ahead of the Friday team sync.
* **Phase 1 Update** (Target: **Saturday, July 4**): Implement easy codebase changes (find/replace, basic formatting).
* **Phase 2 Update** (Target: **Thursday, July 9**): Complete medium changes (refactoring specific code chunks in keys and homeworks).
* **Phase 3 Update** (Target: **Wednesday, July 22**): Finalize hard changes (rebuilding complex examples in the textbook text) to close out the project.

---

### Code Revision Impact Analysis

| Level      | Modification                                       | Scope / Impact                               |
| :--------- | :------------------------------------------------- | :------------------------------------------- |
| **High**   | Switch away from GLS in favor of ARIMA             | 5 files (Code & Content revision)            |
| **High**   | Change root analysis to use unit circle charts     | 5 files (Code & Content revision)            |
| **High**   | Package import clarity                             | ~85 files (Automated find/replace potential) |
| **High**   | Clean tidyverts version migration to `ggtime`      | 100+ files (Very minimal effort)             |
| **Medium** | Improved code explanations & bug tips              | Dispersed throughout textbook                |
| **Medium** | Align all keys, homeworks, and text to style guide | Global codebase                              |
| **Medium** | Clarify skewness code and calculation              | 2 lessons (5.2, 6.2)                         |
| **Low**    | Localize Shiny Apps using `shinylive`              | Removes external server dependency           |
| **Low**    | Visual formatting improvements                     | Pretty tables, use `geom_time_line`          |