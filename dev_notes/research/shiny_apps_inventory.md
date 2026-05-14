# Shiny Apps Inventory

_Audited: 2026-05-12_

These apps are not wired through the main lesson code as local imports. The lessons embed the deployed Posit URLs directly with `<iframe>` tags, while the `ShinyApps/` folders hold the app source and `rsconnect/` holds deployment metadata for republishing.

The key pattern is:

1. `ShinyApps/...` contains the app source.
2. `rsconnect/shinyapps.io/.../*.dcf` records the deployment target and hosted URL.
3. The lesson or archive `.qmd` files embed that hosted URL directly in an iframe.

## Current website usage

The table below links each deployed app to the local source file, the deployment metadata, and the page that embeds it.

| Local app source | Deployment record | Hosted URL | Page(s) using it | Status |
| --- | --- | --- | --- | --- |
| [ShinyApps/StochasticRealizations/StochasticRealizations.R](../../ShinyApps/StochasticRealizations/StochasticRealizations.R) | [stochasticrealizations.dcf](../../rsconnect/shinyapps.io/StochasticRealizations/rsconnect/documents/StochasticRealizations.R/posit.byui.edu/sombreroboy/stochasticrealizations.dcf) | `https://posit.byui.edu/content/728a7ddc-97d2-44ee-9977-fc648102b986/` | [chapter_1_lesson_2.qmd](../../chapter_1_lesson_2.qmd#L96) | Used on the current website |
| [ShinyApps/DeterministicRealizations/DeterministicRealizations.R](../../ShinyApps/DeterministicRealizations/DeterministicRealizations.R) | [deterministicrealizations.dcf](../../rsconnect/shinyapps.io/DeterministicRealizations/rsconnect/posit.byui.edu/sombreroboy/deterministicrealizations.dcf) | `https://posit.byui.edu/content/4d25268b-d562-455c-ac47-5a9871770e7c/` | [chapter_1_lesson_2.qmd](../../chapter_1_lesson_2.qmd#L105) | Used on the current website |
| [ShinyApps/CovarianceAndAutoCorrelation/Covariance-and-Correlation.R](../../ShinyApps/CovarianceAndAutoCorrelation/Covariance-and-Correlation.R) | [covarianceandautocorrelation.dcf](../../rsconnect/shinyapps.io/CovarianceAndAutoCorrelation/rsconnect/documents/Covariance-and-Correlation.R/posit.byui.edu/sombreroboy/covarianceandautocorrelation.dcf) | `https://posit.byui.edu/content/564c2e71-3d0b-43a6-8c6f-d402125c8b28/` | [chapter_2_lesson_1.qmd](../../chapter_2_lesson_1.qmd#L210) | Used on the current website |
| [ShinyApps/4_Simulation/WhiteNoise.R](../../ShinyApps/4_Simulation/WhiteNoise.R) | [whitenoise.dcf](../../rsconnect/shinyapps.io/4_Simluation/rsconnect/documents/WhiteNoise.R/posit.byui.edu/sombreroboy/whitenoise.dcf) | `https://posit.byui.edu/content/86061d12-80c9-455c-98ff-aca015c75ed1/` | [chapter_4_lesson_1.qmd](../../chapter_4_lesson_1.qmd#L138) | Used on the current website |
| [ShinyApps/4_Simulation/RandomWalk.R](../../ShinyApps/4_Simulation/RandomWalk.R) | [randomwalks.dcf](../../rsconnect/shinyapps.io/4_Simluation/rsconnect/documents/RandomWalk.R/posit.byui.edu/sombreroboy/randomwalks.dcf) | `https://posit.byui.edu/content/f9a0690a-87ce-423c-9d9c-4dfd0188dd86/` | [chapter_4_lesson_1.qmd](../../chapter_4_lesson_1.qmd#L579) | Used on the current website |
| [ShinyApps/4_Simulation/AR1.R](../../ShinyApps/4_Simulation/AR1.R) | [ar1.dcf](../../rsconnect/shinyapps.io/4_Simluation/rsconnect/documents/AR1.R/posit.byui.edu/sombreroboy/ar1.dcf) | `https://posit.byui.edu/content/be8ec2cd-6209-4b53-a5c6-be11464b86de/` | [chapter_4_lesson_3.qmd](../../chapter_4_lesson_3.qmd#L236) | Used on the current website |
| [ShinyApps/4_Simulation/FittedModels.R](../../ShinyApps/4_Simulation/FittedModels.R) | [fittedmodels.dcf](../../rsconnect/shinyapps.io/4_Simluation/rsconnect/documents/FittedModels.R/posit.byui.edu/sombreroboy/fittedmodels.dcf) | `https://posit.byui.edu/content/6c451298-48a0-43a4-b8e2-52d9adc6baeb/` | [chapter_4_lesson_3.qmd](../../chapter_4_lesson_3.qmd#L346) | Used on the current website |
| [ShinyApps/6_Simulation/chapter_6_lesson_1.R](../../ShinyApps/6_Simulation/chapter_6_lesson_1.R) | [ma3_simulations.dcf](../../rsconnect/shinyapps.io/6_Simulation/rsconnect/documents/chapter_6_lesson_1.R/posit.byui.edu/sombreroboy/ma3_simulations.dcf) | `https://posit.byui.edu/content/edfd752d-58f2-468e-8695-a28a5b4eec6f/` | [chapter_6_lesson_1.qmd](../../chapter_6_lesson_1.qmd#L265) | Used on the current website |
| [ShinyApps/5_Simulation/Lesson_5_2.R](../../ShinyApps/5_Simulation/Lesson_5_2.R) | [harmonic_seasonality_simulations.dcf](../../rsconnect/shinyapps.io/5_Simulation/rsconnect/documents/Lesson_5_2.R/posit.byui.edu/sombreroboy/harmonic_seasonality_simulations.dcf) | `https://posit.byui.edu/content/3fcf5813-76fe-44ee-ab7f-b4a99884c855/` | [archive/harmonics_part_1.qmd](../../archive/harmonics_part_1.qmd#L619) | Used only in archive/legacy content |
| [ShinyApps/6_Simulation/chapter_6_lesson_1.R](../../ShinyApps/6_Simulation/chapter_6_lesson_1.R) | [6_simulation.dcf](../../rsconnect/shinyapps.io/6_Simulation/rsconnect/documents/chapter_6_lesson_1.R/posit.byui.edu/sombreroboy/6_simulation.dcf) | `https://posit.byui.edu/content/241acd67-1e3b-4133-a237-f8ecce184f34/` | none found | No iframe usage found in the repo |

## What the deployment metadata means

The `.dcf` files record deployment state for Posit Connect / shinyapps.io, including the hosted URL, app id, bundle id, and deployment options. They are the bridge between the local app source and the iframe embeds.

That means a lesson can use a Shiny app without referencing the local `.R` file at all. The local file may exist only so the app can be updated and redeployed later.

## Source files with no matching deployed iframe found

I found these Shiny source files in `ShinyApps/` that did not have a matching deployment record or an iframe usage in the repo during this pass:

- [ShinyApps/AutoCorrCoeff.R](../../ShinyApps/AutoCorrCoeff.R)
- [ShinyApps/Chpt1Matching.R](../../ShinyApps/Chpt1Matching.R)
- [ShinyApps/6_Simulation/chapter_6_lesson_1_new/app.R](../../ShinyApps/6_Simulation/chapter_6_lesson_1_new/app.R)
- [ShinyApps/HoltWintersMethod/HoltWinters0.qmd](../../ShinyApps/HoltWintersMethod/HoltWinters0.qmd)
- [ShinyApps/HoltWintersMethod/HoltWinters0.rmarkdown](../../ShinyApps/HoltWintersMethod/HoltWinters0.rmarkdown)
- [ShinyApps/HoltWintersMethod/HoltWinters_add.R](../../ShinyApps/HoltWintersMethod/HoltWinters_add.R)

These may be prototypes, deprecated apps, or app sources that were never deployed from this branch.

## Practical takeaway

The site uses deployed hosted versions rather than sourcing them dynamically from `ShinyApps/`. That is why a search through lesson code may not show the local app filenames even though the app content is clearly used. The current website uses eight deployed apps, one app is only referenced in archived material, and one deployed `6_Simulation` record appears to be orphaned in the repo.