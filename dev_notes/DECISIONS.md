This file is to communicate explicit decisions that may not be otherwise apparent.


## Homework Dependency

Because homeworks will be downloaded as qmd, they should have no dependencies on the full website, being fully self-sustaining. Any exceptions (such as data access) use a link to the github repo for download.


## Package Loading

In a marriage between programming and pedagogy, packages are placed in three places for each lesson page.

1. The file `_common_packages.R` contains the most common packages students frequently use, and it is seen.
2. The file `_backend_functions.R` contains only packages and functions that students never use, and it is not seen.
3. Less frequently used packages are declared only in the lesson they are used right above `_common_packages.R`.

Exceptions to this strict pattern is when it is pedagocially meaningful. 
- *lessons 1.1, 1.2*: the introduction of packages is delayed until the first time code is run by the student.
- *lessons 1.5, 2.1, 2.2*: packages are not likely to be used by the student, so it is loaded silently (hidden with `include: false`).

### Code

`_backend_functions.R` at the top of the page under the YAML

> ```{r}
> #| include: false
> source("_backend_functions.R")
> ```

Beneath the *Learning Journal Exchange*:

> ## Packages
>
> ```{r}
> # Additional packages for that lesson only
> ```
> 
> ```{r}
> #| code-summary: "Common Packages"
> #| file: "_common_packages.R"
> ```

For packages that have conflict potential, students may accidentally loading them after tidyverse, creating issues. For those packages, the following block with package and conflict variation is included below "Common Packages".

> ```{r}
> #| code-summary: "If you have code issues"
> #| code-fold: true
> #| message: false
> #| warning: false
> #| eval: false
> 
> # The ___ package creates conflicts with the ___ function from the tidyverse package
> # This makes sure it defaults to the tidyverse version
> # Alternatively, using dplyr::select() or loading ___ first resolves the issue
> pacman::p_load(conflicted)
> conflict_prefer("select", "dplyr")
> ```

### Code for Homeworks

The format for homework varies because they are downloaded by the student.

```{r}
#| message: false
#| echo: false

# Loading R packages
pacman::p_load(
  tidyverse,  # ggplot, mutate(), cleaning...
  tsibble,    # as_tsibble()
  fable,      # model(...), forecast(), tidy(), glance()...
  feasts,     # ACF(), PACF()
  ggtime,     # autoplot() for tsibbles
  patchwork,  # + and / for ggplots
  rio         # import()
)
```

### Why

To clarify the problem being solved, there are a few key assumptions about the purpose of the course and its website. There are two fundamentals regarding code use:

1. Students use the textbook code by copy/pasting into their local environment
2. Students download homework assignments to render as isolated qmd files
3. Course developers run the code locally, either as a website render or in chunks

This leads to a few lower level assumptions

1. *Visible* code everywhere should be pedagocially clear, highly reliable, and self-standing
2. The above also applies to *invisible* code for homework assignments
3. Website architecture should be simple for maintainers to understand and update
4. Build/render time is ideally efficient, but less important

And finally, architectural decision principles

1. Have only a few common packages in `pacman::p_load()` every lesson; *helps the student see what packages matter*
2. Localize to an invisible global variable `_common_packages.R`; *helps reliability and consistency*
3. Point out the packages that are uncommon or different; *teaches when to use a other packages and reduces "package bloat"*
4. Hide packages used only for backend functions inside `_common_functions.R`; *reduces confusion for the student*

As an added bonus, using the minimum packages per page is *computationally efficient* and creates high reliability through *failure on build*. Having a build fail if the visible packages are insufficient helps the developer know to add missing packages.

#### Last Notes

Maintaining this structure requires awareness and diligence for the devlopers. Maintaining alternate structures would be far more laborious given the listed requirements. To have visible packages unevealuated and an invisible list of all packages removes the failure on build consistency. 

Placing packages in `_backend_functions.R` is not ideal, but seems sufficient with the reasons listed above. It creates an inconsistent instance in just one file that the developers are (hopefully) aware of.

Including packages only needed for `_backend_functions.R` in each file is pedagogically confusing, and placing those packages in `_backend_functions.R` instead of a unified `_packages.R` creates a worse reliability issue where packages loaded into a page are hidden in an obscure file and still get loaded twice if used on the page.