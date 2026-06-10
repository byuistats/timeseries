## Open Questions

- What is Visuals_Plotly_Copy?
- Sample code, common functions, and lesson templates are scattered. Can they be consolidated?

## TODO

[ ] Un-goof 2.1, 4.2 (only slight goofs)
[x] Localize package requirements with `_all_packages.R` and `_common_packages.R`
[ ] Consider pre-running `_all_packages.R` for installation and loading `_common_packages.R` for each page, with additional packages listed below (much clearer for the student)
    [ ] Consequently, add `package::function` for any packages that are loaded only for a single function (this will reduce conflicts if only one function is loaded rather than the whole package)
[ ] Update README.md to have everything Brother Johnson needs to manage the repo

## Style Guide Indecisions

[ ] `ARIMA(.resid + pdq(...))` or `ARIMA(formula + pdq(...))`

## Snag - Plot Unit Circles

Turns out, this takes more work than I thought
1. `gg_arma` should support `AR` but does not
    - And fitting `ARIMA` to something previously `AR` complicates things (perhaps more so than not using the plot)
2. I don't understand what it is checking to a T
3. There is actually a lot of other information that could be included that would help the student
    - See [conversation](https://gemini.google.com/share/b16fe4b80df4)
    - Invertibility/Stationarity relationship
4. It would be great to add polyroot calculation and plot to the Shiny simulation (lot's of work publishing)
5. Checking for stationarity is trivial because ARIMA/AR always fits stationary and invertible processes
    - See [conversation](https://gemini.google.com/share/1c5683b89754)

## Cleanup

Largely following the files in the research folder for direction on what to clean.

Folders unexamined for obsolete files:
- outcomes
- images
- resources
- rsconnect
- sample_code_chunks
- ShinyApps
- Visuals_Plotly_Copy