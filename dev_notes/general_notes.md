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