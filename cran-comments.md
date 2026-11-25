## Submission

This is a feature release (0.2.0 -> 0.3.0). Results of the existing exported
functions (`ACWR()`, `EWMA()`, `RAC()`, `RAU()`) with their default
arguments are unchanged; a test compares them with outputs saved before the
changes.

New features:

* `fwf_cumulative()` and `fwf_delta()`: cumulative and delta variations of
  the Footballer Workload Footprint (Matas-Bustos et al., 2025,
  <doi:10.1371/journal.pone.0327960>), and `plot_fwf()` to draw them.
* `acute` and `chronic` arguments (window lengths in days, defaults 7 and
  28) in `ACWR()`, `EWMA()`, `RAC()` and `RAU()`.
* `run_app()`: a Shiny app for teaching, installed in `inst/shiny/`, that
  uses only the package functions. Its example runs only in interactive
  sessions.

Dependencies:

* New import: tibble.
* New suggested packages: bslib (>= 0.6.0), chromote, DT, ggplot2, plotly,
  shiny, shinytest2, withr, writexl. They are used by `plot_fwf()`,
  `run_app()` and the tests, and are checked with `requireNamespace()` or
  `skip_if_not_installed()`. The browser-based app test is skipped on CRAN.

## Test environments

* local macOS Sequoia 15.4.1 (aarch64-apple-darwin20), R 4.5.3:
  `devtools::check()` (R CMD check --as-cran --no-manual) on ACWR 0.3.0

<!-- Before submitting: add win-builder (R-devel) results for 0.3.0 and
     re-run the check with the manual. -->

## R CMD check results

On local macOS (R 4.5.3): 0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time
  This note comes from the local check machine (it could not reach the time
  server), not from the package.

## Reverse dependencies

There are currently no reverse dependencies on CRAN
(tools::package_dependencies("ACWR", reverse = TRUE, which = "all")).
