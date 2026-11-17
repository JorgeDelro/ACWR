## Submission

This is a bug-fix release (0.1.0 -> 0.2.0) that changes numerical results.

In 0.1.0, the rolling average methods (RAC, RAU) averaged the training load
per session, and EWMA decayed per session, ignoring the dates. Results were
only correct when athletes trained exactly once every day. 0.2.0 computes all
methods on a daily calendar (rest days count as zero load). Results differ
from 0.1.0 whenever sessions are not daily; this is documented in NEWS.md.
Exported functions and their arguments are kept; deprecated arguments are
still accepted.

## Test environments

* local macOS Sequoia 15.4.1 (aarch64-apple-darwin20), R 4.5.3:
  R CMD check --as-cran on ACWR_0.2.0.tar.gz built with devtools::build()
* win-builder (Windows Server 2022, x86_64), R-devel (2026-11-16 r90914)

## R CMD check results

On local macOS (R 4.5.3): 0 errors | 0 warnings | 1 note

* checking HTML version of manual ... NOTE
  Skipping checking HTML validation: 'tidy' doesn't look like recent enough
  HTML Tidy.
  This note comes from the local check machine (its HTML Tidy is too old),
  not from the package.

On win-builder (R-devel): 0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Possibly misspelled words in DESCRIPTION: Gabbett (16:34)
  "Gabbett" is the surname of a cited author (Windt & Gabbett, 2019) and is
  spelled correctly.

## CRAN check results for 0.1.0

OK on all 13 flavors (as of 2026-11-17).

## Reverse dependencies

There are currently no reverse dependencies on CRAN
(tools::package_dependencies("ACWR", reverse = TRUE, which = "all")).
