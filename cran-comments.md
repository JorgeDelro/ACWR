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

* local macOS Sequoia 15.4.1 (aarch64-apple-darwin20), R 4.5.3, with
  devtools::check(args = "--as-cran", remote = TRUE, incoming = TRUE)
  (R CMD check --as-cran --no-manual)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

This note comes from the local check machine, not from the package.
"checking CRAN incoming feasibility" was OK, and all URLs in the package
were checked with urlchecker::url_check().

## CRAN check results for 0.1.0

OK on all 13 flavors (as of 2026-11-12).

## Reverse dependencies

There are currently no reverse dependencies on CRAN
(tools::package_dependencies("ACWR", reverse = TRUE, which = "all")).
