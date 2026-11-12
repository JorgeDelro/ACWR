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

* local macOS (Darwin 24.4.0, aarch64), R 4.5.3

## R CMD check results

0 errors | 0 warnings | 2 notes

Both notes come from the local check machine, not from the package:

* checking for future file timestamps ... NOTE
  unable to verify current time
* checking HTML version of manual ... NOTE
  Skipping checking HTML validation: 'tidy' doesn't look like recent enough
  HTML Tidy.

## CRAN check results for 0.1.0

OK on all 13 flavors (as of 2026-11-12).

## Reverse dependencies

There are currently no reverse dependencies on CRAN
(tools::package_dependencies("ACWR", reverse = TRUE, which = "all")).
