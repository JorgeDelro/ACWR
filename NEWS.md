# ACWR 0.2.0

This is a bug-fix release that **changes numerical results**.

## Results differ from 0.1.0 whenever sessions are not daily

In 0.1.0, `RAC()` and `RAU()` averaged the load per *session* inside each
window, and `EWMA()` decayed once per *session*, ignoring the dates. Rest days
were therefore not counted as zero load, and results were only correct when
there was exactly one session every day. `RAC()` and `RAU()` also failed with
"NA/NaN argument" when a week number was missing.

From 0.2.0, each athlete's sessions are turned into a daily series from the
first to the last date: sessions on the same day are summed and days without a
session count as a load of 0. With exactly one session every day, EWMA results
are unchanged and the RAC and RAU ratios are unchanged from day 28 on (except
for rounding), but RAC/RAU acute and chronic loads are now 7 times larger
(weekly sums instead of daily averages) and the ratios are `NA` during the
first 27 days. Whenever sessions are not daily, all results differ.

For example, with the same weekly loads but 3 sessions per week for three
weeks and 6 in the fourth, the coupled ratio (RAC) at the last session is now
1.45 (it was 0.93), the uncoupled ratio (RAU) 1.70 (was 1.31) and EWMA 1.20
(was 0.93).

## Changes to the calculations

* Acute and chronic loads of `RAC()` and `RAU()` are now **weekly-equivalent
  sums** over calendar days, not averages per session:
  * acute: sum of the daily load over the last 7 days (including today);
  * RAC chronic: sum over the last 28 days / 4 (coupled);
  * RAU chronic: sum over days 8 to 28 before today / 3 (uncoupled).
  This follows the usual definition of acute and chronic workloads (weekly
  load and average weekly load), and it is the only way to count rest days.
* `EWMA()` applies the decay to every calendar day, including rest days with
  zero load, as in Williams et al. (2017) <doi:10.1136/bjsports-2016-096589>.
  Its acute and chronic loads are on the scale of a daily load.
* `RAC()` and `RAU()` return `NA` until 28 days of history exist (acute
  loads, until 7 days). The new argument `partial = TRUE` uses the days
  available instead. `ACWR()` also gains `partial`.
* A missing load (`NA`) makes every window containing its day `NA`, with one
  warning that gives the number of affected days. Because EWMA is recursive,
  every later day is `NA` for EWMA; impute missing loads before calling the
  functions.
* The ratio is `NA` when the chronic load is 0.
* Values are no longer rounded inside the functions.

## Interface

* All functions return one value per input row, in the input order.
  **`ACWR()` now returns the rows in the original order of `db`** (0.1.0
  returned them grouped by athlete). It also no longer fails when no athlete
  has the ID 1.
* `EWMA()` gains `training_dates = NULL`. If it is not given, sessions are
  assumed to be on consecutive days and a message is shown.
* `weeks` (in `ACWR()`, `RAC()` and `RAU()`) and `days` (in `ACWR()`) are
  deprecated: they are kept for compatibility but no longer used in the
  calculation, and are now optional.
* Inputs are validated with clear error messages: dates must be Date or
  POSIXct, loads must be numeric and non-negative, columns must exist in `db`.
  Unsorted dates are sorted internally.
* The internal function `training_blocks()` has been removed.
* `plot_ACWR()` shows one bar per day when there are several sessions on the
  same day, and does not draw the ACWR line on days where it is `NA`.

## Other

* New dataset `training_load_rest`: 3 athletes with rest days, a double
  session day and a week without training.
* Examples now run.
* README: new section on the limitations of the ACWR.
* DESCRIPTION: fixed DOI and spelling of Gabbett.

# ACWR 0.1.0

* First CRAN release.
