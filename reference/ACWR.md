# Acute Chronic Workload Ratio

Calculates the acute chronic workload ratio for one or several athletes
with one or more methods (see
[`EWMA()`](https://jorgedelro.github.io/ACWR/reference/EWMA.md),
[`RAC()`](https://jorgedelro.github.io/ACWR/reference/RAC.md) and
[`RAU()`](https://jorgedelro.github.io/ACWR/reference/RAU.md)).

## Usage

``` r
ACWR(
  db,
  ID,
  TL,
  weeks = NULL,
  days = NULL,
  training_dates,
  ACWR_method = c("EWMA", "RAC", "RAU"),
  partial = FALSE
)
```

## Arguments

- db:

  a data frame with one row per session.

- ID:

  name of the column with the ID of the subjects.

- TL:

  name of the column with the training load (numeric, \>= 0).

- weeks:

  deprecated and ignored in the calculation. If given, the column is
  kept in the output.

- days:

  deprecated and ignored in the calculation. If given, the column is
  kept in the output (e.g. for
  [`plot_ACWR()`](https://jorgedelro.github.io/ACWR/reference/plot_ACWR.md)).

- training_dates:

  name of the column with the training dates (Date or POSIXct). Rows do
  not need to be sorted.

- ACWR_method:

  method(s) to calculate ACWR: any of "EWMA", "RAC" and "RAU".

- partial:

  passed to
  [`RAC()`](https://jorgedelro.github.io/ACWR/reference/RAC.md) and
  [`RAU()`](https://jorgedelro.github.io/ACWR/reference/RAU.md). If
  `FALSE` (default), values are NA until enough history exists; if
  `TRUE`, the days available are used.

## Value

a data frame with the ID, TL, weeks, days and training dates columns,
followed by the acute & chronic training load and ACWR of the selected
method/s. Rows are in the same order as in `db`.

## Details

Each athlete's sessions are turned into a daily series from their first
to their last date: sessions on the same day are summed and days without
a session count as a load of 0.

A missing load (NA) makes every window that contains its day NA, and a
single warning is given for the whole data frame.

## Package decisions

The following choices are made by this package; they are not taken from
the cited sources:

- RAC and RAU windows move day by day. The sources define acute and
  chronic loads by weeks (the most recent week, and the average of the
  last 4 weeks or of the 3 preceding weeks). This package interprets
  them as rolling windows of 7, 28 and 21 calendar days that end on each
  day, so every day has its own value.

- With `partial = TRUE`, a window that starts before the first day uses
  only the days available and is rescaled to 7 days (sum x 7 / number of
  days available). The sources do not cover incomplete windows.

- For all methods, the ratio is NA when the chronic load is 0 (e.g. no
  training in the chronic window): a ratio to zero load is undefined,
  and returning NA avoids infinite values in summaries and plots.

EWMA is updated every calendar day, as defined by Williams et al.
(2017); this is not a package interpretation.

## Examples

``` r
data("training_load", package = "ACWR")

# Calculate ACWR
result_ACWR <- ACWR(db = training_load,
                    ID = "ID",
                    TL = "TL",
                    training_dates = "Training_Date",
                    ACWR_method = c("EWMA", "RAC", "RAU"))

# Athletes with rest days
data("training_load_rest", package = "ACWR")
result_rest <- ACWR(db = training_load_rest,
                    ID = "ID",
                    TL = "TL",
                    training_dates = "Training_Date",
                    ACWR_method = c("EWMA", "RAC", "RAU"))
```
