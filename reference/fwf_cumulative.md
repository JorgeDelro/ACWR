# Footballer Workload Footprint: cumulative and delta variations

Descriptive features of the Footballer Workload Footprint (FWF) of
Matas-Bustos et al. (2025), computed on a daily calendar for one or
several athletes and one or several load variables.

## Usage

``` r
fwf_cumulative(db, ID, vars, training_dates, windows = 1:28)

fwf_delta(db, ID, vars, training_dates, windows = 1:28)
```

## Arguments

- db:

  a data frame with one row per session.

- ID:

  name of the column with the ID of the subjects.

- vars:

  name(s) of the numeric column(s) with the load variables (e.g. total
  distance, high-speed running).

- training_dates:

  name of the column with the training dates (Date or POSIXct). Rows do
  not need to be sorted.

- windows:

  window sizes \\\tau\\, in calendar days (positive integers). Default
  `1:28`.

## Value

a tibble in long format with one row per athlete, calendar day (rest
days included), variable and window, and the columns `ID`, `date`,
`variable`, `window` and `value`.

## Details

Each athlete's sessions are first turned into a daily series per
variable, from their first to their last date: sessions on the same day
are summed and days without a session count as 0. Then, for each
calendar day \\t\\ and each window \\\tau\\:

- `fwf_cumulative()` (cumulative variations, Eqs 9-10 of the article):
  the sum of the daily values over days \\t - \tau + 1\\ to \\t\\,
  \\x(t) + x(t-1) + \dots + x(t-\tau+1)\\. NA until \\\tau\\ days (day
  \\t\\ included) exist.

- `fwf_delta()` (temporary variations, Eqs 17-18 of the article): the
  change of that sum from the previous day, which simplifies to \\x(t) -
  x(t - \tau)\\. NA until day \\t - \tau\\ exists, i.e. until \\\tau\\
  days of history before day \\t\\ exist.

Values are not rounded. With `windows = 7` and `windows = 28`,
`fwf_cumulative()` gives the acute and (x 4) chronic loads of
[`RAC()`](https://jorgedelro.github.io/ACWR/reference/RAC.md).

A missing value (NA) makes every window that contains its day NA, and a
warning is given.

## Package decisions

The following choices are made by this package; they are not taken from
the cited source:

- Windows are calendar days. The article defines \\\tau\\ in *events*
  (training sessions and matches): its \\x(e\_{j-1})\\ is the previous
  session, wherever it falls. Here \\x(t-1)\\ is the previous calendar
  day, and a day without a session counts as 0, as in
  [`RAC()`](https://jorgedelro.github.io/ACWR/reference/RAC.md),
  [`RAU()`](https://jorgedelro.github.io/ACWR/reference/RAU.md) and
  [`EWMA()`](https://jorgedelro.github.io/ACWR/reference/EWMA.md). With
  rest days, both definitions give different values; for `fwf_delta()`,
  \\x(t - \tau)\\ is often a rest day, so the value is then just
  \\x(t)\\.

- There are no partial windows: values are NA until enough history
  exists (see above).

## Interpretation

These functions are descriptive feature engineering. They summarise the
recent load history in many windows at once; they are not a validated
injury-prediction tool. The article tested them in a single team over
one season, and its results should not be generalised to other athletes.

## References

Matas-Bustos JB, Mora-Garcia AM, de Hoyo Lora M, Nieto-Alarcon A,
Gonzalez-Fernandez FT (2025). Advanced feature engineering in
Acute:Chronic Workload Ratio (ACWR) calculation for injury forecasting
in elite soccer. PLoS One, 20(7), e0327960.
[doi:10.1371/journal.pone.0327960](https://doi.org/10.1371/journal.pone.0327960)

## See also

[`plot_fwf()`](https://jorgedelro.github.io/ACWR/reference/plot_fwf.md)
to draw one athlete and one day as a heatmap.

## Examples

``` r
data("training_load", package = "ACWR")

# Cumulative variations, windows of 1 to 28 days
fwf_sum <- fwf_cumulative(db = training_load,
                          ID = "ID",
                          vars = "TL",
                          training_dates = "Training_Date")

# Delta variations for a few windows
fwf_dif <- fwf_delta(db = training_load,
                     ID = "ID",
                     vars = "TL",
                     training_dates = "Training_Date",
                     windows = c(1, 7, 14, 28))

# windows = 7 compares each day with the same weekday of the previous
# week: Monday 8 January 2024 vs Monday 1 January 2024
two_weeks <- data.frame(ID = 1,
                        TL = c(300, 200, 450),
                        Training_Date = as.Date(c("2024-01-01",
                                                  "2024-01-03",
                                                  "2024-01-08")))
d7 <- fwf_delta(two_weeks, "ID", "TL", "Training_Date", windows = 7)
d7[d7$date == as.Date("2024-01-08"), ] # 450 - 300 = 150
#> # A tibble: 1 × 5
#>      ID date       variable window value
#>   <dbl> <date>     <chr>     <int> <dbl>
#> 1     1 2024-01-08 TL            7   150
```
