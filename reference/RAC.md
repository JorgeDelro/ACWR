# Rolling Average Coupled

Calculates the acute chronic workload ratio with rolling averages, where
the acute week is included in the chronic period (coupled ratio).

## Usage

``` r
RAC(TL, weeks = NULL, training_dates, partial = FALSE)
```

## Arguments

- TL:

  training load of each session (numeric, \>= 0).

- weeks:

  deprecated and ignored. Kept for backwards compatibility; the
  calculation now uses `training_dates` only.

- training_dates:

  date of each session (Date or POSIXct). Sessions do not need to be
  sorted.

- partial:

  if `FALSE` (default), acute values are NA until 7 days of history
  exist, and chronic values and ACWR until 28 days exist. If `TRUE`, the
  days available are used and expressed per 7 days.

## Value

A list with one value per session, in the input order:

- RAC_acute: RAC - acute training load.

- RAC_chronic: RAC - chronic training load.

- RAC_ACWR: RAC - Acute-Chronic Workload Ratio (NA when the chronic load
  is 0).

## Details

Sessions are first turned into a daily series from the first to the last
date: sessions on the same day are summed and days without a session
count as a load of 0. Then, for each day \\t\\:

- Acute load: sum of the daily load over days \\t-6\\ to \\t\\.

- Chronic load: sum of the daily load over days \\t-27\\ to \\t\\,
  divided by 4 (weekly-equivalent).

- ACWR: acute / chronic.

Each session gets the value of its day. Values are not rounded.

A missing load (NA) makes every window that contains its day NA, and a
warning is given.

## Package decisions

The following choices are made by this package; they are not taken from
the cited sources:

- Windows move day by day. The sources define acute and chronic loads by
  weeks (the most recent week, and the average of the last 4 weeks or of
  the 3 preceding weeks). This package interprets them as rolling
  windows of 7, 28 and 21 calendar days that end on each day, so every
  day has its own value.

- With `partial = TRUE`, a window that starts before the first day uses
  only the days available and is rescaled to 7 days (sum x 7 / number of
  days available). The sources do not cover incomplete windows.

- The ratio is NA when the chronic load is 0 (e.g. no training in the
  chronic window): a ratio to zero load is undefined, and returning NA
  avoids infinite values in summaries and plots.

## References

Windt J, Gabbett TJ (2019). Is it all for naught? What does mathematical
coupling mean for acute:chronic workload ratios? British Journal of
Sports Medicine, 53(16), 988-990.
[doi:10.1136/bjsports-2017-098925](https://doi.org/10.1136/bjsports-2017-098925)

## Examples

``` r
data("training_load", package = "ACWR")

# Select the first subject
training_load_1 <- training_load[training_load[["ID"]] == 1, ]

# Calculate ACWR
result_RAC <- RAC(TL = training_load_1$TL,
                  training_dates = training_load_1$Training_Date)
```
