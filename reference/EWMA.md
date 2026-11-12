# Exponentially Weighted Moving Average

Calculates the acute chronic workload ratio with exponentially weighted
moving averages (Williams et al., 2017).

## Usage

``` r
EWMA(TL, training_dates = NULL)
```

## Arguments

- TL:

  training load of each session (numeric, \>= 0).

- training_dates:

  date of each session (Date or POSIXct). Sessions do not need to be
  sorted. If `NULL` (default), sessions are assumed to be on consecutive
  days, in the order given, and a message is shown.

## Value

A list with one value per session, in the input order:

- EWMA_acute: EWMA - acute training load.

- EWMA_chronic: EWMA - chronic training load.

- EWMA_ACWR: EWMA - Acute-Chronic Workload Ratio (NA when the chronic
  load is 0).

## Details

Sessions are first turned into a daily series from the first to the last
date: sessions on the same day are summed and days without a session
count as a load of 0. Then, for every calendar day \\t\\ with daily load
\\w_t\\: \$\$E_t = w_t \lambda + (1 - \lambda) E\_{t-1}, \quad \lambda =
2 / (N + 1)\$\$ with \\N = 7\\ (acute) and \\N = 28\\ (chronic), and
\\E_1 = w_1\\. Acute and chronic loads are on the scale of a daily load.
Each session gets the value of its day. Values are not rounded.

EWMA is recursive: a missing load (NA) makes that day and every later
day NA. A warning is given; impute missing loads before calling the
function.

## Package decisions

The following choices are made by this package; they are not taken from
the cited sources:

- The ratio is NA when the chronic load is 0 (e.g. no training in the
  chronic window): a ratio to zero load is undefined, and returning NA
  avoids infinite values in summaries and plots.

Updating the EWMA every calendar day, including rest days, follows the
definition of Williams et al. (2017) and is not a package
interpretation.

## References

Williams S, West S, Cross MJ, Stokes KA (2017). Better way to determine
the acute:chronic workload ratio? British Journal of Sports Medicine,
51(3), 209-210.
[doi:10.1136/bjsports-2016-096589](https://doi.org/10.1136/bjsports-2016-096589)

## Examples

``` r
data("training_load", package = "ACWR")

# Select the first subject
training_load_1 <- training_load[training_load[["ID"]] == 1, ]

# Calculate ACWR
result_EWMA <- EWMA(TL = training_load_1$TL,
                    training_dates = training_load_1$Training_Date)
```
