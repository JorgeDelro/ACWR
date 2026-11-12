# Computing the ACWR on a daily calendar

``` r

library(ACWR)
```

Since version 0.2.0, every method in ACWR works on a **daily calendar**.
Each athlete’s sessions are placed on a series of days from the first to
the last date: sessions on the same day are summed, and days without a
session count as a load of 0. This vignette shows what that means in
practice.

## The data

`training_load_rest` holds the simulated training load of three athletes
over eight weeks, with rest days:

``` r

data("training_load_rest", package = "ACWR")
head(training_load_rest)
#>   ID Week Day  TL Training_Date
#> 1  1    1   1 439    2022-01-03
#> 2  1    1   2 419    2022-01-04
#> 3  1    1   4 396    2022-01-06
#> 4  1    1   6 391    2022-01-08
#> 5  1    2   8 446    2022-01-10
#> 6  1    2   9 452    2022-01-11

# Sessions per week
table(ID = training_load_rest$ID, Week = training_load_rest$Week)
#>    Week
#> ID  1 2 3 4 5 6 7 8
#>   1 4 4 4 4 4 4 4 4
#>   2 6 6 6 6 0 6 6 6
#>   3 3 3 3 3 3 3 3 6
```

Athlete 1 trains 4 days a week. Athlete 2 trains 5 days a week with a
double session on Wednesdays (6 sessions) and does not train in week 5.
Athlete 3 trains 3 days a week and 6 days in the last week.

## The three methods

For each day *t*, with *w* the daily load:

- **RAC** (coupled): acute = sum of *w* over days *t*-6 to *t*; chronic
  = sum over days *t*-27 to *t*, divided by 4.
- **RAU** (uncoupled): same acute load; chronic = sum over days *t*-27
  to *t*-7, divided by 3.
- **EWMA**: *E_(t)* = *w_(t)* λ + (1 - λ) *E_(t-1)*, with λ = 2 / (*N* +
  1), *N* = 7 (acute) and 28 (chronic), updated every calendar day
  including rest days (Williams et al., 2017).

RAC and RAU acute and chronic loads are weekly-equivalent sums, so they
read as “load per week”.
[`ACWR()`](https://jorgedelro.github.io/ACWR/reference/ACWR.md) computes
all three for every athlete and returns one row per session, in the
original order:

``` r

res <- ACWR(db = training_load_rest,
            ID = "ID",
            TL = "TL",
            days = "Day",
            training_dates = "Training_Date",
            ACWR_method = c("EWMA", "RAC", "RAU"))

# Last session of each athlete
last <- res[!duplicated(res$ID, fromLast = TRUE),
            c("ID", "Training_Date", "EWMA_ACWR", "RAC_ACWR", "RAU_ACWR")]
format(last, digits = 3)
#>     ID Training_Date EWMA_ACWR RAC_ACWR RAU_ACWR
#> 32   1    2022-02-26      1.01    0.939     0.92
#> 74   2    2022-02-26      1.12    1.259     1.38
#> 101  3    2022-02-26      1.53    1.698     2.21
```

Athlete 3, who doubled the number of sessions in the last week, shows
the highest ratios.

## What changes versus 0.1.0

Version 0.1.0 averaged the load per *session* inside each window and
decayed EWMA once per *session*, ignoring the dates. Results were only
correct when there was exactly one session every day.

Take an athlete who trains 3 days a week (500 AU per session) for four
weeks and then 6 days in week 5. The weekly load doubles, from 1500 to
3000 AU, but the load per session stays at 500 AU:

``` r

day <- c(0, 2, 4, 7, 9, 11, 14, 16, 18, 21, 23, 25, 28:33)
dates <- as.Date("2024-01-01") + day
load <- rep(500, length(day))

rac <- RAC(TL = load, training_dates = dates)
rau <- RAU(TL = load, training_dates = dates)
ewma <- EWMA(TL = load, training_dates = dates)

# Ignoring the dates reproduces the per-session EWMA of 0.1.0
ewma_sessions <- suppressMessages(EWMA(TL = load))

n <- length(load)
data.frame(
  method = c("RAC", "RAU", "EWMA"),
  version_0.1.0 = c(1.00, 1.86, round(ewma_sessions$EWMA_ACWR[n], 2)),
  version_0.2.0 = round(c(rac$RAC_ACWR[n], rau$RAU_ACWR[n],
                          ewma$EWMA_ACWR[n]), 2)
)
#>   method version_0.1.0 version_0.2.0
#> 1    RAC          1.00          1.60
#> 2    RAU          1.86          2.00
#> 3   EWMA          1.00          1.33
```

The RAC and RAU values of 0.1.0 are quoted from `NEWS.md`, as that code
is no longer in the package. With per-session averages, doubling the
number of sessions is invisible: the coupled ratio stays at 1. On the
daily calendar the acute week is 3000 AU and the coupled chronic load
1875 AU per week, a ratio of 1.6.

## Warm-up period and `partial = TRUE`

RAC and RAU need 28 days of history, so by default they are `NA` until
then:

``` r

a1 <- training_load_rest[training_load_rest$ID == 1, ]
rac_default <- RAC(TL = a1$TL, training_dates = a1$Training_Date)
rac_partial <- RAC(TL = a1$TL, training_dates = a1$Training_Date,
                   partial = TRUE)

data.frame(day = a1$Day,
           default = round(rac_default$RAC_ACWR, 2),
           partial = round(rac_partial$RAC_ACWR, 2))[c(1:3, 15:18), ]
#>    day default partial
#> 1    1      NA    1.00
#> 2    2      NA    1.00
#> 3    4      NA    1.00
#> 15  25      NA    0.92
#> 16  27      NA    0.92
#> 17  29    0.92    0.92
#> 18  30    0.92    0.92
```

With `partial = TRUE`, the days available are used and rescaled to 7
days (sum x 7 / number of days). This is a choice of the package, not of
the cited sources, and early values rest on very few days: interpret
them with care.

## Limitations of the ACWR

The package calculates the ACWR as it is described in the literature,
but the ratio has well-documented limitations:

- **Mathematical coupling.** In the coupled ratio, the acute week is
  also part of the chronic load, which creates a spurious correlation
  between the two and reduces the between-athlete variability of the
  chronic load (Lolli et al., 2019); the coupled ratio can never exceed
  4 (Windt & Gabbett, 2019).
- **Ratio artefacts.** The ACWR behaves as a rescaling of the acute
  load, and adds no predictive value over the acute load alone
  (Impellizzeri et al., 2021). EWMA values depend on the initial load in
  the first weeks (Wang et al., 2020).
- **No validated thresholds.** Recommended thresholds (e.g. a “sweet
  spot” of 0.8-1.3) are affected by these limitations and should not be
  used as cut-offs for injury risk (Wang et al., 2020; Impellizzeri et
  al., 2021).

## References

- Impellizzeri FM, Woodcock S, Coutts AJ, et al. (2021). What role do
  chronic workloads play in the acute to chronic workload ratio? Time to
  dismiss ACWR and its underlying theory. *Sports Medicine*, 51(3),
  581-592. <doi:10.1007/s40279-020-01378-6>
- Lolli L, Batterham AM, Hawkins R, et al. (2019). Mathematical coupling
  causes spurious correlation within the conventional acute-to-chronic
  workload ratio calculations. *British Journal of Sports Medicine*,
  53(15), 921-922. <doi:10.1136/bjsports-2017-098110>
- Wang C, Vargas JT, Stokes T, et al. (2020). Analyzing activity and
  injury: lessons learned from the acute:chronic workload ratio. *Sports
  Medicine*, 50(7), 1243-1254. <doi:10.1007/s40279-020-01280-1>
- Williams S, West S, Cross MJ, Stokes KA (2017). Better way to
  determine the acute:chronic workload ratio? *British Journal of Sports
  Medicine*, 51(3), 209-210. <doi:10.1136/bjsports-2016-096589>
- Windt J, Gabbett TJ (2019). Is it all for naught? What does
  mathematical coupling mean for acute:chronic workload ratios? *British
  Journal of Sports Medicine*, 53(16), 988-990.
  <doi:10.1136/bjsports-2017-098925>
