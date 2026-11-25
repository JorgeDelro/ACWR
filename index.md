# ACWR

## Overview

The **ACWR** package has been designed to calculate the acute chronic
workload ratio (Gabbett, 2016) using three different methods:
exponentially weighted moving average (EWMA; Williams et al., 2017;
Murray et al., 2017), rolling average coupled (RAC) and rolling average
uncoupled (RAU) (Windt & Gabbett, 2019).

Sessions are placed on a daily calendar: sessions on the same day are
summed and days without a session count as a load of 0. Acute and
chronic loads are then computed over calendar days, not over sessions:

- Acute load: sum of the daily load over the last 7 days.
- RAC chronic load: sum over the last 28 days / 4 (coupled; Windt &
  Gabbett, 2019).
- RAU chronic load: sum over days 8 to 28 before today / 3 (uncoupled;
  Windt & Gabbett, 2019).
- EWMA: exponentially weighted moving averages with time-decay constants
  of 7 and 28 days, updated every calendar day (Williams et al., 2017).

Version 0.2.0 fixed a bug: results of earlier versions are only correct
when there was exactly one session every day. See `NEWS.md`.

## Example

This is a basic example which shows you how to use the ACWR package:

``` r

install.packages("ACWR")
library(ACWR)
```

First, we have to load the data stored in the package. `training_load`
has one session every day; `training_load_rest` has rest days, a day
with two sessions and a week without training.

``` r

data("training_load", package = "ACWR")
data("training_load_rest", package = "ACWR")
```

Then, we can calculate the ACWR:

``` r

result_ACWR <- ACWR(db = training_load_rest,
                    ID = "ID",
                    TL = "TL",
                    days = "Day",
                    training_dates = "Training_Date",
                    ACWR_method = c("EWMA", "RAC", "RAU"))
```

RAC and RAU are `NA` until 28 days of history exist. Use
`partial = TRUE` to use the days available instead. The windows can be
changed with `acute` and `chronic` (in days; 7 and 28 by default),
e.g. to explore how the choice changes the ratio.

Additionally, individual plots can be obtained:

``` r

ACWR_plot <- plot_ACWR(db = result_ACWR,
                       TL = "TL",
                       ACWR = "RAC_ACWR",
                       day = "Day",
                       ID = "ID")
```

Functions for each individual method have been implemented too:

``` r

# Select the first subject
training_load_1 <- training_load_rest[training_load_rest[["ID"]] == 1, ]

# EWMA
result_EWMA <- EWMA(TL = training_load_1$TL,
                    training_dates = training_load_1$Training_Date)

# RAC
result_RAC <- RAC(TL = training_load_1$TL,
                  training_dates = training_load_1$Training_Date)

# RAU
result_RAU <- RAU(TL = training_load_1$TL,
                  training_dates = training_load_1$Training_Date)
```

## Footballer Workload Footprint

[`fwf_cumulative()`](https://jorgedelro.github.io/ACWR/reference/fwf_cumulative.md)
and
[`fwf_delta()`](https://jorgedelro.github.io/ACWR/reference/fwf_cumulative.md)
compute the cumulative and delta variations of the Footballer Workload
Footprint (Matas-Bustos et al., 2025) for one or several load variables,
over windows of 1 to 28 calendar days, and
[`plot_fwf()`](https://jorgedelro.github.io/ACWR/reference/plot_fwf.md)
draws them as a heatmap. They are descriptive features: the article
defines the windows in events (sessions), whereas this package uses
calendar days, and its results come from a single team over one season.

``` r

fwf <- fwf_cumulative(db = training_load_rest,
                      ID = "ID",
                      vars = "TL",
                      training_dates = "Training_Date")
plot_fwf(fwf, ID = 1, date = "2022-02-26")
```

## Shiny app

The package includes an interactive app, designed for teaching, to
upload or choose a dataset, compute the ACWR with the three methods, see
each step of the calculation, and explore the Footballer Workload
Footprint:

``` r

ACWR::run_app()
```

All values shown in the app are computed with the functions of this
package. The app needs a few extra packages (shiny, bslib, ggplot2,
plotly, DT and writexl);
[`run_app()`](https://jorgedelro.github.io/ACWR/reference/run_app.md)
tells you which ones are missing and how to install them.

## Limitations of the ACWR

The package calculates the ACWR as it is described in the literature,
but the ratio has well-documented limitations that should be considered
before using it to make decisions:

- **Mathematical coupling.** In the coupled ratio, the acute week is
  also part of the chronic load, which creates a spurious correlation
  between the two and reduces the between-athlete variability of the
  chronic load (Lolli et al., 2019); the coupled ratio can never exceed
  4 (Windt & Gabbett, 2019). The uncoupled ratio removes the overlap,
  but not the other problems below.
- **Ratio artefacts.** The ACWR behaves as a rescaling of the acute
  load: similar associations with injury are obtained when the acute
  load is divided by fixed or random “chronic” loads, and the ratio adds
  no predictive value over the acute load alone (Impellizzeri et al.,
  2021). A ratio may not always be an appropriate way to represent
  changes in activity, and EWMA values depend on the initial load in the
  first weeks (Wang et al., 2020).
- **No validated thresholds.** Recommended ACWR thresholds (e.g. a
  “sweet spot” of 0.8-1.3) are affected by these limitations and by
  problems such as discretization and sparse data, which undermine their
  validity as cut-offs for injury risk (Wang et al., 2020; Impellizzeri
  et al., 2021).
- **No benefit in a randomised trial.** In a cluster randomised trial of
  34 elite youth football teams (18 intervention, 16 control) over a
  10-month season, planning training with the ACWR did not reduce health
  problems (Dalen-Lorentsen et al., 2021). The result refers to that
  specific intervention in youth footballers.

## References

- Dalen-Lorentsen T, Bjørneboe J, Clarsen B, Vagle M, Fagerland MW,
  Andersen TE (2021). Does load management using the acute:chronic
  workload ratio prevent health problems? A cluster randomised trial of
  482 elite youth footballers of both sexes. *British Journal of Sports
  Medicine*, 55(2), 108-114.
  <https://doi.org/10.1136/bjsports-2020-103003>
- Gabbett TJ (2016). The training-injury prevention paradox: should
  athletes be training smarter and harder? *British Journal of Sports
  Medicine*, 50(5), 273-280.
  <https://doi.org/10.1136/bjsports-2015-095788>
- Impellizzeri FM, Woodcock S, Coutts AJ, et al. (2021). What role do
  chronic workloads play in the acute to chronic workload ratio? Time to
  dismiss ACWR and its underlying theory. *Sports Medicine*, 51(3),
  581-592. <https://doi.org/10.1007/s40279-020-01378-6>
- Lolli L, Batterham AM, Hawkins R, et al. (2019). Mathematical coupling
  causes spurious correlation within the conventional acute-to-chronic
  workload ratio calculations. *British Journal of Sports Medicine*,
  53(15), 921-922. <https://doi.org/10.1136/bjsports-2017-098110>
- Matas-Bustos JB, Mora-García AM, de Hoyo Lora M, Nieto-Alarcón A,
  Gonzalez-Fernández FT (2025). Advanced feature engineering in
  Acute:Chronic Workload Ratio (ACWR) calculation for injury forecasting
  in elite soccer. *PLoS One*, 20(7), e0327960.
  <https://doi.org/10.1371/journal.pone.0327960>
- Murray NB, Gabbett TJ, Townshend AD, Blanch P (2017). Calculating
  acute:chronic workload ratios using exponentially weighted moving
  averages provides a more sensitive indicator of injury likelihood than
  rolling averages. *British Journal of Sports Medicine*, 51(9),
  749-754. <https://doi.org/10.1136/bjsports-2016-097152>
- Wang C, Vargas JT, Stokes T, et al. (2020). Analyzing activity and
  injury: lessons learned from the acute:chronic workload ratio. *Sports
  Medicine*, 50(7), 1243-1254.
  <https://doi.org/10.1007/s40279-020-01280-1>
- Williams S, West S, Cross MJ, Stokes KA (2017). Better way to
  determine the acute:chronic workload ratio? *British Journal of Sports
  Medicine*, 51(3), 209-210.
  <https://doi.org/10.1136/bjsports-2016-096589>
- Windt J, Gabbett TJ (2019). Is it all for naught? What does
  mathematical coupling mean for acute:chronic workload ratios? *British
  Journal of Sports Medicine*, 53(16), 988-990.
  <https://doi.org/10.1136/bjsports-2017-098925>
