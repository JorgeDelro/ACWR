# Package index

## Main function

Acute and chronic loads and the ACWR for one or several athletes, with
one or more methods.

- [`ACWR()`](https://jorgedelro.github.io/ACWR/reference/ACWR.md) :
  Acute Chronic Workload Ratio

## Calculation methods

One athlete: exponentially weighted moving average (Williams et al.,
2017) and coupled and uncoupled rolling averages (Windt & Gabbett,
2019), on a daily calendar.

- [`EWMA()`](https://jorgedelro.github.io/ACWR/reference/EWMA.md) :
  Exponentially Weighted Moving Average
- [`RAC()`](https://jorgedelro.github.io/ACWR/reference/RAC.md) :
  Rolling Average Coupled
- [`RAU()`](https://jorgedelro.github.io/ACWR/reference/RAU.md) :
  Rolling Average Uncoupled

## Plotting

Training load and ACWR per day, drawn with d3.js.

- [`plot_ACWR()`](https://jorgedelro.github.io/ACWR/reference/plot_ACWR.md)
  : ACWR plots using d3.js

## Footballer Workload Footprint

Cumulative and delta variations of one or several load variables over
windows of calendar days (Matas-Bustos et al., 2025), and their heatmap.
Descriptive features, not a validated injury-prediction tool.

- [`fwf_cumulative()`](https://jorgedelro.github.io/ACWR/reference/fwf_cumulative.md)
  [`fwf_delta()`](https://jorgedelro.github.io/ACWR/reference/fwf_cumulative.md)
  : Footballer Workload Footprint: cumulative and delta variations
- [`plot_fwf()`](https://jorgedelro.github.io/ACWR/reference/plot_fwf.md)
  : Heatmap of the Footballer Workload Footprint

## Shiny app

Interactive app for teaching, built on the functions above.

- [`run_app()`](https://jorgedelro.github.io/ACWR/reference/run_app.md)
  : Run the ACWR Shiny app

## Datasets

Simulated training loads of three athletes.

- [`training_load`](https://jorgedelro.github.io/ACWR/reference/training_load.md)
  : Training load dataframe
- [`training_load_rest`](https://jorgedelro.github.io/ACWR/reference/training_load_rest.md)
  : Training load dataframe with rest days
