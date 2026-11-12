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

## Datasets

Simulated training loads of three athletes.

- [`training_load`](https://jorgedelro.github.io/ACWR/reference/training_load.md)
  : Training load dataframe
- [`training_load_rest`](https://jorgedelro.github.io/ACWR/reference/training_load_rest.md)
  : Training load dataframe with rest days
