# Run the ACWR Shiny app

Launches an interactive Shiny app (in English, for teaching) to compute
and plot the ACWR and the Footballer Workload Footprint of an athlete,
from an uploaded file or from preloaded datasets.

## Usage

``` r
run_app(...)
```

## Arguments

- ...:

  arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), e.g.
  `launch.browser` or `port`.

## Value

called for its side effect (it starts the app); does not return until
the app is closed.

## Details

All the values shown by the app are computed with the functions of this
package ([`RAC()`](https://jorgedelro.github.io/ACWR/reference/RAC.md),
[`RAU()`](https://jorgedelro.github.io/ACWR/reference/RAU.md),
[`EWMA()`](https://jorgedelro.github.io/ACWR/reference/EWMA.md),
[`fwf_cumulative()`](https://jorgedelro.github.io/ACWR/reference/fwf_cumulative.md)
and
[`fwf_delta()`](https://jorgedelro.github.io/ACWR/reference/fwf_cumulative.md)),
on the same daily calendar.

Uploaded CSV files need a `date` (or numeric `day`) column and a `load`
column, and may have an `athlete_id` column and extra numeric variables
(used for the FWF). The Spanish column names `fecha`, `dia`, `carga` and
`atleta` are accepted as aliases.

         The app needs the packages shiny, bslib, ggplot2, plotly, DT
         and writexl. They are not installed with ACWR;
         `run_app()` says which ones are missing.

## Examples

``` r
if (interactive()) {
  run_app()
}
```
