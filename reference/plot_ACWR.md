# ACWR plots using d3.js

ACWR plots using d3.js

## Usage

``` r
plot_ACWR(
  db,
  TL,
  ACWR,
  day,
  ID = NULL,
  colour = NULL,
  xLabel = NULL,
  y0Label = NULL,
  y1Label = NULL,
  plotTitle = NULL
)
```

## Arguments

- db:

  a data frame

- TL:

  training load

- ACWR:

  Acute Chronic Workload Ratio

- day:

  training days: day numbers (whole numbers) or dates (Date or POSIXct).
  The x-axis is a continuous calendar from the first to the last day.

- ID:

  ID of the subjects

- colour:

  colour of the bars. By default "#87CEEB" (skyblue)

- xLabel:

  x-axis label. By default "Days"

- y0Label:

  left y-axis label. By default `"Load [AU]"`

- y1Label:

  right y-axis label. By default "Acute:chronic workload ratio"

- plotTitle:

  Title of the plot. By default "ACWR"

## Value

This function returns a d3.js object for a single subject. For several
subjects it returns a list of d3.js objects. The x-axis has one slot per
calendar day between the first and the last day, labelled every 7 days;
days without a session have a bar of height 0. Sessions on the same day
are shown as one bar (summed load). The ACWR line joins the sessions
that have a value, across rest days, and is not drawn where the ACWR is
NA.

## Examples

``` r
data("training_load", package = "ACWR")

# Calculate ACWR
result_ACWR <- ACWR(db = training_load,
                    ID = "ID",
                    TL = "TL",
                    days = "Day",
                    training_dates = "Training_Date",
                    ACWR_method = c("EWMA", "RAC", "RAU"))

# Plot for 1 subject
# Select the first subject
result_ACWR_1 <- result_ACWR[result_ACWR[["ID"]] == 1, ]

# plot ACWR (e.g. EWMA)
ACWR_plot_1 <- plot_ACWR(db = result_ACWR_1,
                         TL = "TL",
                         ACWR = "EWMA_ACWR",
                         day = "Day")

# Plot for several subjects
# plot ACWR (e.g. RAC)
ACWR_plot <- plot_ACWR(db = result_ACWR,
                       TL = "TL",
                       ACWR = "RAC_ACWR",
                       day = "Day",
                       ID = "ID")
```
