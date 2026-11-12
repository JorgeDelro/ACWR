# Training load dataframe with rest days

A dataframe with the training load of 3 subjects over 8 weeks, with rest
days. Subject 1 trains 4 days per week; subject 2 trains 5 days per week
with a double session on Wednesdays and does not train in week 5;
subject 3 trains 3 days per week and 6 days in the last week. Created
with simulated data (see `data-raw/training_load_rest.R` in the source
code).

## Usage

``` r
data("training_load_rest", package = "ACWR")
```

## Format

A data frame with 101 rows (one per session) and 5 variables:

- ID:

  ID of the subjects

- Week:

  training week

- Day:

  calendar day since the start (1 = Monday 3 January 2022)

- TL:

  training load (arbitrary units)

- Training_Date:

  training date (Date)
