
# ACWR

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

The **ACWR** package have been designed to calculate the the acute
chronic workload ratio using three different methods: exponentially
weighted moving average (EWMA), rolling average coupled (RAC) and
rolling averaged uncoupled (RAU).

## Resources

  - [Application of ACWR](https://bjsm.bmj.com/content/50/5/273)
    (European Journal of Sport Science)
  - [Rolling average method](https://bjsm.bmj.com/content/53/16/988)
    (British Journal of Sports Medicine)
  - [Exponentially weighted moving averages
    method](https://bjsm.bmj.com/content/51/9/749.long) (British Journal
    of Sports Medicine)

## Example

This is a basic example which shows you how to use the ACWR package:

``` r
library(devtools)
install_github("JorgeDelro/ACWR")
library(ACWR)
```

First, we have to load the data stored in the package

``` r
data("training_load", package = "ACWR")
# Convert to data.frame
training_load <- data.frame(training_load)
```

Then, we can calculate the ACWR:

``` r
result_ACWR <- ACWR(db = training_load,
                  ID = "ID",
                  TL = "TL",
                  weeks = "Week",
                  training_dates = "Training_Date",
                  ACWR_method = c("EWMA", "RAC", "RAU"))
```

Additionally, individual plot can be obtained:

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
training_load_1 <- training_load[training_load[["ID"]] == 1,  ]

# EWMA
result_EWMA <- EWMA(TL = training_load_1$TL)

# RAC
result_RAC <- RAC(TL = training_load_1$TL,
                  weeks = training_load_1$Week,
                  training_dates = training_load_1$Training_Date)
                    
# RAU
result_RAU <- RAU(TL = training_load_db_1$TL,
                  weeks = training_load_1$Week,
                  training_dates = training_load_1$Training_Date)
```
