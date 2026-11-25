# Reference outputs of RAC(), RAU(), EWMA() and ACWR() with their default
# windows, saved before the `acute` and `chronic` arguments were added
# (commit 31d57a6). test-default-windows.R compares the defaults with it
# using a tolerance of 1e-12, as a cross-platform anchor: it was made on
# arm64, and sum() rounds slightly differently on x86_64. The exact check is
# identical() against the v0.2.0 code (helper-reference-0.2.0.R).
# Run from the package root with the old code loaded:
# devtools::load_all(); source("data-raw/fixture_default_results.R")

source("tests/testthat/helper-scenarios.R")

inputs <- list(
  training_load = training_load,
  training_load_rest = training_load_rest,
  A = scenario_A, B = scenario_B, C = scenario_C, D = scenario_D
)
# Missing load and two sessions on one day
inputs$missing <- inputs$training_load_rest
inputs$missing$TL[c(5, 40)] <- NA
inputs$same_day <- scenario_C
inputs$same_day$Training_Date[3] <- inputs$same_day$Training_Date[2]

ref <- list()
for (nm in names(inputs)) {
  db <- inputs[[nm]]
  one <- db[db$ID == db$ID[1], ]
  for (partial in c(FALSE, TRUE)) {
    key <- paste(nm, partial)
    ref[[key]] <- suppressWarnings(list(
      ACWR = ACWR(db, "ID", "TL", training_dates = "Training_Date",
                  partial = partial),
      RAC = RAC(one$TL, training_dates = one$Training_Date, partial = partial),
      RAU = RAU(one$TL, training_dates = one$Training_Date, partial = partial),
      EWMA = EWMA(one$TL, training_dates = one$Training_Date)
    ))
  }
}
# version = 2 keeps the package installable on R < 3.5.0
saveRDS(ref, "tests/testthat/fixtures/default_results.rds", version = 2)
