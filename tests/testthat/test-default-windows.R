# `acute` and `chronic` window arguments.
#
# Default results are checked in two ways:
# * identical() to the 0.2.0 functions (tag v0.2.0, see
#   helper-reference-0.2.0.R), run on the same machine;
# * against fixtures/default_results.rds, outputs saved with the 0.2.0 code
#   (data-raw/fixture_default_results.R). The fixture was made on arm64, and
#   sum() rounds differently on x86_64 (80-bit long double), so this
#   cross-platform anchor uses a tolerance (1e-12 relative) far below any
#   real change but well above that rounding (about 1e-16).

ref <- readRDS(test_path("fixtures", "default_results.rds"))

inputs <- list(
  training_load = training_load,
  training_load_rest = training_load_rest,
  A = scenario_A, B = scenario_B, C = scenario_C, D = scenario_D
)
inputs$missing <- inputs$training_load_rest
inputs$missing$TL[c(5, 40)] <- NA
inputs$same_day <- scenario_C
inputs$same_day$Training_Date[3] <- inputs$same_day$Training_Date[2]

# Outputs of ACWR(), RAC(), RAU() and EWMA() for one input; `fns` is an
# environment with these functions (the package's, or the 0.2.0 ones)
run_all <- function(db, partial, ..., fns = asNamespace("ACWR")) {
  one <- db[db$ID == db$ID[1], ]
  suppressWarnings(list(
    ACWR = fns$ACWR(db, "ID", "TL", training_dates = "Training_Date",
                    partial = partial, ...),
    RAC = fns$RAC(one$TL, training_dates = one$Training_Date,
                  partial = partial, ...),
    RAU = fns$RAU(one$TL, training_dates = one$Training_Date,
                  partial = partial, ...),
    EWMA = fns$EWMA(one$TL, training_dates = one$Training_Date, ...)
  ))
}

test_that("default windows give exactly the results of version 0.2.0", {
  v020 <- reference_0.2.0()
  # The reference is the 0.2.0 code, without the window arguments
  expect_false("acute" %in% names(formals(v020$RAC)))
  expect_true("acute" %in% names(formals(RAC)))
  for (nm in names(inputs)) {
    for (partial in c(FALSE, TRUE)) {
      key <- paste(nm, partial)
      old <- run_all(inputs[[nm]], partial, fns = v020)
      expect_identical(run_all(inputs[[nm]], partial), old,
                       label = paste("defaults:", key))
      expect_identical(run_all(inputs[[nm]], partial,
                               acute = 7, chronic = 28),
                       old, label = paste("acute 7, chronic 28:", key))
    }
  }
})

test_that("default windows match the outputs saved with 0.2.0", {
  expect_length(ref, 16)
  for (nm in names(inputs)) {
    for (partial in c(FALSE, TRUE)) {
      key <- paste(nm, partial)
      expect_equal(run_all(inputs[[nm]], partial), ref[[key]],
                   tolerance = 1e-12, label = paste("defaults:", key))
      expect_equal(run_all(inputs[[nm]], partial, acute = 7, chronic = 28),
                   ref[[key]], tolerance = 1e-12,
                   label = paste("acute 7, chronic 28:", key))
    }
  }
})

# Daily series 1..12 on consecutive days (value at day t is t * 10)
custom <- data.frame(ID = 1, TL = (1:12) * 10,
                     Training_Date = day0 + 0:11)

test_that("RAC with custom windows: weekly-equivalent sums", {
  r <- RAC(custom$TL, training_dates = custom$Training_Date,
           acute = 3, chronic = 9)
  # day 12: acute days 10-12, chronic days 4-12
  acute <- sum(c(100, 110, 120)) * 7 / 3
  chronic <- sum((4:12) * 10) * 7 / 9
  expect_equal(last(r$RAC_acute), acute)
  expect_equal(last(r$RAC_chronic), chronic)
  expect_equal(last(r$RAC_ACWR), acute / chronic)
  # NA until the windows are complete
  expect_true(all(is.na(r$RAC_acute[1:2])))
  expect_false(is.na(r$RAC_acute[3]))
  expect_true(all(is.na(r$RAC_chronic[1:8])))
  expect_false(is.na(r$RAC_chronic[9]))
})

test_that("RAU chronic window is the chronic window minus the acute days", {
  r <- RAU(custom$TL, training_dates = custom$Training_Date,
           acute = 3, chronic = 9)
  # day 12: chronic days 4-9 (6 days)
  chronic <- sum((4:9) * 10) * 7 / 6
  expect_equal(last(r$RAU_chronic), chronic)
  expect_equal(last(r$RAU_ACWR), (330 * 7 / 3) / chronic)
})

test_that("the ratio equals the ratio of mean daily loads", {
  r <- ACWR(custom, "ID", "TL", training_dates = "Training_Date",
            ACWR_method = c("RAC", "RAU"), acute = 4, chronic = 10)
  expect_equal(last(r$RAC_ACWR),
               mean((9:12) * 10) / mean((3:12) * 10))
  expect_equal(last(r$RAU_ACWR),
               mean((9:12) * 10) / mean((3:8) * 10))
})

test_that("EWMA uses N = acute and chronic days", {
  r <- EWMA(custom$TL, training_dates = custom$Training_Date,
            acute = 3, chronic = 10)
  ea <- ec <- custom$TL[1]
  for (t in 2:12) {
    ea <- custom$TL[t] * 2 / 4 + (1 - 2 / 4) * ea
    ec <- custom$TL[t] * 2 / 11 + (1 - 2 / 11) * ec
  }
  expect_equal(last(r$EWMA_acute), ea)
  expect_equal(last(r$EWMA_chronic), ec)
})

test_that("ACWR() passes the windows to every method", {
  r <- ACWR(custom, "ID", "TL", training_dates = "Training_Date",
            acute = 3, chronic = 9)
  expect_equal(r$RAC_ACWR,
               RAC(custom$TL, training_dates = custom$Training_Date,
                   acute = 3, chronic = 9)$RAC_ACWR)
  expect_equal(r$EWMA_ACWR,
               EWMA(custom$TL, training_dates = custom$Training_Date,
                    acute = 3, chronic = 9)$EWMA_ACWR)
})

test_that("window arguments are checked", {
  for (f in list(RAC, RAU)) {
    expect_error(f(custom$TL, training_dates = custom$Training_Date,
                   acute = 7, chronic = 7), "must be longer than `acute`")
    expect_error(f(custom$TL, training_dates = custom$Training_Date,
                   acute = 0), "positive whole number")
    expect_error(f(custom$TL, training_dates = custom$Training_Date,
                   chronic = 21.5), "positive whole number")
  }
  expect_error(EWMA(custom$TL, custom$Training_Date, acute = 28,
                    chronic = 7), "must be longer than `acute`")
  expect_error(ACWR(custom, "ID", "TL", training_dates = "Training_Date",
                    acute = c(7, 14)), "single positive whole number")
})

test_that("with partial = FALSE the ratio is NA until `chronic` days exist", {
  long <- data.frame(ID = 1, TL = 100 + (1:60) %% 5 * 10,
                     Training_Date = day0 + 0:59)
  for (w in list(c(3, 9), c(7, 21), c(7, 28), c(10, 42))) {
    for (f in list(RAC, RAU)) {
      ratio <- f(long$TL, training_dates = long$Training_Date,
                 acute = w[1], chronic = w[2])[[3]]
      expect_equal(which(!is.na(ratio))[1], w[2],
                   label = paste0("first ratio, windows ", w[1], "/", w[2]))
      expect_true(all(!is.na(ratio[w[2]:60])))
    }
  }
})
