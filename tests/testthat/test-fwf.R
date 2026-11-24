# FWF cumulative and delta variations on a daily calendar.
# Hand-computed case: days 0, 1 and 3 (day 2 is a rest day), two sessions on
# day 3 (30 + 10), so the daily series is 10, 20, 0, 40.

hand <- data.frame(
  ID = 1,
  TL = c(30, 10, 20, 10),
  Training_Date = day0 + c(3, 0, 1, 3)
)

get_values <- function(res, w, var = "TL", id = 1) {
  res$value[res$ID == id & res$variable == var & res$window == w]
}

test_that("fwf_cumulative() sums the last tau calendar days", {
  r <- fwf_cumulative(hand, "ID", "TL", "Training_Date", windows = 1:4)
  expect_equal(unique(r$date), day0 + 0:3)
  expect_equal(get_values(r, 1), c(10, 20, 0, 40))
  expect_equal(get_values(r, 2), c(NA, 30, 20, 40))
  expect_equal(get_values(r, 3), c(NA, NA, 30, 60))
  expect_equal(get_values(r, 4), c(NA, NA, NA, 70))
})

test_that("fwf_delta() is x(t) - x(t - tau)", {
  r <- fwf_delta(hand, "ID", "TL", "Training_Date", windows = 1:4)
  expect_equal(get_values(r, 1), c(NA, 10, -20, 40))
  expect_equal(get_values(r, 2), c(NA, NA, -10, 20))
  expect_equal(get_values(r, 3), c(NA, NA, NA, 30))
  expect_equal(get_values(r, 4), rep(NA_real_, 4))
})

test_that("fwf_delta() is the day-to-day change of fwf_cumulative()", {
  s <- scenario_C
  cum <- fwf_cumulative(s, "ID", "TL", "Training_Date", windows = c(3, 7))
  dif <- fwf_delta(s, "ID", "TL", "Training_Date", windows = c(3, 7))
  for (w in c(3, 7)) {
    x <- get_values(cum, w)
    expect_equal(get_values(dif, w)[-1], diff(x))
  }
})

test_that("output is a long tibble sorted by ID, date, variable, window", {
  db <- rbind(
    data.frame(ID = "b", TL = c(5, 6), HSR = c(1, 2),
               Training_Date = day0 + c(1, 0)),
    data.frame(ID = "a", TL = 7, HSR = 3, Training_Date = day0)
  )
  r <- fwf_cumulative(db, "ID", c("TL", "HSR"), "Training_Date",
                      windows = c(2, 1))
  expect_s3_class(r, "tbl_df")
  expect_named(r, c("ID", "date", "variable", "window", "value"))
  expect_equal(nrow(r), 2 * 2 * 2 + 1 * 2 * 2)
  expect_equal(r$ID, rep(c("b", "a"), c(8, 4)))
  expect_equal(r$variable[1:4], c("TL", "TL", "HSR", "HSR"))
  expect_equal(r$window[1:4], c(1L, 2L, 1L, 2L))
  # athlete b: TL 6 then 5, HSR 2 then 1
  expect_equal(get_values(r, 1, "TL", "b"), c(6, 5))
  expect_equal(get_values(r, 2, "TL", "b"), c(NA, 11))
  expect_equal(get_values(r, 2, "HSR", "b"), c(NA, 3))
  expect_equal(get_values(r, 1, "HSR", "a"), 3)
})

test_that("POSIXct dates give the same result as Date", {
  hand_ct <- hand
  hand_ct$Training_Date <- as.POSIXct(paste(hand$Training_Date, "18:30"),
                                      tz = "UTC")
  expect_equal(fwf_cumulative(hand_ct, "ID", "TL", "Training_Date", 1:2),
               fwf_cumulative(hand, "ID", "TL", "Training_Date", 1:2))
})

test_that("a missing load makes its windows NA, with a warning", {
  db <- hand
  db$TL[db$Training_Date == day0 + 1] <- NA
  expect_warning(
    r <- fwf_cumulative(db, "ID", "TL", "Training_Date", windows = 1:2),
    "1 session\\(s\\) have a missing value"
  )
  expect_equal(get_values(r, 1), c(10, NA, 0, 40))
  expect_equal(get_values(r, 2), c(NA, NA, NA, 40))
})

test_that("tau 7 / (tau 28 / 4) equals the RAC ratio", {
  for (data_name in c("training_load", "training_load_rest")) {
    db <- get(data_name, envir = asNamespace("ACWR"))
    cum <- fwf_cumulative(db, "ID", "TL", "Training_Date", windows = c(7, 28))
    acute <- cum[cum$window == 7, ]
    chronic <- cum[cum$window == 28, ]
    ratio <- acute$value / (chronic$value / 4)

    rac <- ACWR(db, "ID", "TL", training_dates = "Training_Date",
                ACWR_method = "RAC")
    key_fwf <- paste(acute$ID, acute$date)
    key_rac <- paste(rac$ID, as.Date(rac$Training_Date))
    expect_equal(ratio[match(key_rac, key_fwf)], rac$RAC_ACWR,
                 label = data_name)
    expect_equal(acute$value[match(key_rac, key_fwf)], rac$RAC_acute)
  }
})

test_that("argument checks", {
  expect_error(fwf_cumulative(hand, "ID", "nope", "Training_Date"),
               "not found")
  expect_error(fwf_cumulative(transform(hand, TL = as.character(TL)),
                              "ID", "TL", "Training_Date"), "numeric")
  expect_error(fwf_delta(hand, "ID", "TL", "Training_Date", windows = 0),
               "positive whole numbers")
  expect_error(fwf_delta(hand, "ID", "TL", "Training_Date", windows = 1.5),
               "positive whole numbers")
  expect_error(fwf_delta(transform(hand, Training_Date = "2024-01-01"),
                         "ID", "TL", "Training_Date"), "Date or POSIXct")
})

test_that("plot_fwf() draws one athlete and one day", {
  skip_if_not_installed("ggplot2")
  r <- fwf_delta(training_load, "ID", "TL", "Training_Date")
  last_day <- max(training_load$Training_Date)
  p <- plot_fwf(r, ID = 1, date = last_day)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 28)
  expect_true(all(p$data$date == as.Date(last_day)))
  expect_match(p$labels$title, "delta")

  z <- plot_fwf(r, ID = 1, date = format(last_day), standardise = TRUE)
  expect_s3_class(z, "ggplot")

  expect_error(plot_fwf(r, date = last_day), "choose one with `ID`")
  expect_error(plot_fwf(r, ID = 99, date = last_day), "must be one of")
  expect_error(plot_fwf(r, ID = 1, date = "1999-01-01"), "must be a day")
  expect_error(plot_fwf(training_load, ID = 1, date = last_day),
               "output of fwf_cumulative")
})

test_that("fwf_delta() with window 7 compares the same weekday", {
  two_weeks <- data.frame(ID = 1, TL = c(300, 200, 450),
                          Training_Date = as.Date(c("2024-01-01",
                                                    "2024-01-03",
                                                    "2024-01-08")))
  d7 <- fwf_delta(two_weeks, "ID", "TL", "Training_Date", windows = 7)
  expect_equal(weekdays(as.Date("2024-01-01")), weekdays(as.Date("2024-01-08")))
  expect_equal(d7$value[d7$date == as.Date("2024-01-08")], 150)
})
