
test_that("plot_ACWR() shows one bar per day and keeps NA ACWR as missing", {
  d <- data.frame(ID = 1, TL = c(100, 200, 50, NA, 300), Day = c(1, 1, 2, 3, 4),
                  Training_Date = as.Date("2024-01-01") + c(0, 0, 1, 2, 3))
  r <- suppressWarnings(
    ACWR(d, "ID", "TL", days = "Day", training_dates = "Training_Date",
         partial = TRUE)
  )
  p <- plot_ACWR(r, TL = "TL", ACWR = "RAC_ACWR", day = "Day")
  expect_s3_class(p, "r2d3")
  expect_equal(p$x$data$day, c(1, 2, 3, 4))
  expect_equal(p$x$data$TL, c(300, 50, NA, 300))
  expect_true(is.na(p$x$data$ACWR[3]))
})

test_that("plot_ACWR() returns one plot per athlete", {
  data("training_load", package = "ACWR", envir = environment())
  res <- ACWR(training_load, ID = "ID", TL = "TL", days = "Day",
              training_dates = "Training_Date")
  p <- plot_ACWR(res, TL = "TL", ACWR = "EWMA_ACWR", day = "Day", ID = "ID")
  expect_named(p, paste0("ID = ", 1:3))
  expect_equal(nrow(p[[1]]$x$data), 28)
})

test_that("plot_ACWR() sends d3 one entry per calendar day", {
  data("training_load_rest", package = "ACWR", envir = environment())
  res <- ACWR(training_load_rest, ID = "ID", TL = "TL", days = "Day",
              training_dates = "Training_Date")
  p <- plot_ACWR(res, TL = "TL", ACWR = "RAC_ACWR", day = "Day", ID = "ID")

  for (id in unique(res$ID)) {
    sessions <- res[res$ID == id, ]
    d <- p[[paste0("ID = ", id)]]$x$data
    # One entry per calendar day, in order, from the first to the last day
    expect_equal(d$day, seq(min(sessions$Day), max(sessions$Day)))
    # Rest days: bar of height 0, no ratio, not a session
    rest <- !d$day %in% sessions$Day
    expect_true(all(d$TL[rest] == 0))
    expect_true(all(is.na(d$ACWR[rest])))
    expect_equal(d$session, !rest)
    # Session days: summed load
    expect_equal(d$TL[!rest],
                 as.vector(tapply(sessions$TL, sessions$Day, sum)))
  }

  # Athlete 2 does not train in week 5 (days 29-35): 7 empty slots
  d2 <- p[["ID = 2"]]$x$data
  expect_equal(d2$TL[d2$day %in% 29:35], rep(0, 7))

  # Dates give the same calendar
  p_dates <- plot_ACWR(res[res$ID == 2, ], TL = "TL", ACWR = "RAC_ACWR",
                       day = "Training_Date")
  expect_equal(nrow(p_dates$x$data), nrow(d2))
  expect_true(all(diff(p_dates$x$data$day) == 1))
})

test_that("plot_ACWR() rejects day values that are not days or dates", {
  d <- data.frame(TL = 1:3, ACWR = 1, Day = c("a", "b", "c"))
  expect_error(plot_ACWR(d, TL = "TL", ACWR = "ACWR", day = "Day"),
               "day numbers")
})
