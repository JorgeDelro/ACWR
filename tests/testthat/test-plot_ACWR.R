
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
