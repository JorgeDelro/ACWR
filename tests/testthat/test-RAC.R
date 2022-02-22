
test_that("output of RAC function", {

  # Get old working directory
  oldwd <- getwd()

  # Set temporary directory
  setwd(tempdir())

  # Simulate Training load
  Week <- c(
    rep(x = 1, n = 7),
    rep(x = 2, n = 7),
    rep(x = 3, n = 7),
    rep(x = 4, n = 7)
  )
  TL <- c(
    rep(x = 30, times = 21),
    rep(x = 50, times = 7)
  )
  Training_Date <- seq(as.Date("2022/1/1"), as.Date("2022/1/28"), "days")

  training_data <- data.frame(Week, TL, Training_Date)

  result_RAC <- RAC(TL = training_data$TL,
                      weeks = training_data$Week,
                      training_dates = training_data$Training_Date)

  expect_equal(is.list(result_RAC), TRUE)
  expect_equal(names(result_RAC)[1], "RAC_acute")
  expect_equal(names(result_RAC)[2], "RAC_chronic")
  expect_equal(names(result_RAC)[3], "RAC_ACWR")

  # set user working directory
  setwd(oldwd)

})
