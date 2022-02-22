
test_that("output of RAU function", {

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

  result_RAU <- RAU(TL = training_data$TL,
                    weeks = training_data$Week,
                    training_dates = training_data$Training_Date)

  expect_equal(is.list(result_RAU), TRUE)
  expect_equal(names(result_RAU)[1], "RAU_acute")
  expect_equal(names(result_RAU)[2], "RAU_chronic")
  expect_equal(names(result_RAU)[3], "RAU_ACWR")

  # set user working directory
  setwd(oldwd)

})
