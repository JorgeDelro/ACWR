
test_that("output of ACWR function", {

  # Get old working directory
  oldwd <- getwd()

  # Set temporary directory
  setwd(tempdir())

  # Simulate Training database
  ID <- rep(x = 1, times = 28)
  Week <- c(
    rep(x = 1, n = 7),
    rep(x = 2, n = 7),
    rep(x = 3, n = 7),
    rep(x = 4, n = 7)
  )
  Days <- 1:28
  TL <- c(
    rep(x = 30, times = 21),
    rep(x = 50, times = 7)
  )
  Training_Date <- seq(as.Date("2022/1/1"), as.Date("2022/1/28"), "days")

  training_data <- data.frame(ID, Week, Days, TL, Training_Date)

  result_ACWR <- ACWR(db = training_data,
                      ID = "ID",
                      TL = "TL",
                      weeks = "Week",
                      days = "Days",
                      training_dates = "Training_Date",
                      ACWR_method = c("EWMA", "RAC", "RAU"))

  expect_equal(ncol(result_ACWR), 14)
  expect_equal(class(result_ACWR$EWMA_chronic), "numeric")
  expect_equal(class(result_ACWR$EWMA_acute), "numeric")
  expect_equal(class(result_ACWR$EWMA_ACWR), "numeric")
  expect_equal(class(result_ACWR$RAC_chronic), "numeric")
  expect_equal(class(result_ACWR$RAC_acute), "numeric")
  expect_equal(class(result_ACWR$RAC_ACWR), "numeric")
  expect_equal(class(result_ACWR$RAU_chronic), "numeric")
  expect_equal(class(result_ACWR$RAU_acute), "numeric")
  expect_equal(class(result_ACWR$RAU_ACWR), "numeric")

  # set user working directory
  setwd(oldwd)

})
