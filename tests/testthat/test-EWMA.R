
test_that("output of EWMA function", {

  # Get old working directory
  oldwd <- getwd()

  # Set temporary directory
  setwd(tempdir())

  # Simulate Training load
  TL <- c(
    rep(x = 30, times = 21),
    rep(x = 50, times = 7)
  )

  result_EWMA <- EWMA(TL)

  expect_equal(is.list(result_EWMA), TRUE)
  expect_equal(names(result_EWMA)[1], "EWMA_acute")
  expect_equal(names(result_EWMA)[2], "EWMA_chronic")
  expect_equal(names(result_EWMA)[3], "EWMA_ACWR")

  # set user working directory
  setwd(oldwd)

})
