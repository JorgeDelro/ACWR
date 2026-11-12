
# Numerical behaviour on a daily calendar (version 0.2.0).
# RAC/RAU/EWMA are called with the deprecated `weeks` argument where it
# exists, so the same calls also run against 0.1.0.

run_methods <- function(s, ...) {
  list(
    RAC = RAC(TL = s$TL, weeks = s$Week, training_dates = s$Training_Date, ...),
    RAU = RAU(TL = s$TL, weeks = s$Week, training_dates = s$Training_Date, ...),
    EWMA = EWMA(TL = s$TL, training_dates = s$Training_Date)
  )
}

# Scenarios A-D: value at the last session --------------------------------

# A: daily sessions (Lolli et al. 2019 example: 1.45 and 1.71)
# B: same weekly totals, 3 sessions/week then 6
# C: 3 sessions/week for 4 weeks, then 6
# D: a missing week (must not error)
scenarios <- list(
  A = list(data = scenario_A, RAC = 1.449, RAU = 1.704, EWMA = 1.26),
  B = list(data = scenario_B, RAC = 1.449, RAU = 1.704, EWMA = 1.20),
  C = list(data = scenario_C, RAC = 1.60, RAU = 2.00, EWMA = 1.33),
  D = list(data = scenario_D, RAC = 0.222, RAU = 0.176, EWMA = 0.56)
)

for (sc in names(scenarios)) {
  s <- scenarios[[sc]]$data

  test_that(paste("scenario", sc, "RAC"), {
    expect_no_error(r <- RAC(TL = s$TL, weeks = s$Week,
                             training_dates = s$Training_Date))
    expect_near(last(r$RAC_ACWR), scenarios[[sc]]$RAC)
  })

  test_that(paste("scenario", sc, "RAU"), {
    expect_no_error(r <- RAU(TL = s$TL, weeks = s$Week,
                             training_dates = s$Training_Date))
    expect_near(last(r$RAU_ACWR), scenarios[[sc]]$RAU)
  })

  test_that(paste("scenario", sc, "EWMA"), {
    expect_no_error(r <- EWMA(TL = s$TL, training_dates = s$Training_Date))
    expect_near(last(r$EWMA_ACWR), scenarios[[sc]]$EWMA)
  })
}

test_that("acute and chronic are weekly-equivalent sums", {
  r <- run_methods(scenario_A)
  expect_near(last(r$RAC$RAC_acute), wk_acute)
  expect_near(last(r$RAC$RAC_chronic), (3 * wk_chronic + wk_acute) / 4)
  expect_near(last(r$RAU$RAU_acute), wk_acute)
  expect_near(last(r$RAU$RAU_chronic), wk_chronic)
})

test_that("no rounding inside the functions", {
  r <- run_methods(scenario_A)
  exact <- wk_acute / ((3 * wk_chronic + wk_acute) / 4)
  expect_equal(last(r$RAC$RAC_ACWR), exact, tolerance = 1e-12)
  expect_equal(last(r$RAU$RAU_ACWR), wk_acute / wk_chronic, tolerance = 1e-12)
})

# Shape of the output ------------------------------------------------------

test_that("one value per input row", {
  r <- run_methods(scenario_C)
  n <- nrow(scenario_C)
  for (m in r) for (v in m) expect_length(v, n)
})

test_that("two sessions on the same day are summed for that day", {
  single <- make_scenario(0:29, 300)
  double <- make_scenario(c(0:29, 29), c(rep(300, 29), 200, 100))
  r1 <- run_methods(single)
  r2 <- run_methods(double)
  # Both rows of day 29 get the value of the day with 300 load
  expect_equal(r2$RAC$RAC_ACWR[30:31], rep(last(r1$RAC$RAC_ACWR), 2))
  expect_equal(r2$RAU$RAU_ACWR[30:31], rep(last(r1$RAU$RAU_ACWR), 2))
  expect_equal(r2$EWMA$EWMA_ACWR[30:31], rep(last(r1$EWMA$EWMA_ACWR), 2))
})

test_that("unsorted dates are sorted internally, output keeps input order", {
  set.seed(1)
  ord <- sample(nrow(scenario_C))
  shuffled <- scenario_C[ord, ]
  r_sorted <- run_methods(scenario_C)
  r_shuffled <- run_methods(shuffled)
  expect_equal(r_shuffled$RAC$RAC_ACWR, r_sorted$RAC$RAC_ACWR[ord])
  expect_equal(r_shuffled$RAU$RAU_ACWR, r_sorted$RAU$RAU_ACWR[ord])
  expect_equal(r_shuffled$EWMA$EWMA_ACWR, r_sorted$EWMA$EWMA_ACWR[ord])
})

test_that("Date and POSIXct dates give the same result", {
  s <- scenario_C
  r_date <- run_methods(s)
  s$Training_Date <- as.POSIXct(format(s$Training_Date), tz = "UTC")
  r_posix <- run_methods(s)
  expect_equal(r_posix, r_date)
})

# Warm-up period and `partial` ---------------------------------------------

test_that("RAC/RAU are NA until 28 days of history exist", {
  r <- run_methods(make_scenario(0:40, 500))
  expect_true(all(is.na(r$RAC$RAC_ACWR[1:27])))
  expect_true(all(is.na(r$RAU$RAU_ACWR[1:27])))
  expect_false(anyNA(r$RAC$RAC_ACWR[28:41]))
  expect_false(anyNA(r$RAU$RAU_ACWR[28:41]))
})

test_that("fewer than 28 days: NA by default, values with partial = TRUE", {
  s <- make_scenario(0:13, c(rep(100, 7), rep(300, 7)))
  r <- run_methods(s)
  expect_true(all(is.na(r$RAC$RAC_ACWR)))
  expect_true(all(is.na(r$RAU$RAU_ACWR)))

  rp <- run_methods(s, partial = TRUE)
  # Day 13: acute = 2100; RAC chronic = 2800 over 14 days = 1400/week
  expect_near(last(rp$RAC$RAC_acute), 2100)
  expect_near(last(rp$RAC$RAC_chronic), 1400)
  expect_near(last(rp$RAC$RAC_ACWR), 1.5)
  # RAU chronic = days 0-6 only = 700/week
  expect_near(last(rp$RAU$RAU_chronic), 700)
  expect_near(last(rp$RAU$RAU_ACWR), 3.0)
})

# Missing loads -------------------------------------------------------------

test_that("an NA load makes every window containing it NA, with one warning", {
  s <- make_scenario(0:40, 500)
  s$TL[4] <- NA # day 3
  w <- capture_warnings(r <- run_methods(s))
  # RAC and RAU warn once each; EWMA once
  expect_length(w, 3)
  expect_match(w, "missing", all = TRUE)

  # Chronic windows containing day 3 are those ending on days 3-30
  expect_true(all(is.na(r$RAC$RAC_ACWR[4:31])))
  expect_false(anyNA(r$RAC$RAC_ACWR[32:41]))
  expect_true(all(is.na(r$RAU$RAU_ACWR[4:31])))
  expect_false(anyNA(r$RAU$RAU_ACWR[32:41]))
  # Acute windows containing day 3 end on days 3-9
  expect_true(all(is.na(r$RAC$RAC_acute[4:10])))
  expect_false(anyNA(r$RAC$RAC_acute[11:41]))
  # EWMA is recursive: every day from day 3 on depends on it
  expect_false(anyNA(r$EWMA$EWMA_ACWR[1:3]))
  expect_true(all(is.na(r$EWMA$EWMA_ACWR[4:41])))
})

# EWMA without dates ----------------------------------------------------------

test_that("EWMA without training_dates assumes consecutive days", {
  s <- scenario_A
  expect_message(r <- EWMA(TL = s$TL), "consecutive")
  expect_equal(r, EWMA(TL = s$TL, training_dates = s$Training_Date))
})

# Input validation ----------------------------------------------------------

test_that("invalid inputs give clear errors", {
  s <- scenario_A
  expect_error(RAC(TL = s$TL, training_dates = as.character(s$Training_Date)),
               "Date")
  expect_error(EWMA(TL = s$TL, training_dates = as.character(s$Training_Date)),
               "Date")
  neg <- s$TL
  neg[3] <- -1
  expect_error(RAC(TL = neg, training_dates = s$Training_Date), "negative")
  expect_error(RAU(TL = neg, training_dates = s$Training_Date), "negative")
  expect_error(EWMA(TL = neg, training_dates = s$Training_Date), "negative")
  expect_error(RAC(TL = as.character(s$TL), training_dates = s$Training_Date),
               "numeric")
  expect_error(RAC(TL = s$TL[-1], training_dates = s$Training_Date), "length")
  bad_dates <- s$Training_Date
  bad_dates[2] <- NA
  expect_error(RAC(TL = s$TL, training_dates = bad_dates), "missing")
})

test_that("training_blocks() has been removed", {
  expect_false(exists("training_blocks", envir = asNamespace("ACWR"),
                      inherits = FALSE))
})

# ACWR() wrapper --------------------------------------------------------------

test_that("ACWR() works for a single athlete whose ID is not 1", {
  s <- scenario_C
  s$ID <- "P07"
  res <- ACWR(db = s, ID = "ID", TL = "TL", weeks = "Week", days = "Day",
              training_dates = "Training_Date")
  expect_equal(nrow(res), nrow(s))
  expect_near(last(res$RAC_ACWR), 1.60)
  expect_near(last(res$RAU_ACWR), 2.00)
  expect_near(last(res$EWMA_ACWR), 1.33)
})

test_that("ACWR() handles several athletes and keeps the input row order", {
  a <- scenario_B
  a$ID <- "A"
  b <- scenario_D
  b$ID <- "B"
  db <- rbind(a, b)
  set.seed(2)
  db <- db[sample(nrow(db)), ]

  res <- ACWR(db = db, ID = "ID", TL = "TL", weeks = "Week", days = "Day",
              training_dates = "Training_Date")
  expect_equal(nrow(res), nrow(db))
  expect_equal(res$ID, db$ID)
  expect_equal(res$Training_Date, db$Training_Date)

  for (id in c("A", "B")) {
    rows <- db$ID == id
    expect_equal(res$RAC_ACWR[rows],
                 RAC(TL = db$TL[rows], training_dates = db$Training_Date[rows])$RAC_ACWR)
    expect_equal(res$RAU_ACWR[rows],
                 RAU(TL = db$TL[rows], training_dates = db$Training_Date[rows])$RAU_ACWR)
    expect_equal(res$EWMA_ACWR[rows],
                 EWMA(TL = db$TL[rows], training_dates = db$Training_Date[rows])$EWMA_ACWR)
  }
})

test_that("ACWR() works without the deprecated weeks/days columns", {
  s <- scenario_C[, c("ID", "TL", "Training_Date")]
  expect_no_error(
    res <- ACWR(db = s, ID = "ID", TL = "TL", training_dates = "Training_Date")
  )
  expect_near(last(res$RAC_ACWR), 1.60)
})

test_that("ACWR() reports missing columns and emits a single NA warning", {
  s <- scenario_A
  expect_error(ACWR(db = s, ID = "ID", TL = "Load", weeks = "Week",
                    days = "Day", training_dates = "Training_Date"),
               "Load")
  s2 <- rbind(transform(s, ID = 1), transform(s, ID = 2))
  s2$TL[c(3, 40)] <- NA
  w <- capture_warnings(
    ACWR(db = s2, ID = "ID", TL = "TL", weeks = "Week", days = "Day",
         training_dates = "Training_Date")
  )
  expect_length(w, 1)
})

test_that("matches a brute-force calculation on irregular data", {
  set.seed(3)
  day <- sort(sample(0:69, 45, replace = TRUE)) # gaps and double sessions
  load <- round(runif(length(day), 50, 900))
  s <- make_scenario(day, load)
  r <- run_methods(s)
  keep <- day >= min(day) + 27
  expected <- t(sapply(day[keep], function(t) brute_force(day, load, t)))
  expect_equal(r$RAC$RAC_ACWR[keep], unname(expected[, "RAC"]))
  expect_equal(r$RAU$RAU_ACWR[keep], unname(expected[, "RAU"]))
  expect_equal(r$EWMA$EWMA_ACWR[keep], unname(expected[, "EWMA"]))
})

test_that("the NA warning gives the number of NA days and advises imputation", {
  s <- make_scenario(0:40, 500)
  s$TL[4] <- NA # day 3: EWMA NA on days 3-40
  expect_warning(EWMA(TL = s$TL, training_dates = s$Training_Date),
                 "EWMA: 38 day\\(s\\).*Impute")
  # RAC: chronic windows containing day 3 end on days 27-30 (after warm-up)
  expect_warning(RAC(TL = s$TL, training_dates = s$Training_Date),
                 "RAC: 4 day\\(s\\).*Impute")
})

test_that("the ratio is NA when the chronic load is 0", {
  s <- make_scenario(0:27, c(rep(0, 21), rep(500, 7)))
  r <- RAU(TL = s$TL, training_dates = s$Training_Date)
  expect_equal(last(r$RAU_chronic), 0)
  expect_true(is.na(last(r$RAU_ACWR)))
})
