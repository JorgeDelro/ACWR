# Shared fixtures for the numerical tests.
# Day 0 is Monday 2024-01-01. Expected values were recomputed independently
# with a brute-force loop over calendar days before being hard-coded here.

day0 <- as.Date("2024-01-01")

make_scenario <- function(day, load) {
  data.frame(
    ID = 1,
    Week = day %/% 7 + 1,
    Day = day + 1,
    TL = load,
    Training_Date = day0 + day
  )
}

wk_chronic <- 1393.667
wk_acute <- 2375

scenario_A <- make_scenario(
  day = 0:27,
  load = c(rep(wk_chronic / 7, 21), rep(wk_acute / 7, 7))
)

scenario_B <- make_scenario(
  day = c(0, 2, 4, 7, 9, 11, 14, 16, 18, 22:27),
  load = c(rep(wk_chronic / 3, 9), rep(wk_acute / 6, 6))
)

scenario_C <- make_scenario(
  day = c(0, 2, 4, 7, 9, 11, 14, 16, 18, 21, 23, 25, 28:33),
  load = 500
)

scenario_D <- make_scenario(
  day = c(0:27, 38),
  load = 500
)

# Absolute tolerance check (testthat's `tolerance` is relative)
expect_near <- function(object, expected, tol = 0.01) {
  label <- deparse(substitute(object))
  diff <- abs(object - expected)
  expect(
    length(object) == length(expected) && isTRUE(all(diff < tol)),
    sprintf("%s is %s, expected %s (absolute tolerance %s).",
            label, paste(format(object, digits = 4), collapse = ", "),
            paste(expected, collapse = ", "), tol)
  )
  invisible(object)
}

last <- function(x) x[length(x)]

# Brute-force reference: loops over calendar dates, value at day `t`
brute_force <- function(day, load, t) {
  w <- function(d) sum(load[day == d]) # 0 if no session that day
  acute <- sum(sapply((t - 6):t, w))
  ea <- ec <- w(min(day))
  if (t > min(day)) for (d in (min(day) + 1):t) {
    ea <- w(d) * 2 / 8 + (1 - 2 / 8) * ea
    ec <- w(d) * 2 / 29 + (1 - 2 / 29) * ec
  }
  c(RAC = acute / (sum(sapply((t - 27):t, w)) / 4),
    RAU = acute / (sum(sapply((t - 27):(t - 7), w)) / 3),
    EWMA = ea / ec)
}
