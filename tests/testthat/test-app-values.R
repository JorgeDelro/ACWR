# The values shown by the Shiny app are the package's output.
# The app's helpers (inst/shiny/acwr_app/R/utils_app.R) and its preloaded
# datasets are loaded without starting Shiny, and every value the app
# displays is compared with ACWR(), fwf_cumulative() and fwf_delta() run
# on the raw sessions.

app_env <- function() {
  app_dir <- system.file("shiny", "acwr_app", "R", package = "ACWR")
  env <- new.env()
  sys.source(file.path(app_dir, "utils_app.R"), env)
  # data_preloaded.R simulates data with set.seed(): keep the test RNG intact
  withr::with_preserve_seed(
    sys.source(file.path(app_dir, "data_preloaded.R"), env)
  )
  env
}

# Every athlete of every preloaded dataset, as the app sees it
app_athletes <- function(env) {
  out <- list()
  for (nm in names(env$preloaded)) {
    sessions <- env$prepare_input(env$preloaded[[nm]])
    for (id in unique(sessions$ID)) {
      one <- sessions[sessions$ID == id, setdiff(names(sessions), "ID")]
      attr(one, "time") <- attr(sessions, "time")
      out[[paste(nm, id)]] <- one
    }
  }
  out
}

# Package output on the raw sessions, one row per session
package_ACWR <- function(sessions, ...) {
  db <- data.frame(ID = 1, TL = sessions$load, date = sessions$date)
  suppressWarnings(ACWR(db, "ID", "TL", training_dates = "date", ...))
}

ratio_cols <- c("RAC_acute", "RAC_chronic", "RAC_ACWR",
                "RAU_acute", "RAU_chronic", "RAU_ACWR",
                "EWMA_acute", "EWMA_chronic", "EWMA_ACWR")

expect_app_matches_package <- function(env, sessions, label, ...) {
  res <- env$acwr_daily(env$daily_series(sessions), ...)
  ref <- package_ACWR(sessions, ...)
  i <- match(ref$date, res$date)
  expect_false(anyNA(i), label = label)
  expect_equal(res[i, ratio_cols], ref[, ratio_cols], ignore_attr = TRUE,
               label = label)
  res
}

test_that("preloaded datasets include rest days and same-day sessions", {
  env <- app_env()
  athletes <- app_athletes(env)
  info <- lapply(athletes, env$calendar_summary)
  expect_true(any(vapply(info, `[[`, numeric(1), "rest_days") > 0))
  expect_true(any(vapply(info, `[[`, numeric(1), "multi_days") > 0))
})

test_that("app ACWR values equal ACWR() for every preloaded athlete", {
  env <- app_env()
  for (nm in names(athletes <- app_athletes(env))) {
    s <- athletes[[nm]]
    res <- expect_app_matches_package(env, s, nm)

    # The daily series covers every calendar day; rest days are 0
    cal <- env$calendar_summary(s)
    expect_equal(nrow(res), cal$span, label = nm)
    rest <- !res$date %in% s$date
    expect_true(all(res$load[rest] == 0), label = nm)
    expect_equal(sum(res$load), sum(s$load), label = nm)

    # Partial windows and other windows
    expect_app_matches_package(env, s, paste(nm, "partial"), partial = TRUE)
    expect_app_matches_package(env, s, paste(nm, "5/21"), acute = 5, chronic = 21)
  }
})

test_that("detailed-calculation tables are built from the package values", {
  env <- app_env()
  athletes <- app_athletes(env)
  for (nm in c("Team with rest days (3 athletes, dates) 2",
               "Athlete 7 (FWF Demo 1) 1")) {
    for (w in list(c(7, 28), c(5, 21))) {
      res <- env$acwr_daily(env$daily_series(athletes[[nm]]),
                            acute = w[1], chronic = w[2])
      rac <- env$detail_rolling(res, "coupled", 0.8, 1.3, 1.5,
                                acute = w[1], chronic = w[2])
      rau <- env$detail_rolling(res, "uncoupled", 0.8, 1.3, 1.5,
                                acute = w[1], chronic = w[2])
      ewma <- env$detail_ewma(res, 0.8, 1.3, 1.5, w[1], w[2])

      # Columns shown are the package's values...
      expect_identical(rac$Acute, res$RAC_acute)
      expect_identical(rac$Chronic, res$RAC_chronic)
      expect_identical(rac$ACWR, res$RAC_ACWR)
      expect_identical(rau$Chronic, res$RAU_chronic)
      expect_identical(ewma$EWMA_Acute, res$EWMA_acute)
      expect_identical(ewma$ACWR, res$EWMA_ACWR)
      # ...and the intermediate sums lead to them
      expect_equal(rac$Acute_sum * 7 / w[1], res$RAC_acute)
      expect_equal(rac$Chronic_sum * 7 / w[2], res$RAC_chronic)
      expect_equal(rau$Chronic_sum * 7 / (w[2] - w[1]), res$RAU_chronic)
      expect_equal((ewma$la_x_Load + ewma$Decay_Acute)[-1],
                   res$EWMA_acute[-1])
      expect_equal((ewma$lc_x_Load + ewma$Decay_Chronic)[-1],
                   res$EWMA_chronic[-1])
    }
  }
})

test_that("app FWF values equal fwf_cumulative() and fwf_delta()", {
  env <- app_env()
  athletes <- app_athletes(env)
  for (nm in c("Athlete 8 (FWF Demo 2) 1",
               "Team with rest days (3 athletes, dates) 2")) {
    s <- athletes[[nm]]
    vars <- setdiff(names(s), "date")
    db <- cbind(ID = 1, s)
    for (f in list(fwf_cumulative, fwf_delta)) {
      long <- f(db, "ID", vars, "date")
      for (v in vars) {
        wide <- env$fwf_wide(long, v, "x", attr(s, "time"))
        lv <- long[long$variable == v & long$window == 7, ]
        expect_equal(wide$x_tau_7, lv$value, label = paste(nm, v))
        expect_equal(nrow(wide), env$calendar_summary(s)$span)
      }
    }
  }
})

test_that("uploaded file with Spanish column names (aliases)", {
  env <- app_env()
  raw <- data.frame(
    Atleta = c("Ana", "Ana", "Ana", "Ana", "Ana", "Luis", "Luis"),
    Fecha = c("02/09/2024", "03/09/2024", "03/09/2024", "05/09/2024",
              "06/09/2024", "02/09/2024", "04/09/2024"),
    Carga = c(450, 300, 200, NA, 500, 410, 380)
  )
  sessions <- env$prepare_input(raw)
  expect_identical(attr(sessions, "time"), "date")
  expect_true(attr(sessions, "has_id"))
  ana <- sessions[sessions$ID == "Ana", c("date", "load")]
  attr(ana, "time") <- "date"

  cal <- env$calendar_summary(ana)
  expect_equal(cal[c("sessions", "span", "rest_days", "multi_days",
                     "missing_load")],
               list(sessions = 5, span = 5, rest_days = 1, multi_days = 1,
                    missing_load = 1))

  daily <- env$daily_series(ana)
  expect_equal(daily$load, c(450, 500, 0, NA, 500))
  expect_app_matches_package(env, ana, "upload", partial = TRUE)
})

test_that("uploaded file with English column names", {
  env <- app_env()
  raw <- data.frame(
    athlete_id = c(1, 1, 1, 2),
    date = c("2024-09-02", "2024-09-02", "2024-09-04", "2024-09-02"),
    load = c(300, 100, 250, 500),
    hsr = c(50, 10, 40, 90)
  )
  sessions <- env$prepare_input(raw)
  expect_identical(attr(sessions, "time"), "date")
  expect_true(attr(sessions, "has_id"))
  expect_named(sessions, c("ID", "date", "load", "hsr"))
  one <- sessions[sessions$ID == "1", c("date", "load")]
  attr(one, "time") <- "date"
  expect_equal(env$daily_series(one)$load, c(400, 0, 250))
  expect_app_matches_package(env, one, "upload (English names)")
})

test_that("numeric days: absent days are rest days", {
  env <- app_env()
  s <- env$prepare_input(data.frame(day = c(1, 2, 4, 4), load = c(10, 20, 5, 5)))
  expect_identical(attr(s, "time"), "day")
  daily <- env$daily_series(s)
  expect_equal(daily$day, 1:4)
  expect_equal(daily$load, c(10, 20, 0, 10))
})

test_that("input errors are explained to the user", {
  env <- app_env()
  expect_error(env$prepare_input(data.frame(fecha = "2024-13-45", load = 1)),
               "date column format is not recognised")
  expect_error(env$prepare_input(data.frame(day = 1.5, load = 1)),
               "whole days")
  expect_error(env$prepare_input(data.frame(day = 1, load = -1)),
               "cannot be negative")
})
