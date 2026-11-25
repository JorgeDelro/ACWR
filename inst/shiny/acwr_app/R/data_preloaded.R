# ============================================================
# Preloaded data (8 athletes + 1 team)
# ============================================================

# --- Athlete 8: synthetic dataset, 180 days, 6 GPS variables (FWF Demo 2) ---
# Contrasting profile: high initial volume that decreases, a block of
# congested matches, 1 long injury, double sessions, match on Sundays.
generate_fwf_demo2_data <- function() {
  set.seed(99)
  n_days <- 180

  meso <- list(
    list(start = 1,   end = 60,  label = "Preparatory",  factor = 1.10),
    list(start = 61,  end = 130, label = "Competitive",    factor = 0.85),
    list(start = 131, end = 180, label = "Transition",     factor = 0.60)
  )

  base_train <- c(     380,   6200,   150,    500,  20,   800)
  base_match <- c(     620,   11000,  620,    900,  60,   2300)
  var_names  <- c("sRPE_UA", "distance_m", "dist_21_kmh",
                  "player_load", "n_acc_3", "dist_hmld")
  n_var <- length(var_names)

  # Mon=recovery(35%), Tue=physical(100%), Wed=double session(120%),
  # Thu=tactical(75%), Fri=rest, Sat=activation(50%), Sun=match
  sess_mult <- c(0.35, 1.00, 1.20, 0.75, 0, 0.50, NA)

  injury <- 95:112
  congested_days <- c(80, 83, 86, 89, 92)

  mat <- matrix(0, nrow = n_days, ncol = n_var)

  for (d in 1:n_days) {
    m_factor <- 1.0
    for (m in meso) {
      if (d >= m$start && d <= m$end) { m_factor <- m$factor; break }
    }

    if (d %in% injury) {
      mat[d, ] <- 0
      next
    }

    dow <- ((d - 1) %% 7) + 1
    is_congested <- d %in% congested_days

    if (dow == 7 || is_congested) {
      for (v in 1:n_var) {
        noise <- rnorm(1, mean = 0, sd = 0.12 * base_match[v])
        mat[d, v] <- max(0, round(base_match[v] * m_factor + noise, 1))
      }
    } else {
      mult <- sess_mult[dow]
      if (mult == 0) {
        mat[d, ] <- 0
      } else {
        for (v in 1:n_var) {
          noise <- rnorm(1, mean = 0, sd = 0.12 * base_train[v] * mult)
          mat[d, v] <- max(0, round(base_train[v] * mult * m_factor + noise, 1))
        }
      }
    }
  }

  # Within-mesocycle progression: REVERSE ramp (starts high, goes down)
  for (m in meso) {
    idx <- m$start:m$end
    len <- length(idx)
    ramp <- seq(1.15, 0.85, length.out = len)
    for (v in 1:n_var) {
      mat[idx, v] <- round(mat[idx, v] * ramp, 1)
    }
  }

  df <- data.frame(day = 1:n_days)
  for (v in 1:n_var) {
    df[[var_names[v]]] <- mat[, v]
  }
  df$load <- df$sRPE_UA
  df
}

# --- Athlete 7: synthetic dataset, 200 days, 6 GPS variables (FWF Demo 1) ---
generate_fwf_demo_data <- function() {
  set.seed(42)
  n_days <- 200

  meso <- list(
    list(start = 1,   end = 50,  label = "Pre-season",  factor = 0.80),
    list(start = 51,  end = 100, label = "League start",   factor = 0.90),
    list(start = 101, end = 150, label = "Competitive",   factor = 1.00),
    list(start = 151, end = 200, label = "Tapering",      factor = 0.75)
  )

  base_train <- c(     450,   5500,   180,    550,  25,   900)
  base_match <- c(     700,   10500,  550,    850,  55,   2100)
  var_names  <- c("sRPE_UA", "distance_m", "dist_21_kmh",
                  "player_load", "n_acc_3", "dist_hmld")
  n_var <- length(var_names)

  # Monday=rest, Tuesday=recovery(40%), Wednesday=tactical(70%),
  # Thursday=physical(100%), Friday=activation(60%), Saturday=match, Sunday=rest
  sess_mult <- c(0, 0.40, 0.70, 1.00, 0.60, NA, 0)

  injury_1 <- 72:80
  injury_2 <- 128:137
  extra_match_weeks <- c(15, 18, 21)

  mat <- matrix(0, nrow = n_days, ncol = n_var)

  for (d in 1:n_days) {
    m_factor <- 1.0
    for (m in meso) {
      if (d >= m$start && d <= m$end) { m_factor <- m$factor; break }
    }

    if (d %in% injury_1 || d %in% injury_2) {
      mat[d, ] <- 0
      next
    }

    dow <- ((d - 1) %% 7) + 1
    week_num <- ((d - 1) %/% 7) + 1
    is_extra_match <- (dow == 3) && (week_num %in% extra_match_weeks)

    if (dow == 6 || is_extra_match) {
      for (v in 1:n_var) {
        noise <- rnorm(1, mean = 0, sd = 0.10 * base_match[v])
        mat[d, v] <- max(0, round(base_match[v] * m_factor + noise, 1))
      }
    } else {
      mult <- sess_mult[dow]
      if (mult == 0) {
        mat[d, ] <- 0
      } else {
        for (v in 1:n_var) {
          noise <- rnorm(1, mean = 0, sd = 0.10 * base_train[v] * mult)
          mat[d, v] <- max(0, round(base_train[v] * mult * m_factor + noise, 1))
        }
      }
    }
  }

  # Within-mesocycle progression: gentle linear ramp (+/- 15%)
  for (m in meso) {
    idx <- m$start:m$end
    len <- length(idx)
    ramp <- seq(0.85, 1.15, length.out = len)
    for (v in 1:n_var) {
      mat[idx, v] <- round(mat[idx, v] * ramp, 1)
    }
  }

  df <- data.frame(day = 1:n_days)
  for (v in 1:n_var) {
    df[[var_names[v]]] <- mat[, v]
  }
  df$load <- df$sRPE_UA
  df
}

generate_preloaded_data <- function() {

  # --- Athletes 1-3: training_load dataset of the ACWR package ---
  tl <- as.data.frame(ACWR::training_load)

  a1_tl <- tl[tl$ID == 1, ]
  a2_tl <- tl[tl$ID == 2, ]
  a3_tl <- tl[tl$ID == 3, ]

  # --- Athletes 4-6: generated ---

  # Athlete 4: gradual, linear increase
  a4 <- seq(16, 54, length.out = 28)
  a4_first21 <- a4[1:21]
  a4_first21 <- a4_first21 * (630 / sum(a4_first21))
  a4_last7   <- a4[22:28]
  a4_last7   <- a4_last7 * (350 / sum(a4_last7))
  a4 <- round(c(a4_first21, a4_last7), 1)
  a4[21] <- a4[21] + (630 - sum(a4[1:21]))
  a4[28] <- a4[28] + (350 - sum(a4[22:28]))

  # Athlete 5: stepped increase (two levels), then a rise in the last week
  a5_first21 <- c(18, 20, 22, 17, 25, 20, 23, 19, 26, 21, 24,
                   30, 34, 28, 36, 32, 38, 35, 40, 37, 35)
  a5_last7   <- c(36, 42, 46, 50, 54, 58, 64)
  a5_first21 <- round(a5_first21 * (630 / sum(a5_first21)), 1)
  a5_last7   <- round(a5_last7 * (350 / sum(a5_last7)), 1)
  a5_first21[21] <- a5_first21[21] + (630 - sum(a5_first21))
  a5_last7[7]    <- a5_last7[7] + (350 - sum(a5_last7))
  a5 <- c(a5_first21, a5_last7)

  # Athlete 6: late peak (constant loads, then a sudden ramp)
  a6_first21 <- c(32, 30, 28, 31, 30, 29, 31, 30, 29, 30, 30,
                   30, 31, 29, 30, 30, 31, 29, 30, 30, 30)
  a6_last7   <- c(10, 18, 30, 48, 68, 82, 94)
  a6_first21 <- round(a6_first21 * (630 / sum(a6_first21)), 1)
  a6_last7   <- round(a6_last7 * (350 / sum(a6_last7)), 1)
  a6_first21[21] <- a6_first21[21] + (630 - sum(a6_first21))
  a6_last7[7]    <- a6_last7[7] + (350 - sum(a6_last7))
  a6 <- c(a6_first21, a6_last7)

  list(
    "Athlete 1 (Williams, 2017)" = data.frame(day = a1_tl$Day, load = a1_tl$TL),
    "Athlete 2 (Williams, 2017)" = data.frame(day = a2_tl$Day, load = a2_tl$TL),
    "Athlete 3 (Williams, 2017)" = data.frame(day = a3_tl$Day, load = a3_tl$TL),
    "Athlete 4 (linear increase)" = data.frame(day = 1:28, load = a4),
    "Athlete 5 (two-step increase)" = data.frame(day = 1:28, load = a5),
    "Athlete 6 (sudden increase)" = data.frame(day = 1:28, load = a6),
    "Athlete 7 (FWF Demo 1)" = generate_fwf_demo_data(),
    "Athlete 8 (FWF Demo 2)" = generate_fwf_demo2_data(),
    # Real dates, rest days and double sessions (athlete 2)
    "Team with rest days (3 athletes, dates)" = data.frame(
      athlete_id = ACWR::training_load_rest$ID,
      date = ACWR::training_load_rest$Training_Date,
      load = ACWR::training_load_rest$TL
    )
  )
}

preloaded <- generate_preloaded_data()
