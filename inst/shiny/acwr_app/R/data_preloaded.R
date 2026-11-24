# ============================================================
# Datos pre-cargados (8 atletas)
# ============================================================

# --- Atleta 8: dataset sintético 180 días, 6 variables GPS (FWF Demo 2) ---
# Perfil contrastante: alto volumen inicial que decrece, bloque de partidos
# congestionados, 1 lesión larga, sesiones dobles, partido los domingos.
generate_fwf_demo2_data <- function() {
  set.seed(99)
  n_days <- 180

  meso <- list(
    list(start = 1,   end = 60,  label = "Preparatorio",  factor = 1.10),
    list(start = 61,  end = 130, label = "Competitivo",    factor = 0.85),
    list(start = 131, end = 180, label = "Transición",     factor = 0.60)
  )

  base_train <- c(     380,   6200,   150,    500,  20,   800)
  base_match <- c(     620,   11000,  620,    900,  60,   2300)
  var_names  <- c("sRPE_UA", "distancia_m", "dist_21_kmh",
                  "player_load", "n_acc_3", "dist_hmld")
  n_var <- length(var_names)

  # Lun=regen(35%), Mar=físico(100%), Mié=doble sesión(120%),
  # Jue=táctico(75%), Vie=descanso, Sáb=activación(50%), Dom=partido
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

  # Progresión intra-mesociclo: rampa INVERSA (empieza alto, baja)
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

# --- Atleta 7: dataset sintético 200 días, 6 variables GPS (FWF Demo 1) ---
generate_fwf_demo_data <- function() {
  set.seed(42)
  n_days <- 200

  meso <- list(
    list(start = 1,   end = 50,  label = "Pretemporada",  factor = 0.80),
    list(start = 51,  end = 100, label = "Inicio Liga",   factor = 0.90),
    list(start = 101, end = 150, label = "Competitivo",   factor = 1.00),
    list(start = 151, end = 200, label = "Tapering",      factor = 0.75)
  )

  base_train <- c(     450,   5500,   180,    550,  25,   900)
  base_match <- c(     700,   10500,  550,    850,  55,   2100)
  var_names  <- c("sRPE_UA", "distancia_m", "dist_21_kmh",
                  "player_load", "n_acc_3", "dist_hmld")
  n_var <- length(var_names)

  # Lunes=descanso, Martes=regen(40%), Miércoles=táctico(70%),
  # Jueves=físico(100%), Viernes=activación(60%), Sábado=partido, Domingo=descanso
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

  # Progresión intra-mesociclo: rampa lineal suave (+/- 15%)
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

  # --- Atletas 1-3: dataset training_load del paquete ACWR ---
  tl <- as.data.frame(ACWR::training_load)

  a1_tl <- tl[tl$ID == 1, ]
  a2_tl <- tl[tl$ID == 2, ]
  a3_tl <- tl[tl$ID == 3, ]

  # --- Atletas 4-6: generados ---

  # Atleta 4: Aumento gradual y lineal
  a4 <- seq(16, 54, length.out = 28)
  a4_first21 <- a4[1:21]
  a4_first21 <- a4_first21 * (630 / sum(a4_first21))
  a4_last7   <- a4[22:28]
  a4_last7   <- a4_last7 * (350 / sum(a4_last7))
  a4 <- round(c(a4_first21, a4_last7), 1)
  a4[21] <- a4[21] + (630 - sum(a4[1:21]))
  a4[28] <- a4[28] + (350 - sum(a4[22:28]))

  # Atleta 5: Incremento escalonado (dos niveles)
  a5_first21 <- c(18, 20, 22, 17, 25, 20, 23, 19, 26, 21, 24,
                   30, 34, 28, 36, 32, 38, 35, 40, 37, 35)
  a5_last7   <- c(36, 42, 46, 50, 54, 58, 64)
  a5_first21 <- round(a5_first21 * (630 / sum(a5_first21)), 1)
  a5_last7   <- round(a5_last7 * (350 / sum(a5_last7)), 1)
  a5_first21[21] <- a5_first21[21] + (630 - sum(a5_first21))
  a5_last7[7]    <- a5_last7[7] + (350 - sum(a5_last7))
  a5 <- c(a5_first21, a5_last7)

  # Atleta 6: Pico tardio (cargas constantes, luego rampa abrupta)
  a6_first21 <- c(32, 30, 28, 31, 30, 29, 31, 30, 29, 30, 30,
                   30, 31, 29, 30, 30, 31, 29, 30, 30, 30)
  a6_last7   <- c(10, 18, 30, 48, 68, 82, 94)
  a6_first21 <- round(a6_first21 * (630 / sum(a6_first21)), 1)
  a6_last7   <- round(a6_last7 * (350 / sum(a6_last7)), 1)
  a6_first21[21] <- a6_first21[21] + (630 - sum(a6_first21))
  a6_last7[7]    <- a6_last7[7] + (350 - sum(a6_last7))
  a6 <- c(a6_first21, a6_last7)

  list(
    "Atleta 1 (Williams, 2017)" = data.frame(day = a1_tl$Day, load = a1_tl$TL),
    "Atleta 2 (Williams, 2017)" = data.frame(day = a2_tl$Day, load = a2_tl$TL),
    "Atleta 3 (Williams, 2017)" = data.frame(day = a3_tl$Day, load = a3_tl$TL),
    "Atleta 4 (incremento lineal)" = data.frame(day = 1:28, load = a4),
    "Atleta 5 (incremento aleatorio)" = data.frame(day = 1:28, load = a5),
    "Atleta 6 (incremento abrupto)" = data.frame(day = 1:28, load = a6),
    "Atleta 7 (FWF Demo 1)" = generate_fwf_demo_data(),
    "Atleta 8 (FWF Demo 2)" = generate_fwf_demo2_data()
  )
}

preloaded <- generate_preloaded_data()
