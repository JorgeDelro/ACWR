# Code to create `training_load_rest`: 3 athletes over 8 weeks with rest days,
# double sessions and a week without training.
set.seed(2024)

start <- as.Date("2022-01-03") # a Monday
n_weeks <- 8

sessions <- function(ID, weekdays_by_week, load_mean) {
  out <- lapply(seq_len(n_weeks), function(w) {
    wd <- weekdays_by_week(w)
    if (length(wd) == 0) return(NULL)
    day <- (w - 1) * 7 + wd # 0-based calendar day
    data.frame(ID = ID, Week = w, Day = day + 1,
               TL = round(rnorm(length(day), load_mean(w), 40)),
               Training_Date = start + day)
  })
  do.call(rbind, out)
}

# Athlete 1: 4 sessions per week (Mon, Tue, Thu, Sat), stable load
a1 <- sessions(1, function(w) c(0, 1, 3, 5), function(w) 400)

# Athlete 2: 5 days per week with a double session on Wednesdays,
# no training in week 5
a2 <- sessions(2, function(w) if (w == 5) integer(0) else c(0, 1, 2, 2, 4, 5),
               function(w) 300)

# Athlete 3: 3 sessions per week, then 6 sessions in the last week
a3 <- sessions(3, function(w) if (w < 8) c(0, 2, 4) else 0:5,
               function(w) if (w < 8) 450 else 500)

training_load_rest <- rbind(a1, a2, a3)
rownames(training_load_rest) <- NULL

save(training_load_rest, file = "data/training_load_rest.rda",
     compress = "bzip2", version = 2)
