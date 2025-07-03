library(parallel)
library(microbenchmark)

cal_angle_each_deprecated <- function(r, m) {
  a <- r - m
  b <- c(1, 0)
  if (m[2] >= r[2]) {
    angle <- 2 * pi - (acos(sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b)))))
  } else {
    angle <- acos(sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b))))
  }
  # angle[is.na(angle)] <- 0
  return(angle)
}


cal_angle_each <- function(r, m) {
  a <- r - m
  b <- c(1, 0)
  SQRT_SUM_B <- sqrt(sum(b * b))
  base_angle <- acos(sum(a * b) / (sqrt(sum(a * a)) * SQRT_SUM_B))
  if (m[2] >= r[2]) {
    angle <- 2 * pi - base_angle
  } else {
    angle <- base_angle
  }
  # angle[is.na(angle)] <- 0
  return(angle)
}


## calculate the angel of each individual to another.
#' @export
angle.table.1 <- function(x, y) {
  loc <- cbind(x, y)
  dist <- matrix(NA, length(x), length(x) - 1)
  j <- 1
  while (j <= length(x)) {
    for (i in 1:(length(x) - 1)) {
      dist[j, i] <- cal_angle_each(loc[-j, ][i, ], loc[j, ])
    }
    j <- j + 1
  }
  return(dist)
}


angle.table.2 <- function(loc) {
  n <- NROW(loc)
  dist <- matrix(NA, n, n - 1)
  for (j in 1:n) {
    base <- loc[j, ]
    dist[j, ] <- apply(loc[-j, ], 1, function(ll) {
      cal_angle_each(ll, base)
    })
  }
  return(dist)
}

#' @export
cal_angle_table <- function(xy_cord, ncpu = 12) {
  index <- seq_len(NCOL(xy_cord))
  dist <- mclapply(index, mc.cores = ncpu, function(i) {
    base <- xy_cord[, i]
    apply(xy_cord[, -i], 2, function(ll) {
      cal_angle_each(ll, base)
    })
  })
  dist <- t(simplify2array(dist))
  return(dist)
}



time_trial_angle <- function() {
  cat("--- Running time trial for angle calculations ---\n")
  n <- 1000 # Set number of points for the trial
  x <- runif(n)
  y <- runif(n)
  xy_cord <- rbind(x, y)

  cat("\nTiming angle.table.1 (nested loop):\n")
  print(system.time({
    angle_tab <- angle.table.1(xy_cord[1, ], xy_cord[2, ])
  }))

  cat("\nTiming angle.table.2 (for/apply):\n")
  print(system.time({
    angle_tab_2 <- angle.table.2(t(xy_cord))
  }))
  print(paste("Checking equality (1 vs 2):", all.equal(angle_tab_2, angle_tab)))

  cat("\nTiming cal_angle_table (mclapply):\n")
  print(system.time({
    angle_tab_3 <- cal_angle_table(xy_cord, ncpu = floor(parallel::detectCores() * 0.75))
  }))
  print(paste("Checking equality (1 vs 3):", all.equal(angle_tab_3, angle_tab)))
}


#' @title Benchmark the speed of different angle calculation methods.
#' @description This function uses the microbenchmark package to compare the
#' performance of the three primary angle table calculation functions.
#' @noRd
run_angle_benchmark <- function() {
  # Angle calculations are computationally intensive, so use a moderate number of points for the benchmark.
  n_points <- 200
  x <- runif(n_points)
  y <- runif(n_points)

  # Prepare data in the formats required by each function
  loc_matrix <- cbind(x, y)
  xy_cord_matrix <- rbind(x, y)
  ncpu <- parallel::detectCores()

  mb_results <- microbenchmark::microbenchmark(
    "nested_loop" = angle.table.1(x, y),
    "for_apply" = angle.table.2(loc_matrix),
    "mclapply" = cal_angle_table(xy_cord_matrix, ncpu = ncpu),
    times = 10
  )
  print(mb_results)
}


if (interactive()) {
  time_trial_angle()
  # run_angle_benchmark()
}
