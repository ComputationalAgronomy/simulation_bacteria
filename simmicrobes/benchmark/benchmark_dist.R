library(parallel)
library(microbenchmark)


calculate_dist <- function(i, m, n) {
  sqrt((m[i] - m[-i])^2 + (n[i] - n[-i])^2)
}

calculate_dist.2 <- function(i, xy_cord) {
  xx <- (xy_cord[, i] - xy_cord[, -i])^2
  sqrt(colSums(xx))
}


distance.table.1 <- function(m, n) {
  distance <- matrix(NA, length(m), length(m) - 1)
  for (i in seq_along(m)) {
    distance[i, ] <- sqrt((m[i] - m[-i])^2 + (n[i] - n[-i])^2)
  }
  distance
}

distance.table.2 <- function(m, n) {
  dist_temp <- sapply(seq_along(m), function(i) {
    calculate_dist(i, m, n)
  })
  distance <- t(dist_temp)
  distance
}

distance.table.3 <- function(xy_cord, ncpu = 4) {
  index <- seq_len(NCOL(xy_cord))
  dist_temp <- mclapply(index, mc.cores = ncpu, function(i) {
    calculate_dist.2(i, xy_cord)
  })
  distance <- t(simplify2array(dist_temp))
  distance
}

calculate_dist_table <- function(m, n) {
  coords <- cbind(m, n)
  dist_temp <- as.matrix(dist(coords))
  # distance <- t(sapply(seq_len(NROW(dist_temp)), function(i) dist_temp[i, -i]))
  # distance
  num_points <- nrow(coords)
  diag_indices <- seq.int(from = 1, by = num_points + 1, length.out = num_points)
  vector_no_diag <- as.vector(dist_temp)[-diag_indices]
  t(matrix(vector_no_diag, nrow = num_points - 1, ncol = num_points))
}

time_trial_distance <- function() {
  l <- 1e4
  m <- runif(l)
  n <- runif(l)
  xy_cord <- rbind(m, n)

  system.time({
    distance <- matrix(NA, length(m), length(m) - 1)
    for (i in seq_along(m)) {
      distance[i, ] <- sqrt((m[i] - m[-i])^2 + (n[i] - n[-i])^2)
    }
  })

  system.time({
    dist_1 <- distance.table.1(m, n)
  })
  all.equal(distance, dist_1)

  system.time({
    dist_2 <- distance.table.2(m, n)
  })
  all.equal(distance, dist_2)

  system.time({
    dist_3 <- distance.table.3(xy_cord, ncpu = 4)
  })
  all.equal(distance, dist_3)

  system.time({
    dist_4 <- calculate_dist_table(m, n)
  })
  all.equal(distance, dist_4)
}



run_microbenchmark <- function() {
  n_points <- 5000
  m <- runif(n_points)
  n <- runif(n_points)
  ncpu <- detectCores() * 0.75
  xy_cord <- rbind(m, n)

  mb_results <- microbenchmark(
    "for_loop" = distance.table.1(m, n),
    "sapply" = distance.table.2(m, n),
    "mclapply" = distance.table.3(xy_cord, ncpu = ncpu),
    "dist_based" = calculate_dist_table(m, n),
    times = 10
  )

  print(mb_results)
}


run_conversion_benchmark <- function() {
  n_points <- 10000
  coords <- cbind(runif(n_points), runif(n_points))
  dist_n_by_n <- as.matrix(dist(coords))

  # Method 1: Sapply (loop-based)
  convert_sapply <- function(mat) {
    t(sapply(seq_len(NROW(mat)), function(i) mat[i, -i]))
  }

  # Method 2: Vectorized indexing
  convert_vectorized <- function(mat) {
    num_points <- NROW(mat)
    diag_indices <- seq.int(from = 1, by = num_points + 1, length.out = num_points)
    vector_no_diag <- as.vector(mat)[-diag_indices]
    t(matrix(vector_no_diag, nrow = num_points - 1, ncol = num_points))
  }

  mb_results <- microbenchmark(
    "sapply_based" = convert_sapply(dist_n_by_n),
    "vectorized" = convert_vectorized(dist_n_by_n),
    times = 10
  )

  print(mb_results)
}


if (interactive()) {
  # time_trial_distance()
  run_microbenchmark()
  # run_conversion_benchmark()
}
