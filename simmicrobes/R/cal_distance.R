library(parallel)


calculate_dist <- function(i, m, n) {
  sqrt((m[i] - m[-i])^2 + (n[i] - n[-i])^2)
}

calculate_dist.2 <- function(i, xy_cord) {
  xx <- (xy_cord[, i] - xy_cord[, -i])^2
  sqrt(colSums(xx))
}


distance.table.2 <- function(m, n) {
  dist_1 <- sapply(seq_along(m), function(i) {
    calculate_dist(i, m, n)
  })
  dist_1
}

distance.table.3 <- function(index, xy_cord, ncpu = 4) {
  distance <- mclapply(seq_along(index), mc.cores = ncpu, function(i) {
    calculate_dist.2(i, xy_cord)
  })
  distance
}


time_trial_distance <- function() {
  l <- 1e4
  m <- runif(l)
  n <- runif(l)
  xy_cord <- rbind(m, n)

  system.time({
    distance <- matrix(NA, length(m), length(m) - 1)
    for (i in 1:length(m)) {
      distance[i, ] <- sqrt((m[i] - m[-i])^2 + (n[i] - n[-i])^2)
      # distance[i, ] <- calculate_dist(i, m, n)
    }
  })


  system.time({
    dist_1 <- distance.table.2(m, n)
  })
  all.equal(distance, t(dist_1))


  system.time({
    dist_temp <- distance.table.3(seq_len(NCOL(xy_cord)), xy_cord, ncpu = 4)
  })
  dist_3 <- t(simplify2array(dist_temp))
  all.equal(distance, dist_3)
}





