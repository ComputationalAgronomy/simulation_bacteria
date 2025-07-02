library(parallel)


angle.cal.2 <- function(r, m) {
  a <- r - m
  b <- c(1, 0)
  SQRT_SUM_B <- sqrt(sum(b * b))
  base_angle <- acos(sum(a * b) / (sqrt(sum(a * a)) * SQRT_SUM_B))
  if (m[2] >= r[2]) {
    angle <- 2 * pi - base_angle
  } else {
    angle <- base_angle
  }
  return(angle)
}


angle.table.2 <- function(loc) {
  n <- NROW(loc)
  dist <- matrix(NA, n, n - 1)
  for (j in 1:n) {
    base <- loc[j, ]
    dist[j, ] <- apply(loc[-j, ], 1, function(ll) {
      angle.cal.2(ll, base)
    })
  }
  return(dist)
}


angle.table.3 <- function(xy_cord, index, ncpu = 8) {
  dist <- mclapply(index, mc.cores = ncpu, function(i) {
    base <- xy_cord[, i]
    apply(xy_cord[, -i], 2, function(ll) {
      angle.cal.2(ll, base)
    })
  })
  return(dist)
}



time_trial_angle <- function() {
  n <- 1e4
  n <- 2e3
  x <- runif(n)
  y <- runif(n)
  xy_cord <- rbind(x, y)

  system.time({
    angle_tab <- angle.table(xy_cord[1, ], xy_cord[2, ])
  })

  system.time({
    angle_tab_2 <- angle.table.2(t(xy_cord))
  })
  all.equal(angle_tab_2, angle_tab)

  xy_cord <- rbind(x, y)
  system.time({
    angle_tab_3 <- angle.table.3(xy_cord, index = seq_len(NCOL(xy_cord)))
  })
  all.equal(t(simplify2array(angle_tab_3)), angle_tab)
}
