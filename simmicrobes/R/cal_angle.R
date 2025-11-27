library(parallel)





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


#' @export
calculate_angle_table <- function(x, y, ncpu = 12) {
  xy_cord <- rbind(x, y)
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


