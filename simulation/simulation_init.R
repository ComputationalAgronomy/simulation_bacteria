

generate_init_square <- function(boundary) {
  c(runif(1, -boundary, boundary), runif(1, -boundary, boundary))
}

generate_init_circle <- function(center, radius) {
  if (length(center) != 2 || !is.numeric(center)) {
    stop("'center' must be a numeric vector of length 2.", call. = FALSE)
  }
  if (radius < 0 || !is.numeric(radius)) {
    stop("'radius' must be a non-negative number.", call. = FALSE)
  }

  theta <- stats::runif(1, 0, 2 * pi)
  r_scaled <- radius * sqrt(stats::runif(1))

  c(x = center[1] + r_scaled * cos(theta), y = center[2] + r_scaled * sin(theta))
}


ncpu <- floor(detectCores() * 0.75)
options(mc.cores = ncpu)
