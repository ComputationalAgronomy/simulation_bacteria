#' @export
convert_polar_to_cartesian <- function(radius, angle) {
  x <- radius * cos(angle)
  y <- radius * sin(angle)
  return(c(x = x, y = y))
}

#' @export
convert_cartesian_to_polar <- function(x, y) {
  radius <- sqrt(x^2 + y^2)
  angle <- atan2(y, x)
  return(c(radius = radius, angle = angle))
}


generate_circle_points <- function(center, radius, n_points = 720) {
  if (length(center) != 2 || !is.numeric(center)) {
    stop("'center' must be a numeric vector of length 2.", call. = FALSE)
  }
  if (radius < 0 || !is.numeric(radius)) {
    stop("'radius' must be a non-negative number.", call. = FALSE)
  }

  theta <- seq(0, 2 * pi, length.out = n_points)
  x <- center[1] + radius * cos(theta)
  y <- center[2] + radius * sin(theta)
  return(data.frame(x = x, y = y))
}
