

# Deprecated function, use `calculate_dist_table` instead
calculate_dist <- function(i, m, n) {
  sqrt((m[i] - m[-i])^2 + (n[i] - n[-i])^2)
}

# Deprecated function, use `calculate_dist_table` instead
distance.table.2 <- function(m, n) {
  dist_temp <- sapply(seq_along(m), function(i) {
    calculate_dist(i, m, n)
  })
  distance <- t(dist_temp)
  distance
}

#' @export
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
