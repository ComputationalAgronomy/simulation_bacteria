library(devtools)
library(parallel)
load_all("simmicrobes")


update_microbe_movement <- function(microbes, sd, radius, arc, ec, resource) {
  x <- sapply(microbes, function(m) m$location[1])
  y <- sapply(microbes, function(m) m$location[2])
  angle_tab <- simmicrobes::calculate_angle_table(x, y)
  distance_tab <- simmicrobes::calculate_dist_table(x, y)
  lapply(seq_along(microbes), function(i) {
    if (microbes[[i]]$speed > 0) {
      simmicrobes::move(microbes[[i]], distance_tab[i, ], angle_tab[i, ], sd, radius, arc, ec, resource)
    }
  })
}


update_microbe_birth_death <- function(microbe_list, st_birth, birth_rate, st_death, death_rate, capacity) {

  logic <- lapply(microbe_list, simmicrobes::death, st_death, death_rate, length(microbe_list), capacity)
  survive <- which(logic == TRUE)
  microbe_list <- microbe_list[survive]

  new_microbe <- lapply(microbe_list, simmicrobes::birth, st_birth, birth_rate, length(microbe_list), capacity)
  new_microbe <- new_microbe[!is.na(new_microbe)]
  temp_total_length <- length(microbe_list) + length(new_microbe)
  if (temp_total_length > capacity) {
    new_microbe <- sample(new_microbe, capacity - length(microbe_list), replace = FALSE)
  }
  microbe_list <- c(microbe_list, new_microbe)
  microbe_list
}

