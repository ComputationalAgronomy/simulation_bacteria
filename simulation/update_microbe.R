library(devtools)
library(parallel)
load_all("simmicrobes")


update_microbe_movement <- function(microbes, sd, radius, arc, ec, resource, max_radius = NA) {
  if (!is.na(max_radius) && max_radius < 0) {
    max_radius <- NA
  }
  x <- sapply(microbes, function(m) m$location[1])
  y <- sapply(microbes, function(m) m$location[2])
  angle_tab <- simmicrobes::calculate_angle_table(x, y)
  distance_tab <- simmicrobes::calculate_dist_table(x, y)
  lapply(seq_along(microbes), function(i) {
    if (microbes[[i]]$speed > 0) {
      microbes[[i]] <- simmicrobes::move(microbes[[i]], distance_tab[i, ], angle_tab[i, ], sd, radius, arc, ec, resource)
      if (!is.na(max_radius)) {
        polar <- convert_cartesian_to_polar(
          microbes[[i]][["location"]][[1]],
          microbes[[i]][["location"]][[2]]
        )
        if (polar[["radius"]] > max_radius) {
          cartesian <- convert_polar_to_cartesian(max_radius, polar[["angle"]])
          microbes[[i]][["location"]] <- cartesian
        }
      }
      return(microbes[[i]])
    }
  })
}


update_microbe_birth_death <- function(microbe_list, st_birth, birth_rate, st_death, death_rate, capacity, birth_move_dist = 0.1) {

  logic <- lapply(microbe_list, simmicrobes::death, st_death, death_rate, length(microbe_list), capacity)
  survive <- which(logic == TRUE)
  microbe_list <- microbe_list[survive]

  new_microbe <- lapply(microbe_list, simmicrobes::birth, st_birth, birth_rate, length(microbe_list), capacity, move_dist = birth_move_dist)
  new_microbe <- new_microbe[!is.na(new_microbe)]
  temp_total_length <- length(microbe_list) + length(new_microbe)
  if (temp_total_length > capacity) {
    new_microbe <- sample(new_microbe, capacity - length(microbe_list), replace = FALSE)
  }
  microbe_list <- c(microbe_list, new_microbe)
  microbe_list
}

