library(devtools)
library(parallel)
load_all("simmicrobes")






# general_movement <- function(n, t, boundary, sd = NA, radius, arc, ec) {
#   general_list <- lapply(1:n, function(x) {
#     new_microbe(speed = 1, location = c(runif(1, -5, 5), runif(1, -5, 5)))
#   })
#   iter <- 1
#   while (iter <= t) {
#     x <- y <- vector("numeric")
#     for (i in seq_along(general_list)) {
#       x[i] <- general_list[[i]]$location[1]
#       y[i] <- general_list[[i]]$location[2]
#     }
#     inside <- which(abs(x) <= boundary & abs(y) <= boundary)
#     general_list <- general_list[inside]
#     x <- x[inside]
#     y <- y[inside]
#     distance_tab <- calculate_dist_table(x, y)
#     angle_tab <- calulate_angle_table(x, y)
#     if (length(general_list) > 0) {
#       general_list <- lapply(
#         seq_along(general_list),
#         function(k) {
#           move(general_list[[k]], distance_tab[k, ],
#             angle_tab[k, ],
#             sd = NA, radius, arc, ec
#           )
#         }
#       )
#     } else {
#       print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
#       break
#     }
#     iter <- iter + 1
#   }
#   return(general_list)
# }


# background_movement <- function(n, t, boundary, sd, radius, arc, ec, resource) {
#   background_list <- lapply(1:n, function(x) {
#     background_microbe(speed = runif(1, 0, 1), location = c(runif(1, -1, 1), runif(1, -1, 1)))
#   })
#   iter <- 1
#   while (iter <= t) {
#     x <- y <- vector("numeric")
#     for (i in seq_along(background_list)) {
#       x[i] <- background_list[[i]]$location[1]
#       y[i] <- background_list[[i]]$location[2]
#     }
#     inside <- which(abs(x) <= boundary & abs(y) <= boundary)
#     background_list <- background_list[inside]
#     if (length(background_list) > 0) {
#       x <- x[inside]
#       y <- y[inside]
#       angle_tab <- calulate_angle_table(x, y)
#       distance_tab <- calculate_dist_table(x, y)
#       background_list <- lapply(
#         seq_along(background_list),
#         function(k) {
#           move(
#             background_list[[k]], distance_tab[k, ],
#             angle_tab[k, ], sd, radius, arc, ec, resource
#           )
#         }
#       )
#     } else {
#       print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
#       break
#     }
#     iter <- iter + 1
#   }
#   return(background_list)
# }






overall_simulation <- function(all_microbes, t, sd, radius = 3, boundary = 20, arc = pi / 6, ec = 2, resource = c(5, 5),
                               st_birth = 1, birth_rate = 0.5, st_death = 1, death_rate = 0.1, capacity = 2000) {
  iter <- 1
  while (iter <= t) {
    print(paste("Iteration: ", iter, "  T:", t))
    ## Age
    all_microbes <- lapply(all_microbes, age)
    ## Move
    x <- y <- vector("numeric")
    for (i in seq_along(all_microbes)) {
      x[i] <- all_microbes[[i]]$location[1]
      y[i] <- all_microbes[[i]]$location[2]
    }
    inside <- which(abs(x) <= boundary & abs(y) <= boundary)
    all_microbes <- all_microbes[inside]
    if (length(all_microbes) > 0) {
      x <- x[inside]
      y <- y[inside]
      # angle_tab_1 <- calulate_angle_table(x, y)
      xy_cord <- rbind(x, y)
      angle_tab <- calculate_angle_table(x, y)
      distance_tab <- calculate_dist_table(x, y)
      all_microbes <- lapply(seq_along(all_microbes), function(k) {
        move(all_microbes[[k]], distance_tab[k, ], angle_tab[k, ], sd, radius, arc, ec, resource)
      })
    } else {
      print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
      break
    }
    ## Birth
    pop <- length(all_microbes)
    all_microbes <- c(all_microbes, mclapply(all_microbes, birth, st_birth, birth_rate, pop, capacity))
    all_microbes <- all_microbes[!is.na(all_microbes)]
    ## Death
    pop <- length(all_microbes)
    logic <- mclapply(all_microbes, death, st_death, death_rate, pop, capacity)
    survive <- which(logic == "True")
    all_microbes <- all_microbes[survive]
    if (length(all_microbes) == 0) {
      print(paste("All microbes die at i th iteration where i = ", iter))
      break
    }
    iter <- iter + 1
  }
  return(all_microbes)
}

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


overall_search <- function(sd) {
  init_bound_active <- 1
  init_bound_bg <- 10
  birth_rate <- 0.5
  death_rate <- 0.1

  background_list <- lapply(1:100, function(x) {
    background_microbe(speed = runif(1, 0, 1), location = generate_init_square(init_bound_active))
  })

  general_list <- lapply(1:500, function(x) {
    new_microbe(speed = runif(1, 0, 1), location = generate_init_square(init_bound_bg))
  })


  interval <- 3
  total_time <- 100

  par(mfrow = c(3, 3))
  all_microbes <- c(background_list, general_list)
  plot_loc_resource(all_microbes, "Age = 0")
  for (i in seq(interval, total_time, by = interval)) {
    all_microbes <- overall_simulation(all_microbes, interval, sd, birth_rate = birth_rate, death_rate = death_rate)
    plot_loc_resource(all_microbes, paste("Age =", i))
  }


  # background_list <- overall_simulation(background_list, 10, sd, birth_rate = birth_rate, death_rate = death_rate)
  # plot_loc_resource(background_list, "Age = 20")

  # background_list <- overall_simulation(background_list, 10, sd)
  # plot_loc_resource(background_list, "Age = 40")

  # background_list <- overall_simulation(background_list, 20, sd)
  # plot_loc_resource(background_list, "Age = 60")

  # background_list <- overall_simulation(background_list, 20, sd)
  # plot_loc_resource(background_list, "Age = 80")

  # background_list <- overall_simulation(background_list, 20, sd)
  # plot_loc_resource(background_list, "Age = 100")

  par(mfrow = c(1, 1))
}


overall_search <- function(sd) {
  init_bound_active <- 1
  init_bound_bg <- 10
  birth_rate <- 0.5
  death_rate <- 0.1

  background_list <- lapply(1:100, function(x) {
    background_microbe(speed = runif(1, 0, 1), location = generate_init_square(init_bound_active))
  })

  general_list <- lapply(1:500, function(x) {
    new_microbe(speed = runif(1, 0, 1), location = generate_init_square(init_bound_bg))
  })


  interval <- 3
  total_time <- 100

  par(mfrow = c(3, 3))
  all_microbes <- c(background_list, general_list)
  plot_loc_resource(all_microbes, "Age = 0")
  for (i in seq(interval, total_time, by = interval)) {
    all_microbes <- overall_simulation(all_microbes, interval, sd,
      radius = 3, arc = pi / 6, ec = 2, resource = c(5, 5),
      birth_rate = birth_rate, death_rate = death_rate, capacity = 3000
    )
    plot_loc_resource(all_microbes, paste("Age =", i))
  }
  # sd, radius = 3, boundary = 20, arc = pi / 6, ec = 2, resource = c(5, 5),
  #                                st_birth = 1, birth_rate = 0.5, st_death = 1, death_rate = 0.1, capacity = 5000) {

  # background_list <- overall_simulation(background_list, 10, sd, birth_rate = birth_rate, death_rate = death_rate)
  # plot_loc_resource(background_list, "Age = 20")

  # background_list <- overall_simulation(background_list, 10, sd)
  # plot_loc_resource(background_list, "Age = 40")

  # background_list <- overall_simulation(background_list, 20, sd)
  # plot_loc_resource(background_list, "Age = 60")

  # background_list <- overall_simulation(background_list, 20, sd)
  # plot_loc_resource(background_list, "Age = 80")

  # background_list <- overall_simulation(background_list, 20, sd)
  # plot_loc_resource(background_list, "Age = 100")

  par(mfrow = c(1, 1))
}




simulation_real_data <- function() {
  sd <- 1
  birth_rate <- 0.5
  death_rate <- 0.1
  capacity <- 1000
  resource <- c(15, 0)

  background_list <- lapply(1:100, function(x) {
    background_microbe(
      speed = runif(1, 0, 1),
      location = generate_init_circle(c(2, 0), radius = 1)
    )
  })

  general_list <- lapply(1:100, function(x) {
    new_microbe(
      speed = runif(1, 0, 1),
      location = generate_init_circle(c(-2, 0), radius = 1)
    )
  })


  interval <- 3
  total_time <- 100

  par(mfrow = c(3, 3))
  all_microbes <- c(background_list, general_list)
  plot_loc_resource(all_microbes, "Age = 0", resource = resource)
  for (i in seq(interval, total_time, by = interval)) {
    all_microbes <- overall_simulation(all_microbes, interval,
      sd = sd,
      radius = 3, arc = pi / 6, ec = 2, resource = resource,
      birth_rate = birth_rate, death_rate = death_rate, capacity = capacity
    )
    plot_loc_resource(all_microbes, paste("Age =", i), resource = resource)
    abline(v=0, col=2)
  }

  par(mfrow = c(1, 1))
}





ncpu <- floor(detectCores() * 0.75)
options(mc.cores = ncpu)
