library(devtools)
library(parallel)
load_all("simmicrobes")



## calculate the distance of each microbe's around.
distance.table <- function(m, n) {
  distance <- matrix(NA, length(m), length(m) - 1)
  for (i in seq_along(m)) {
    distance[i, ] <- sqrt((m[i] - m[-i])^2 + (n[i] - n[-i])^2)
  }
  return(distance)
}



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
#     distance_tab <- distance.table(x, y)
#     angle_tab <- angle.table(x, y)
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
#       angle_tab <- angle.table(x, y)
#       distance_tab <- distance.table(x, y)
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


loc.print <- function(all_microbes, main_title, resource = c(5, 5)) {
  back <- list()
  general <- list()
  for (i in seq_along(all_microbes)) {
    if (class(all_microbes[[i]])[2] == "background") {
      back <- c(back, list(all_microbes[[i]]))
    } else {
      general <- c(general, list(all_microbes[[i]]))
    }
  }
  all_microbes <- c(back, general)
  x <- y <- vector("numeric")
  for (i in seq_along(all_microbes)) {
    x[i] <- all_microbes[[i]]$location[1]
    y[i] <- all_microbes[[i]]$location[2]
  }
  boundary <- 15
  bound_lim <- c(-boundary, boundary)
  plot(x, y,
    xlim = bound_lim, ylim = bound_lim,
    col = c(rep("black", length(back)), rep("aquamarine4", length(general))),
    pch = 20, main = main_title, cex = 0.5,
    xaxt = "n", yaxt = "n", axes = FALSE, xlab = "", ylab = ""
  )
  points(resource[1], resource[2], col = "red", pch = 19, cex = 1.5)
}






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
      # angle_tab_1 <- angle.table(x, y)
      xy_cord <- rbind(x, y)
      angle_tab <- cal_angle_table(xy_cord)
      distance_tab <- distance.table(x, y)
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

generate_init_loc <- function(boundary) {
  c(runif(1, -boundary, boundary), runif(1, -boundary, boundary))
}

overall_search <- function(sd) {
  init_bound_active <- 1
  init_bound_bg <- 10
  birth_rate <- 0.5
  death_rate <- 0.1

  background_list <- lapply(1:100, function(x) {
    background_microbe(speed = runif(1, 0, 1), location = generate_init_loc(init_bound_active))
  })

  general_list <- lapply(1:500, function(x) {
    new_microbe(speed = runif(1, 0, 1), location = generate_init_loc(init_bound_bg))
  })


  interval <- 3
  total_time <- 100

  par(mfrow = c(3, 3))
  all_microbes <- c(background_list, general_list)
  loc.print(all_microbes, "Age = 0")
  for (i in seq(interval, total_time, by = interval)) {
    all_microbes <- overall_simulation(all_microbes, interval, sd, birth_rate = birth_rate, death_rate = death_rate)
    loc.print(all_microbes, paste("Age =", i + interval))
  }


  # background_list <- overall_simulation(background_list, 10, sd, birth_rate = birth_rate, death_rate = death_rate)
  # loc.print(background_list, "Age = 20")

  # background_list <- overall_simulation(background_list, 10, sd)
  # loc.print(background_list, "Age = 40")

  # background_list <- overall_simulation(background_list, 20, sd)
  # loc.print(background_list, "Age = 60")

  # background_list <- overall_simulation(background_list, 20, sd)
  # loc.print(background_list, "Age = 80")

  # background_list <- overall_simulation(background_list, 20, sd)
  # loc.print(background_list, "Age = 100")

  par(mfrow = c(1, 1))
}


overall_search <- function(sd) {
  init_bound_active <- 1
  init_bound_bg <- 10
  birth_rate <- 0.5
  death_rate <- 0.1

  background_list <- lapply(1:100, function(x) {
    background_microbe(speed = runif(1, 0, 1), location = generate_init_loc(init_bound_active))
  })

  general_list <- lapply(1:500, function(x) {
    new_microbe(speed = runif(1, 0, 1), location = generate_init_loc(init_bound_bg))
  })


  interval <- 3
  total_time <- 100

  par(mfrow = c(3, 3))
  all_microbes <- c(background_list, general_list)
  loc.print(all_microbes, "Age = 0")
  for (i in seq(interval, total_time, by = interval)) {
    all_microbes <- overall_simulation(all_microbes, interval, sd,
      radius = 3, arc = pi / 6, ec = 2, resource = c(5, 5),
      birth_rate = birth_rate, death_rate = death_rate, capacity = 3000
    )
    loc.print(all_microbes, paste("Age =", i + interval))
  }
  # sd, radius = 3, boundary = 20, arc = pi / 6, ec = 2, resource = c(5, 5),
  #                                st_birth = 1, birth_rate = 0.5, st_death = 1, death_rate = 0.1, capacity = 5000) {

  # background_list <- overall_simulation(background_list, 10, sd, birth_rate = birth_rate, death_rate = death_rate)
  # loc.print(background_list, "Age = 20")

  # background_list <- overall_simulation(background_list, 10, sd)
  # loc.print(background_list, "Age = 40")

  # background_list <- overall_simulation(background_list, 20, sd)
  # loc.print(background_list, "Age = 60")

  # background_list <- overall_simulation(background_list, 20, sd)
  # loc.print(background_list, "Age = 80")

  # background_list <- overall_simulation(background_list, 20, sd)
  # loc.print(background_list, "Age = 100")

  par(mfrow = c(1, 1))
}




simulation_real_data <- function() {
  sd <- 5
  birth_rate <- 0.5
  death_rate <- 0.1
  capacity <- 2000
  resource <- c(10, 0)

  background_list <- lapply(1:100, function(x) {
    background_microbe(
      speed = runif(1, 0, 1),
      location = c(
        runif(1, 1, 2),
        runif(1, -0.5, 0.5)
      )
    )
  })

  general_list <- lapply(1:100, function(x) {
    new_microbe(
      speed = runif(1, 0, 1),
      location = c(
        runif(1, -2, -1),
        runif(1, -0.5, 0.5)
      )
    )
  })


  interval <- 3
  total_time <- 100

  par(mfrow = c(3, 3))
  all_microbes <- c(background_list, general_list)
  loc.print(all_microbes, "Age = 0", resource = resource)
  for (i in seq(interval, total_time, by = interval)) {
    all_microbes <- overall_simulation(all_microbes, interval,
      sd = sd,
      radius = 3, arc = pi / 6, ec = 10, resource = resource,
      birth_rate = birth_rate, death_rate = death_rate, capacity = capacity
    )
    loc.print(all_microbes, paste("Age =", i + interval))
  }

  par(mfrow = c(1, 1))
}





ncpu <- floor(detectCores() * 0.75)
options(mc.cores = ncpu)
