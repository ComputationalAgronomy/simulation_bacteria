## Grid research for movement
resource_test <- function(n, t, boundary = 10, radius = 0, arc = 0, ec = 0, resource = c(5, 5)) {
  par(mfrow = c(2, 3))
  g1 <- general_movement(n, t, boundary, sd = NA, radius, arc, ec)
  plot_loc(g1, "No resource")
  g2 <- background_movement(n, t, boundary, sd = 2, radius, arc, ec, c(5, 5))
  plot_loc(g2, "sd = 2")
  g3 <- background_movement(n, t, boundary, sd = 1.5, radius, arc, ec, c(5, 5))
  plot_loc(g3, "sd = 1.5")
  g4 <- background_movement(n, t, boundary, sd = 1, radius, arc, ec, c(5, 5))
  plot_loc(g4, "sd = 1")
  g5 <- background_movement(n, t, boundary, sd = 0.5, radius, arc, ec, c(5, 5))
  plot_loc(g5, "sd = 0.5")
  g6 <- background_movement(n, t, boundary, sd = 0.1, radius, arc, ec, c(5, 5))
  plot_loc(g6, "sd = 0.1")
  par(mfrow = c(1, 1))
}

## Check generations
check_movement <- function(n, t = 100, boundary = 10, sd = 2, radius = 0, arc = 0, ec = 0, resource = c(5, 5)) {
  par(mfrow = c(2, 3))
  background_list <- lapply(1:n, function(x) {
    background_microbe(speed = runif(1, 0, 1), location = c(runif(1, -1, 1), runif(1, -1, 1)))
  })
  plot_loc(background_list, "Age = 0")
  iter <- 1
  while (iter <= 20) {
    x <- y <- vector("numeric")
    for (i in 1:length(background_list)) {
      x[i] <- background_list[[i]]$location[1]
      y[i] <- background_list[[i]]$location[2]
    }
    inside <- which(abs(x) <= boundary & abs(y) <= boundary)
    background_list <- background_list[inside]
    if (length(background_list) > 0) {
      x <- x[inside]
      y <- y[inside]
      angle_tab <- calulate_angle_table(x, y)
      distance_tab <- calculate_dist_table(x, y)
      background_list <- lapply(
        seq_along(background_list),
        function(k) {
          move(
            background_list[[k]], distance_tab[k, ],
            angle_tab[k, ], sd, radius, arc, ec, resource
          )
        }
      )
    } else {
      print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
      break
    }
    iter <- iter + 1
  }
  plot_loc(background_list, "Age = 20")

  while (iter <= 40) {
    x <- y <- vector("numeric")
    for (i in 1:length(background_list)) {
      x[i] <- background_list[[i]]$location[1]
      y[i] <- background_list[[i]]$location[2]
    }
    inside <- which(abs(x) <= boundary & abs(y) <= boundary)
    background_list <- background_list[inside]
    if (length(background_list) > 0) {
      x <- x[inside]
      y <- y[inside]
      angle_tab <- calulate_angle_table(x, y)
      distance_tab <- calculate_dist_table(x, y)
      background_list <- lapply(
        seq_along(background_list),
        function(k) {
          move(
            background_list[[k]], distance_tab[k, ],
            angle_tab[k, ], sd, radius, arc, ec, resource
          )
        }
      )
    } else {
      print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
      break
    }
    iter <- iter + 1
  }
  plot_loc(background_list, "Age = 40")

  while (iter <= 60) {
    x <- y <- vector("numeric")
    for (i in 1:length(background_list)) {
      x[i] <- background_list[[i]]$location[1]
      y[i] <- background_list[[i]]$location[2]
    }
    inside <- which(abs(x) <= boundary & abs(y) <= boundary)
    background_list <- background_list[inside]
    if (length(background_list) > 0) {
      x <- x[inside]
      y <- y[inside]
      angle_tab <- calulate_angle_table(x, y)
      distance_tab <- calculate_dist_table(x, y)
      background_list <- lapply(
        seq_along(background_list),
        function(k) {
          move(
            background_list[[k]], distance_tab[k, ],
            angle_tab[k, ], sd, radius, arc, ec, resource
          )
        }
      )
    } else {
      print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
      break
    }
    iter <- iter + 1
  }
  plot_loc(background_list, "Age = 60")

  while (iter <= 80) {
    x <- y <- vector("numeric")
    for (i in 1:length(background_list)) {
      x[i] <- background_list[[i]]$location[1]
      y[i] <- background_list[[i]]$location[2]
    }
    inside <- which(abs(x) <= boundary & abs(y) <= boundary)
    background_list <- background_list[inside]
    if (length(background_list) > 0) {
      x <- x[inside]
      y <- y[inside]
      angle_tab <- calulate_angle_table(x, y)
      distance_tab <- calculate_dist_table(x, y)
      background_list <- lapply(
        seq_along(background_list),
        function(k) {
          move(
            background_list[[k]], distance_tab[k, ],
            angle_tab[k, ], sd, radius, arc, ec, resource
          )
        }
      )
    } else {
      print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
      break
    }
    iter <- iter + 1
  }
  plot_loc(background_list, "Age = 80")


  while (iter <= 100) {
    x <- y <- vector("numeric")
    for (i in 1:length(background_list)) {
      x[i] <- background_list[[i]]$location[1]
      y[i] <- background_list[[i]]$location[2]
    }
    inside <- which(abs(x) <= boundary & abs(y) <= boundary)
    background_list <- background_list[inside]
    if (length(background_list) > 0) {
      x <- x[inside]
      y <- y[inside]
      angle_tab <- calulate_angle_table(x, y)
      distance_tab <- calculate_dist_table(x, y)
      background_list <- lapply(
        seq_along(background_list),
        function(k) {
          move(
            background_list[[k]], distance_tab[k, ],
            angle_tab[k, ], sd, radius, arc, ec, resource
          )
        }
      )
    } else {
      print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
      break
    }
    iter <- iter + 1
  }
  plot_loc(background_list, "Age = 100")
  par(mfrow = c(1, 1))
}



## set speed to 1
density_test1 <- function(sd = NA, radius, arc, ec) {
  par(mfrow = c(2, 2))
  g1 <- lapply(1:50, function(x) {
    new_microbe(speed = 1, location = c(runif(1, 1, 20), 2.5))
  })
  g2 <- lapply(1:1000, function(x) {
    new_microbe(speed = 0, location = c(runif(1, 1, 50), runif(1, -2, 2)))
  })
  g3 <- lapply(1:50, function(x) {
    new_microbe(speed = 1, location = c(runif(1, 1, 20), -2.5))
  })
  for (i in 1:50) {
    g1[[i]]$location[1] <- i
    g3[[i]]$location[1] <- i
  }
  g1 <- c(g1, g3)
  plot_loc_two_microbes(g1, g2, "Age = 0")
  iter <- 1
  while (iter < 5) {
    x <- y <- vector("numeric")
    for (i in 1:length(g1)) {
      x[i] <- g1[[i]]$location[1]
      y[i] <- g1[[i]]$location[2]
    }
    angle_tab <- calulate_angle_table(x, y)
    distance_tab <- calculate_dist_table(x, y)
    g1 <- lapply(seq_along(g1), function(k) {
      move(
        g1[[k]], distance_tab[k, ],
        angle_tab[k, ], sd, radius, arc, ec
      )
    })
    iter <- iter + 1
  }
  plot_loc_two_microbes(g1, g2, "Age = 5")

  while (iter < 10) {
    x <- y <- vector("numeric")
    for (i in 1:length(g1)) {
      x[i] <- g1[[i]]$location[1]
      y[i] <- g1[[i]]$location[2]
    }
    angle_tab <- calulate_angle_table(x, y)
    distance_tab <- calculate_dist_table(x, y)
    g1 <- lapply(seq_along(g1), function(k) {
      move(
        g1[[k]], distance_tab[k, ],
        angle_tab[k, ], sd, radius, arc, ec
      )
    })
    iter <- iter + 1
  }
  plot_loc_two_microbes(g1, g2, "Age = 10")

  while (iter < 15) {
    x <- y <- vector("numeric")
    for (i in 1:length(g1)) {
      x[i] <- g1[[i]]$location[1]
      y[i] <- g1[[i]]$location[2]
    }
    angle_tab <- calulate_angle_table(x, y)
    distance_tab <- calculate_dist_table(x, y)
    g1 <- lapply(seq_along(g1), function(k) {
      move(
        g1[[k]], distance_tab[k, ],
        angle_tab[k, ], sd, radius, arc, ec
      )
    })
    iter <- iter + 1
  }
  plot_loc_two_microbes(g1, g2, "Age = 15")
}
