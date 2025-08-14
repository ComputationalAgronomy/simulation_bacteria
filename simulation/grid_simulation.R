library(devtools)
library(parallel)
load_all("simmicrobes")
source("simulation/simulation_init.R")
source("simulation/update_microbe.R")

ncpus <- min(12, floor(detectCores() * 0.8))
options(mc.cores = ncpus)

overall_simulation <- function(microbe_active, microbe_background, total_iter, sample_interval, sd = 1, radius = 2, arc = pi / 6, ec = 1.2, resource = c(5, 5), st_birth = 1, birth_rate = 0.5, st_death = 1, death_rate = 0.1, capacity = 10000) {

  microbe_table <- list()
  microbe_results <- list()

  n_background <- length(microbe_background)

  title <- sprintf("Age = %s", 0)
  ids <- sapply(microbe_list, "[[", "id")
  ages <- sapply(microbe_list, "[[", "age")
  microbe_table[[title]] <- data.frame(id = ids, age = ages)
  microbe_list <- c(microbe_background, microbe_active)
  microbe_results[[title]] <- microbe_list

  for (iter in seq_len(total_iter)) {
    print(paste("Iteration: ", iter, "  Total:", total_iter))
    ## Age
    microbe_list <- lapply(microbe_list, age)
    microbe_list <- update_microbe_movement(microbe_list, sd, radius, arc, ec, resource)
    microbe_active <- microbe_list[(n_background+1):length(microbe_list)]
    microbe_active <- update_microbe_birth_death(microbe_active, st_birth, birth_rate, st_death, death_rate, capacity)
    microbe_list <- c(microbe_background, microbe_active)
    ## Move
    # x <- y <- vector("numeric")
    # for (i in seq_along(microbe_list)) {
    #   x[i] <- microbe_list[[i]]$location[1]
    #   y[i] <- microbe_list[[i]]$location[2]
    # }
    # inside <- which(abs(x) <= boundary & abs(y) <= boundary)
    # microbe_list <- microbe_list[inside]
    # if (length(microbe_list) > 0) {
    #   x <- x[inside]
    #   y <- y[inside]
    #   # angle_tab_1 <- calulate_angle_table(x, y)
    #   xy_cord <- rbind(x, y)
    #   angle_tab <- calculate_angle_table(x, y)
    #   distance_tab <- calculate_dist_table(x, y)
    #   microbe_list <- lapply(seq_along(microbe_list), function(k) {
    #     move(microbe_list[[k]], distance_tab[k, ], angle_tab[k, ], sd, radius, arc, ec, resource)
    #   })
    # } else {
    #   print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
    #   break
    # }
    # while (iter <= t) {
    # microbe_list <- lapply(microbe_list, age)
    # logic <- lapply(microbe_list, death, st_death, death_rate, length(microbe_list), capacity)
    # survive <- which(logic == TRUE)
    # microbe_list <- microbe_list[survive]
    # new_microbe <- lapply(microbe_list, birth, st_birth, birth_rate, length(microbe_list), capacity)
    # new_microbe <- new_microbe[!is.na(new_microbe)]
    # temp_total_length <- length(microbe_list) + length(new_microbe)
    # temp_total_length

    # if (temp_total_length > capacity) {
    #   new_microbe <- sample(new_microbe, capacity - length(microbe_list), replace = FALSE)
    # }
    # microbe_list <- c(microbe_list, new_microbe)

    l <- length(microbe_list)
    title <- sprintf("Age = %s", iter)
    if (l == 0) {
      print(paste("All microbes die at i th iteration where i = ", iter))
      break
    } else {
      ids <- sapply(microbe_list, "[[", "id")
      ages <- sapply(microbe_list, "[[", "age")
      microbe_table[[title]] <- data.frame(id = ids, age = ages)
    }
    if (iter %% sample_interval == 0 || iter == total_iter) {
      microbe_results[[title]] <- microbe_list
    }
  }
  return(list(microbe_table = microbe_table, microbe_results = microbe_results))
}


overall_search <- function(total_iter, sample_interval, sd = 1, radius = 2, arc = pi / 6, ec = 2, capacity = 10000) {
  init_bound_active <- 1
  init_bound_bg <- 15
  birth_rate <- 0.3
  death_rate <- 0.1
  resource <- c(5, 5)
  # capacity <- 5000

  microbe_active <- lapply(1:100, function(x) {
    background_microbe(speed = runif(1, 0, 1), location = generate_init_circle(c(0, 0), radius = init_bound_active))
  })

  microbe_background <- lapply(1:500, function(x) {
    new_microbe(speed = runif(1, 0, 1), location = generate_init_circle(c(0, 0), radius = init_bound_bg))
  })


  results <- overall_simulation(
    microbe_active = microbe_active, microbe_background = microbe_background,
    total_iter = 50, sample_interval = 5,
    sd = 1, radius = 2, arc = pi / 6, ec = 2,
    resource = resource,
    st_birth = 1, birth_rate = 0.5, st_death = 1, death_rate = 0.1, capacity = 500
  )

  return(results)
}

results <- overall_search(total_iter = 50, sample_interval = 5, sd = 1, radius = 2, arc = pi / 6, ec = 1, capacity = 1000)


png("FigureXX_combined.png", width = 5000, height = 5000, res = 300)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1), mgp = c(2.5, 1, 0))
par(mfrow = c(4, 4), mar = c(4, 4, 2, 1), mgp = c(2.5, 1, 0))

fig_each <- ceiling(sqrt(length(results[["microbe_results"]])))
par(mfrow = c(fig_each, fig_each), mar = c(4, 4, 2, 1), mgp = c(2.5, 1, 0))
for (name in names(results[["microbe_results"]])) {
  res <- results[["microbe_results"]][[name]]
  plot_loc_resource(res, name, resource = resource)
}
dev.off()



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
  microbe_list <- c(background_list, general_list)
  plot_loc_resource(microbe_list, "Age = 0", resource = resource)
  for (i in seq(interval, total_time, by = interval)) {
    microbe_list <- overall_simulation(microbe_list, interval,
      sd = sd,
      radius = 3, arc = pi / 6, ec = 2, resource = resource,
      birth_rate = birth_rate, death_rate = death_rate, capacity = capacity
    )
    plot_loc_resource(microbe_list, paste("Age =", i), resource = resource)
    abline(v = 0, col = 2)
  }

  par(mfrow = c(1, 1))
}
