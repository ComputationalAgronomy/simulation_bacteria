library(devtools)
library(parallel)
load_all("simmicrobes")
source("simulation/simulation_init.R")
ncpu <- floor(detectCores() * 2 / 3)




update_microbe_list <- function(microbes, sd, radius, arc, ec, resource) {
  x <- sapply(microbes, function(m) m$location[1])
  y <- sapply(microbes, function(m) m$location[2])
  angle_tab <- simmicrobes::calculate_angle_table(x, y)
  distance_tab <- calculate_dist_table(x, y)
  lapply(
    seq_along(microbes),
    function(i) {
      if (microbes[[i]]$speed > 0) {
        move(
          microbes[[i]], distance_tab[i, ],
          angle_tab[i, ], sd, radius, arc, ec, resource
        )
      }
    }
  )
}



different_attraction_movement <- function(n, total_iter = 100, sd_list = c(0.1, 0.5, 1, 1.5, 2), radius = 0, arc = 0, ec = 0, resource = c(5, 5), ncpu = 4) {

  microbes_init <- lapply(1:n, function(x) {
    background_microbe(speed = runif(1, 0, 1), location = generate_init_circle(c(0, 0), radius = 1))
  })
  ncpu <- min(length(sd_list), ncpu)
  results <- list()
  results[["Initial"]] <- microbes_init
  results_list <- mclapply(
    sd_list,
    function(sd) {
      microbes <- microbes_init
      for (iter in seq_len(total_iter)) {
        microbes <- update_microbe_list(microbes, sd, radius, arc, ec, resource)
      }
      title <- sprintf("Attraction = %s", sd)
      list(title = title, microbes = microbes)
    },
    mc.cores = ncpu
  )
  for (res in results_list) {
    results[[res$title]] <- res$microbes
  }
  return(results)
}

sd_list <- c(0.001, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 3)
sd_list <- c(0.1, 0.5, 1, 1.5, 2, 3, 5, 10)
system.time({
  results <- different_attraction_movement(n = 100, total_iter = 100, sd_list = sd_list, radius = 0, arc = 0, ncpu = ncpu)
})
# saveRDS(results, "results_movement_attraction.rds")
# results <- readRDS("results_movement_attraction.rds")

png("Figure5_movement_attraction.png", width = 4000, height = 4000, res = 300)

fig_each <- ceiling(sqrt(length(results)))
par(mfrow = c(fig_each, fig_each), mar = c(3, 3, 2, 1), mgp = c(2.5, 0, 0))
for (title in names(results)) {
  plot_loc(results[[title]], title)
  points(0, 0, col = "green", pch = 19, cex = 2)
}

dev.off()


####################
movement_by_generation <- function(n, total_iter = 100, boundary = 10, sd = 2, radius = 0, arc = 0, ec = 0, resource = c(5, 5), sample_interval = 20) {

  microbes <- lapply(1:n, function(x) {
    background_microbe(speed = runif(1, 0, 1), location = generate_init_circle(c(0, 0), radius = 1))
  })
  iter <- 0
  title <- sprintf("Age = %s", iter)
  results <- list()
  results[[title]] <- microbes
  for (iter in seq_len(total_iter)) {
    microbes <- update_microbe_list(microbes, sd, radius, arc, ec, resource)
    if (iter %% sample_interval == 0 || iter == total_iter) {
      title <- sprintf("Age = %s", iter)
      results[[title]] <- microbes
    }
  }
  return(results)
}


system.time({
  results <- movement_by_generation(n = 1000, total_iter = 100, sd = 2, plot_interval = 10)
})
# saveRDS(results, "results_movement_attraction_generations.rds")
# results <- readRDS("results_movement_attraction_generations.rds")

png("FigureS2_movement_attraction_generations.png", width = 6000, height = 4500, res = 300)

fig_each <- ceiling(sqrt(length(results)))
par(mfrow = c(fig_each, fig_each), mar = c(3, 3, 2, 1), mgp = c(2.5, 0, 0))
par(mfrow = c(3, 4), mar = c(3, 3, 2, 1), mgp = c(2.5, 0, 0))
for (title in names(results)) {
  plot_loc(results[[title]], title)
  points(0, 0, col = "green", pch = 19, cex = 2)
}

dev.off()


#######
density_test <- function(total_iter, sample_interval = 10, sd = NA, radius, arc, ec) {

  microbe_background <- lapply(1:500, function(x) {
    # new_microbe(speed = 0, location = c(runif(1, 1, 50), runif(1, -2, 2)))
    new_microbe(speed = 0, location = generate_init_circle(c(0, 0), radius = 2))
  })
  n_active <- 180
  angle <- seq(0, 2 * pi, by=2*pi/n_active)[1:n_active]
  microbe_active <- lapply(angle, function(x) {
    new_microbe(speed = 0.5, location = convert_polar_to_cartesian(radius = 2.5, angle = x))
    # y_loc <- 2.5
    # list(
    #   new_microbe(speed = 1, location = c(x, y_loc)),
    #   new_microbe(speed = 1, location = c(x, -y_loc))
    # )
  })


  results <- list()
  title <- sprintf("Age = %s. Repulsion = %s", 0, ec)
  results[[title]] <- list(active = microbe_active, fixed = microbe_background)
  for (iter in seq_len(total_iter)) {
    microbe_all <- c(microbe_active, microbe_background)
    microbe_temp <- update_microbe_list(microbe_all, sd, radius, arc, ec, NA)
    microbe_active <- microbe_temp[1:n_active]
    if (iter %% sample_interval == 0 || iter == total_iter) {
      title <- sprintf("Age = %s. Repulsion = %s", iter, ec)
      results[[title]] <- list(active = microbe_active, fixed = microbe_background)
    }
  }
  return(results)
}



system.time({
  # repulsion_list <- c(0.25, 0.5, 1, 2, 5)
  repulsion_list <- c(0.5, 0.75, 1, 1.5, 2)
  radius <- 2
  arc <- pi / 4
  total_iter <- 20
  sample_interval <- 5
  results_all <- list()
  for (ec in repulsion_list) {
    results_all[[sprintf("Repulsion_%s", ec)]] <- density_test(total_iter, sample_interval, sd = NA, radius = radius, arc = arc, ec = ec)
  }
})
lines <- setNames(c(1, -10, -20, -30, -40), names(results_all))
lines <- setNames(seq(0, -100, by = -24), names(results_all))

# saveRDS(results, "results_movement_density.rds")
# results <- readRDS("results_movement_density.rds")

png("Figure6_movement_density.png", width = 5000, height = 5000, res = 300)

par(mfrow = c(5, 5), mar = c(5, 1, 2, 1), oma = c(0, 0, 5, 0), pty = "s")
for(param in names(results_all)) {

  for (title in names(results_all[[param]])) {
    plot_loc_two_microbes(results_all[[param]][[title]]$active, results_all[[param]][[title]]$fixed, title)
   #eqscplot
  }
  title(param, outer = TRUE, line = lines[[param]], cex.main = 2)
}

dev.off()

