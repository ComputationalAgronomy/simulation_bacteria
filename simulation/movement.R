library(devtools)
library(parallel)
load_all("simmicrobes")
source("simulation/simulation_init.R")
source("simulation/update_microbe.R")

ncpus <- min(12, floor(detectCores() * 0.8))
options(mc.cores = ncpus)





movement_diff_attraction <- function(n, total_iter = 100, sd_list = c(0.1, 0.5, 1, 1.5, 2), radius = 0, arc = 0, ec = 0, resource = c(5, 5), ncpu = 4) {
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
        microbes <- update_microbe_movement(microbes, sd, radius, arc, ec, resource)
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
  results <- movement_diff_attraction(n = 500, total_iter = 100, sd_list = sd_list, radius = 0, arc = 0, ncpu = ncpu)
})
# saveRDS(results, "results_movement_attraction.rds")
# results <- readRDS("results_movement_attraction.rds")

png("Figure6_movement_attraction.png", width = 4000, height = 4000, res = 300)

fig_each <- ceiling(sqrt(length(results)))
par(mfrow = c(fig_each, fig_each), mar = c(3, 3, 2, 1), mgp = c(2.5, 0, 0))
for (title in names(results)) {
  plot_loc(results[[title]], title, cex.main = 2)
  points(0, 0, col = "black", pch = 19, cex = 2)
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
    microbes <- update_microbe_movement(microbes, sd, radius, arc, ec, resource)
    if (iter %% sample_interval == 0 || iter == total_iter) {
      title <- sprintf("Age = %s", iter)
      results[[title]] <- microbes
    }
  }
  return(results)
}


system.time({
  results <- movement_by_generation(n = 1000, total_iter = 100, sd = 2, sample_interval = 10)
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
movement_diff_density <- function(total_iter, sample_interval = 10, sd = NA, radius, arc, ec) {
  microbe_background <- lapply(1:500, function(x) {
    # new_microbe(speed = 0, location = c(runif(1, 1, 50), runif(1, -2, 2)))
    new_microbe(speed = 0, location = generate_init_circle(c(0, 0), radius = 2))
  })
  n_active <- 180
  angle <- seq(0, 2 * pi, by = 2 * pi / n_active)[1:n_active]
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
    microbe_temp <- update_microbe_movement(microbe_all, sd, radius, arc, ec, NA)
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
  repulsion_list <- c(0.2, 0.5, 0.75, 1, 1.5, 2, 5)
  radius <- 2
  arc <- pi / 4
  total_iter <- 20
  sample_interval <- 5
  results_all <- list()
  for (ec in repulsion_list) {
    results_all[[sprintf("Repulsion_%s", ec)]] <- movement_diff_density(total_iter, sample_interval, sd = NA, radius = radius, arc = arc, ec = ec)
  }
})


# saveRDS(results_all, "results_movement_density.rds")
# results_all <- readRDS("results_movement_density.rds")

png("Figure7_movement_density.png", width = 5000, height = 7000, res = 300)

lines <- setNames(seq(0, by = -24.5, length = length(repulsion_list)), names(results_all))
par(mfrow = c(7, 5), mar = c(5, 1, 2, 1), oma = c(0, 0, 5, 0))
for (param in names(results_all)) {
  for (title in names(results_all[[param]])) {
    main <- gsub(". Repulsion = .+", "", title)
    plot_loc_two_microbes(results_all[[param]][[title]]$active, results_all[[param]][[title]]$fixed, main)
    # eqscplot
  }
  title(param, outer = TRUE, line = lines[[param]], cex.main = 2)
}

dev.off()


png("Figure7_movement_density_reduced.png", width = 4000, height = 4000, res = 300)

selected_param <- names(results_all)[c(2, 4, 6)]
lines <- setNames(seq(1, by = -32, length = length(selected_param)), selected_param)
par(mfrow = c(3, 3), mar = c(5, 1, 2, 1), oma = c(0, 0, 5, 0))
for (param in selected_param) {
  sub_titles <- names(results_all[[param]])[c(1, 2, 5)]
  for (title in sub_titles) {
    main <- gsub(". Repulsion = .+", "", title)
    plot_loc_two_microbes(results_all[[param]][[title]]$active, results_all[[param]][[title]]$fixed, main, cex.main=2.25, cex.legend=2)
    # eqscplot
  }
  title(gsub("_", "=", param), outer = TRUE, line = lines[[param]], cex.main = 2.5)
}

dev.off()

