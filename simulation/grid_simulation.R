library(devtools)
library(parallel)
load_all("simmicrobes")
source("simulation/simulation_init.R")
sys.source("simulation/update_microbe.R", envir = .GlobalEnv)

ncpus <- min(12, floor(detectCores() * 0.8))
options(mc.cores = ncpus)


overall_simulation <- function(
    microbe_active, microbe_background,
    total_iter, sample_interval,
    sd = 1, radius = 2, arc = pi / 6, ec = 1.2, resource = c(5, 5),
    st_birth = 1, birth_rate = 0.5, st_death = 1, death_rate = 0.1, capacity = 1000) {
  microbe_table <- list()
  microbe_results <- list()

  title <- sprintf("Age = %s", 0)
  microbe_list <- c(microbe_background, microbe_active)
  n_background <- length(microbe_background)
  range_microbe_background <- 1:n_background
  range_microbe_active <- (n_background + 1):length(microbe_list)

  ids <- sapply(microbe_list, "[[", "id")
  ages <- sapply(microbe_list, "[[", "age")
  microbe_table[[title]] <- data.frame(id = ids, age = ages)
  microbe_results[[title]] <- microbe_list

  for (iter in seq_len(total_iter)) {
    # print(paste("Iteration: ", iter, "  Total:", total_iter))
    ## Age
    microbe_list <- lapply(microbe_list, age)
    microbe_list <- update_microbe_movement(microbe_list, sd, radius, arc, ec, resource)
    microbe_active <- microbe_list[range_microbe_active]
    microbe_active <- update_microbe_birth_death(microbe_active, st_birth, birth_rate, st_death, death_rate, capacity)
    microbe_list <- c(microbe_list[range_microbe_background], microbe_active)

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


grid_search <- function(
    total_iter, sample_interval,
    sd = 1, radius = 2, arc = pi / 6, ec = 2,
    capacity = 1000, resource = c(5, 5)) {

  init_bound_active <- 1
  init_bound_bg <- 15

  birth_rate <- 0.3
  death_rate <- 0.1

  microbe_active <- lapply(1:100, function(x) {
    background_microbe(speed = runif(1, 0, 1), location = generate_init_circle(c(0, 0), radius = init_bound_active))
  })

  microbe_background <- lapply(1:1000, function(x) {
    new_microbe(speed = runif(1, 0, 1), location = generate_init_circle(c(0, 0), radius = init_bound_bg))
  })

  results <- overall_simulation(
    microbe_active = microbe_active, microbe_background = microbe_background,
    total_iter = total_iter, sample_interval = sample_interval,
    sd = sd, radius = radius, arc = arc, ec = ec,
    resource = resource, capacity = capacity,
    st_birth = 1, birth_rate = birth_rate, st_death = 1, death_rate = death_rate
  )

  return(results)
}


results_grid_search <- list()
total_iter <- 30
sample_interval <- 5
radius <- 2
arc <- pi / 6
capacity <- 500
resource <- c(5, 5)
sd_values <- c(0.5, 1, 2)
ec_values <- c(0.75, 1, 1.5)

# Extreme values
sd_values <- 1
ec_values <- c(0.2, 1, 10)


param_grid <- expand.grid(sd = sd_values, ec = ec_values)
param_grid$name <- sprintf("sd=%.1f_ec=%.1f", param_grid$sd, param_grid$ec)

ncore <- min(nrow(param_grid), ncpus)
results_list <- mclapply(seq_len(nrow(param_grid)), function(i) {
# results_list2 <- mclapply(7:9, function(i) {
  sd <- param_grid$sd[i]
  ec <- param_grid$ec[i]
  param_comb <- param_grid$name[i]
  results <- grid_search(
    total_iter = total_iter, sample_interval = sample_interval,
    sd = sd, radius = radius, arc = arc, ec = ec,
    capacity = capacity, resource = resource
  )
  list(param_comb = param_comb, results = results)
}, mc.cores = ncore)

results_grid_search <- setNames(
  lapply(results_list, function(x) x$results),
  sapply(results_list, function(x) x$param_comb)
)

saveRDS(results_grid_search, "results_grid_search.rds")
# saveRDS(results_grid_search, "results_grid_search_extreme_ec.rds")


circle_radius <- 15
cir_outer <- generate_circle_points(c(0, 0), circle_radius, n_points = 720)

# results_grid_search <- readRDS("results_grid_search.rds")
png("Figure8_combined_parameters.png", width = 5000, height = 9000, res = 300)
par(mfrow = c(9, 5), mar = c(4, 4, 2, 1), mgp = c(2.5, 1, 0))
for (param in names(results_grid_search)) {
  res_each <- results_grid_search[[param]]$microbe_results
  for (iteration in names(res_each)) {
    res <- res_each[[iteration]]
    iter <- as.numeric(gsub("Age = ([0-9]+)", "\\1", iteration))
    main <- bquote(sigma == .(gsub("sd=([0-9.]+)_ec=.+", "\\1", param)) ~~~
             gamma == ~ .(gsub("sd=.+_ec=([0-9.]+)", "\\1", param)) ~~~
             "Iteration =" ~ .(iter))
    if (iter %in% c(0, 5, 10, 20, 30)) {
      plot_loc_resource(res, main, resource = resource)
      polygon(cir_outer[["x"]], cir_outer[["y"]], border = "black")
    }
  }
}
dev.off()


# results_grid_search <- readRDS("results_grid_search_extreme_ec.rds")
png("FigureS3_combined_extreme_ec.png", width = 5000, height = 3000, res = 300)
length(names(results_grid_search))
par(mfrow = c(3, 5), mar = c(4, 4, 2, 1), mgp = c(2.5, 1, 0))
for (param in names(results_grid_search)) {
  res_each <- results_grid_search[[param]]$microbe_results
  for (iteration in names(res_each)) {
    res <- res_each[[iteration]]
    iter <- as.numeric(gsub("Age = ([0-9]+)", "\\1", iteration))
    main <- bquote(
      # sigma == .(gsub("sd=([0-9.]+)_ec=.+", "\\1", param)) ~~~
          gamma == ~ .(gsub("sd=.+_ec=([0-9.]+)", "\\1", param)) ~~~
          "Iteration =" ~ .(iter))
    if (iter %in% c(0, 5, 10, 20, 30)) {
      plot_loc_resource(res, main, resource = resource)
      polygon(cir_outer[["x"]], cir_outer[["y"]], border = "black")
    }
  }
}
dev.off()




# simulation_real_data <- function() {
#   sd <- 1
#   birth_rate <- 0.5
#   death_rate <- 0.1
#   capacity <- 1000
#   resource <- c(15, 0)

#   background_list <- lapply(1:100, function(x) {
#     background_microbe(
#       speed = runif(1, 0, 1),
#       location = generate_init_circle(c(2, 0), radius = 1)
#     )
#   })

#   general_list <- lapply(1:100, function(x) {
#     new_microbe(
#       speed = runif(1, 0, 1),
#       location = generate_init_circle(c(-2, 0), radius = 1)
#     )
#   })


#   interval <- 3
#   total_time <- 100

#   par(mfrow = c(3, 3))
#   microbe_list <- c(background_list, general_list)
#   plot_loc_resource(microbe_list, "Age = 0", resource = resource)
#   for (i in seq(interval, total_time, by = interval)) {
#     microbe_list <- overall_simulation(microbe_list, interval,
#       sd = sd,
#       radius = 3, arc = pi / 6, ec = 2, resource = resource,
#       birth_rate = birth_rate, death_rate = death_rate, capacity = capacity
#     )
#     plot_loc_resource(microbe_list, paste("Age =", i), resource = resource)
#     abline(v = 0, col = 2)
#   }

#   par(mfrow = c(1, 1))
# }
