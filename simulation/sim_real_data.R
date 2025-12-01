library(devtools)
library(parallel)
load_all("simmicrobes")
source("simulation/simulation_init.R")
sys.source("simulation/update_microbe.R", envir = .GlobalEnv)

ncpus <- min(12, floor(detectCores() * 0.8))
options(mc.cores = ncpus)




real_data_simulation <- function(
    microbe_active, microbe_background,
    total_iter, sample_interval,
    sd = 1, radius = 2, arc = pi / 6, ec = 1.2,
    resource = c(5, 5), plate_size = 6, capacity = 1000,
    st_birth = 1, birth_rate = 0.5, st_death = 1, death_rate = 0.1,
    birth_move_dist = 0.1,
    DEBUG = FALSE) {

  microbe_table <- list()
  microbe_results <- list()

  title <- sprintf("Age = %s", 0)
  if (is.null(microbe_background)) {
    microbe_background <- list()
  }
  microbe_list <- c(microbe_background, microbe_active)

  # n_background <- length(microbe_background)
  # range_microbe_background <- 1:n_background
  # range_microbe_active <- (n_background + 1):length(microbe_list)

  ids <- sapply(microbe_list, "[[", "id")
  ages <- sapply(microbe_list, "[[", "age")
  microbe_table[[title]] <- data.frame(id = ids, age = ages)
  microbe_results[[title]] <- microbe_list

  for (iter in seq_len(total_iter)) {
    if (DEBUG) {
      print(sprintf("Iteration: %d.  Total: %d. Microbes: %d", iter, total_iter, length(microbe_list)))
    }
    ## Age
    microbe_list <- lapply(microbe_list, age)
    microbe_list <- update_microbe_movement(microbe_list, sd, radius, arc, ec, resource, max_radius = plate_size)
    # microbe_active <- microbe_list[range_microbe_active]
    microbe_list <- update_microbe_birth_death(microbe_list, st_birth, birth_rate, st_death, death_rate, capacity, birth_move_dist = birth_move_dist)
    # microbe_list <- c(microbe_list[range_microbe_background], microbe_active)

    l <- length(microbe_list)
    title <- sprintf("Age = %s", iter)
    if (l == 0) {
      print(sprintf("All microbes die at i th iteration where i = %d", iter))
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


## Single microbes
grid_search_single_microbe <- function(
    total_iter, sample_interval,
    sd = 1, radius = 2, arc = pi / 6, ec = 2,
    birth_rate = 0.5, death_rate = 0.1,
    max_move = 1, birth_move_dist = 0.1,
    capacity = 1000, resource = c(5, 5), plate_size = 6) {

  init_radius <- 0.3
  n_microbes <- 100
  microbe_active <- lapply(seq_len(n_microbes), function(x) {
    background_microbe(speed = runif(1, 0, max_move), location = generate_init_circle(c(0, 0), radius = init_radius))
  })

  results <- real_data_simulation(
    microbe_active = microbe_active, microbe_background = NULL,
    total_iter = total_iter, sample_interval = sample_interval,
    sd = sd, radius = radius, arc = arc, ec = ec,
    resource = resource, capacity = capacity, plate_size = plate_size,
    st_birth = 1, birth_rate = birth_rate, st_death = 1, death_rate = death_rate,
    birth_move_dist = birth_move_dist,
    DEBUG = TRUE
  )

  return(results)
}


simulation_real_data_single <- function() {

  birth_rate <- 0.3
  death_rate <- 0.1
  max_move <- 0.01

  resource <- c(3, 0)
  plate_size <- 6

  total_iter <- 15
  sample_interval <- 1
  capacity <- 10000

  radius <- 2
  arc <- pi / 4
  sd_values <- c(1, 2, 3)
  sd_values <- c(2)
  # sd_values <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)
  sd_values <- c(0.5, 1, 1.5, 2, 3, 4)
  # sd_values <- c(2.5, 3, 3.5, 4, 4.5, 5)
  # ec_values <- c(1, 2)
  ec_values <- c(1)

  param_grid <- expand.grid(sd = sd_values, ec = ec_values)
  param_grid$name <- sprintf("sd=%.1f_ec=%.1f", param_grid$sd, param_grid$ec)

  ncore <- min(nrow(param_grid), ncpus)
  results_list <- mclapply(seq_len(nrow(param_grid)), function(i) {
    # results_list2 <- mclapply(7:9, function(i) {
    sd <- param_grid$sd[i]
    ec <- param_grid$ec[i]
    param_comb <- param_grid$name[i]
    results <- grid_search_single_microbe(
      total_iter = total_iter, sample_interval = sample_interval,
      max_move = max_move, birth_move_dist = max_move*0.1,
      sd = sd, radius = radius, arc = arc, ec = ec,
      birth_rate = birth_rate, death_rate = death_rate,
      capacity = capacity, plate_size = plate_size,
      resource = resource
    )
    list(param_comb = param_comb, results = results)
  }, mc.cores = ncore)


  results_real_data <- setNames(
    lapply(results_list, function(x) x$results),
    sapply(results_list, function(x) x$param_comb)
  )

# saveRDS(results_real_data_sd2, "results_real_data_sd.rds")
# saveRDS(results_real_data, "results_real_data.rds")

}


parse_main_f9 <- function(param) {
  main <- gsub("ec=", "Repulsion=", param)
  main <- gsub("sd=", "Attraction=", main)
  main <- gsub("_", "  ", main)
  return(main)
}

parse_main_f9_iter <- function(iteration) {
  main <- gsub("Age = ([0-9]+)", "Iteration = \\1", iteration)
  iter <- as.numeric(gsub("Age = ([0-9]+)", "\\1", iteration))
  return(list(main = main, iter = iter))
}



# results_real_data <- readRDS("results_real_data.rds")
png("Supplemental_FigureS7_real_full.png", width = 10000, height = 10000, res = 300)

selected_param <- names(results_real_data)
lines <- setNames(seq(1, by = -41, length = length(selected_param)), selected_param)
par(mfrow = c(6, 6), mar = c(5, 1, 2, 1), oma = c(0, 0, 6, 0))

for (param in grep("ec=1.0", names(results_real_data), value = TRUE)) {
  res_each <- results_real_data[[param]]$microbe_results
  for (iteration in names(res_each)) {
    res <- res_each[[iteration]]
    plot_info <- parse_main_f9_iter(iteration)
    if (plot_info[["iter"]] %in% seq(0, 30, by=3)) {
      plot_real_resource(res, plot_info[["main"]], resource = resource, plate_size = plate_size,
        cex.main = 2.5, cex.legend = 2
      )
    }
  }
  main <- parse_main_f9(param)
  title(main, outer = TRUE, line = lines[[param]], cex.main = 3)
}
dev.off()



png("Figure9_real_reduced.png", width = 4000, height = 4000, res = 300)

selected_param <- names(results_real_data)[c(2, 4, 5)]
lines <- setNames(seq(1, by = -32, length = length(selected_param)), selected_param)

par(mfrow = c(3, 3), mar = c(5, 1, 2, 1), oma = c(0, 0, 5, 0))
# par(mfrow = c(3, 3), mar = c(4, 4, 2, 1), mgp = c(2.5, 1, 0))

for (param in selected_param) {
  res_each <- results_real_data[[param]]$microbe_results
  for (iteration in names(res_each)) {
    res <- res_each[[iteration]]
    plot_info <- parse_main_f9_iter(iteration)
    if (plot_info[["iter"]] %in% c(0, 7, 14)) {
      plot_real_resource(res, plot_info[["main"]],
        resource = resource, plate_size = plate_size,
        cex.main = 2, cex.legend = 1.5
      )
    }
  }

  main <- parse_main_f9(param)
  title(main, outer = TRUE, line = lines[[param]], cex.main = 2.5)
}
dev.off()



# selected_param <- names(results_all)[c(2, 4, 6)]
# lines <- setNames(seq(1, by = -32, length = length(selected_param)), selected_param)
# par(mfrow = c(3, 3), mar = c(5, 1, 2, 1), oma = c(0, 0, 5, 0))
# for (param in selected_param) {
#   sub_titles <- names(results_all[[param]])[c(1, 2, 5)]
#   for (title in sub_titles) {
#     main <- gsub(". Repulsion = .+", "", title)
#     plot_loc_two_microbes(results_all[[param]][[title]]$active, results_all[[param]][[title]]$fixed, main, cex.main=2.25, cex.legend=2)
#     # eqscplot
#   }
#   title(gsub("_", "=", param), outer = TRUE, line = lines[[param]], cex.main = 2.5)
# }

#### Two microbes



grid_search_two_microbes <- function(
    total_iter, sample_interval,
    sd = 1, radius = 2, arc = pi / 6, ec = 2,
    birth_rate = 0.5, death_rate = 0.1,
    capacity = 1000, resource = c(5, 5)) {

  init_radius <- 1
  n_microbes <- 50
  microbe_active <- lapply(seq_len(n_microbes), function(x) {
    background_microbe(speed = runif(1, 0, 1), location = generate_init_circle(c(1, 0), radius = init_radius))
  })
  microbe_background <- lapply(seq_len(n_microbes), function(x) {
    new_microbe(speed = runif(1, 0, 1), location = generate_init_circle(c(-1, 0), radius = init_radius))
  })

  results <- real_data_simulation(
    microbe_active = microbe_active, microbe_background = microbe_background,
    total_iter = total_iter, sample_interval = sample_interval,
    sd = sd, radius = radius, arc = arc, ec = ec,
    resource = resource, capacity = capacity,
    st_birth = 1, birth_rate = birth_rate, st_death = 1, death_rate = death_rate
  )

  return(results)
}



simulation_real_data_two_microbes <- function() {
  birth_rate <- 0.75
  death_rate <- 0.1

  resource <- c(5, 0)

  total_iter <- 20
  sample_interval <- 5
  capacity <- 1000

  radius <- 2
  arc <- pi / 4
  sd <- c(1, 1.5, 2)
  ec <- 2

  param_grid <- expand.grid(sd = sd, ec = ec)
  param_grid$name <- sprintf("sd=%.1f_ec=%.1f", param_grid$sd, param_grid$ec)

  ncore <- min(nrow(param_grid), ncpus)
  results_list <- mclapply(seq_len(nrow(param_grid)), function(i) {
    # results_list2 <- mclapply(7:9, function(i) {
    sd <- param_grid$sd[i]
    ec <- param_grid$ec[i]
    param_comb <- param_grid$name[i]
    results <- grid_search_two_microbes(
      total_iter = total_iter, sample_interval = sample_interval,
      sd = sd, radius = radius, arc = arc, ec = ec,
      birth_rate = birth_rate, death_rate = death_rate,
      capacity = capacity, resource = resource
    )
    list(param_comb = param_comb, results = results)
  }, mc.cores = ncore)


  results_real_data <- setNames(
    lapply(results_list, function(x) x$results),
    sapply(results_list, function(x) x$param_comb)
  )

# saveRDS(results_real_data, "results_real_data.rds")

}
