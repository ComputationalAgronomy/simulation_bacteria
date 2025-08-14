library(stringr)
library(devtools)
library(parallel)
load_all("simmicrobes")
source("simulation/simulation_init.R")
source("simulation/update_microbe.R")

ncpus <- min(12, floor(detectCores() * 0.8))
options(mc.cores = ncpus)


## Fix the parameters of birth and death rate function, basically, nine sets are used: low, medium, high birth rate
## and low, medium,high death rate.
## Initial population and iterations are fixed.
## Ignore movement here.
update_microbe_list <- function(n, t, st_birth, birth_rate, st_death, death_rate, capacity) {
  microbe_list <- lapply(1:n, function(x) {
    new_microbe(speed = runif(1, 1, 5), location = c(0, 0))
  })
  # id <- old <- amount <- average_age <- vector("numeric")
  microbe_table <- vector("list")
  iter <- 1
  while (iter <= t) {
    microbe_list <- lapply(microbe_list, age)
    microbe_list <- update_microbe_birth_death(microbe_list, st_birth, birth_rate, st_death, death_rate, capacity)
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
    if (l == 0) {
      print(paste("All microbes die at i th iteration where i = ", iter))
      break
    } else {
      ids <- sapply(microbe_list, "[[", "id")
      ages <- sapply(microbe_list, "[[", "age")
      microbe_table[[iter]] <- data.frame(id = ids, age = ages)
    }
    iter <- iter + 1
  }
  amount <- average_age <- vector("numeric")
  for (i in seq_along(microbe_table)) {
    amount[i] <- NROW(microbe_table[[i]])
    average_age[i] <- colMeans(microbe_table[[i]][2])
    table <- data.frame(amount, average_age)
  }
  return(table)
}


## n = initial population
## t = iteration(age)
## r = replication
grid_search_rates <- function(init_n, total_iter, birth_rates = c(0.1, 0.5, 0.9), death_rates = c(0.1, 0.5, 0.9),
                              st_birth = 1, st_death = 1, pop_capacity = 1000, ncpus = getOption("mc.cores", 8L)) {
  results <- list()
  param_grid <- expand.grid(birth_rate = birth_rates, death_rate = death_rates)
  results_list <- mclapply(
    seq_len(nrow(param_grid)),
    function(x) {
      b <- param_grid[x, "birth_rate"]
      d <- param_grid[x, "death_rate"]
      res <- update_microbe_list(init_n, total_iter, st_birth, b, st_death, d, pop_capacity)
      list(name = sprintf("b=%.2f_d=%.2f", b, d), result = res)
    },
    mc.cores = ncpus
  )
  for (i in results_list) {
    results[[i$name]] <- i$result
  }
  # for (b in birth_rates) {
  #   for (d in death_rates) {
  #     res <- update_microbe_list(init_n, total_iter, st_birth, b, st_death, d, pop_capacity, ncpus = ncpus)
  #     results[[sprintf("b=%.1f_d=%.1f", b, d)]] <- res
  #   }
  # }
  return(results)
}


pop_capacity <- 10000
rates <- c(0.25, 0.5, 0.75)
rates <- c(0.1, 0.5, 0.9)
rates <- c(0.5, 1, 10, 50)
rates <- c(0.05, 0.25, 0.5, 0.75, 0.95)
rates <- c(0.1, 0.3, 0.7, 0.9) # MS Figure 4

system.time({
  results <- grid_search_rates(
    init_n = 10, total_iter = 100,
    birth_rates = rates, death_rates = rates, pop_capacity = pop_capacity,
    ncpus = ncpus
  )
})


png("Figure4_birth_death_coef.png", width = 4000, height = 4000, res = 300)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1), mgp = c(2.5, 1, 0))
par(mfrow = c(4, 4), mar = c(4, 4, 2, 1), mgp = c(2.5, 1, 0))

fig_each <- ceiling(sqrt(length(results)))
par(mfrow = c(fig_each, fig_each), mar = c(4, 4, 2, 1), mgp = c(2.5, 1, 0))
for (name in names(results)) {
  res <- results[[name]]
  plot_population(seq_len(NROW(res)), res[, 1], pop_capacity = pop_capacity, res_name = name)
}
dev.off()




#########
pal8 <- RColorBrewer::brewer.pal(8, "Dark2")
generate_logistic_data <- function(total_iter, birth_rate, capacity, init_n) {
  range_iter <- c(0, seq_len(total_iter))
  theoretical_pop <- capacity / (1 + ((capacity - init_n) / init_n) * exp(-birth_rate * range_iter))
  simulated <- mclapply(1:10, function(i) {
    mt <- update_microbe_list(init_n, total_iter, 0, birth_rate, 0, 0, capacity = capacity)
    c(init_n, mt[, 1])
  }, mc.cores = getOption("mc.cores", 10L))
  list(time = range_iter, birth_rate = birth_rate, capacity = capacity, theoretical = theoretical_pop, simulated = simulated)
}

plot_logistic_curve <- function(sim_data) {
  pal8 <- RColorBrewer::brewer.pal(8, "Dark2")
  pal_set2 <- RColorBrewer::brewer.pal(8, "Set2")
  plot(NA,
    type = "n", xlim = range(sim_data$time), ylim = c(0, sim_data$capacity),
    xlab = "Iteration", ylab = "Population Size", main = sprintf("Birth = %.2f", sim_data$birth_rate)
  )
  for (sim in sim_data$simulated) {
    lines(sim_data$time, sim, type = "l", lty = 1, lwd = 1, col = pal_set2[1])
  }
  lines(sim_data$time, sim_data$theoretical, type = "l", lty = 1, lwd = 2, col = pal8[4])
  legend("bottomright", legend = c("Theoretical", "Simulated"), col = c(pal8[4], pal_set2[1]), bty = "n", lty = 1)
}

rates <- c(0.05, 0.1, 0.2, 0.3, 0.5, 0.95)
results <- list()
for (b in rates) {
  iterations <- 50
  if (b <= 0.1) {
    iterations <- 100
  }
  results[[sprintf("b=%.2f", b)]] <- generate_logistic_data(iterations, b, 1000, 100)
}

png("FigureS1_logistic_growth.png", width = 3000, height = 2000, res = 300)
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1), mgp = c(2.5, 1, 0))
for (sim_data in results) {
  plot_logistic_curve(sim_data)
}
dev.off()


sim_data <- generate_logistic_data(100, 0.05, 1000, 100)
#########################



Lag_test <- function(n, t = 30) {
  m1 <- update_microbe_list(n, t, 1, 0.5, 1, 0.5, pop_capacity)
  plot(1:t, m1[, 1], type = "l", lwd = 2, xlab = "Time", ylab = "Population size", ylim = c(100, pop_capacity))
  m2 <- update_microbe_list(n, t, 2, 0.5, 1, 0.5, pop_capacity)
  lines(1:30, m2[, 1], lwd = 2, col = "blue")
  m3 <- update_microbe_list(n, t, 1, 0.5, 2, 0.5, pop_capacity)
  lines(1:30, m3[, 1], lwd = 2, col = "red")
  legend("topleft", legend = c("Normal", "Lag in birth", "Lag in death"), lty = c(1, 1, 1), col = c("black", "blue", "red"))
}

Real_curve <- function(n, t1 = 40, t2 = 60, capacity = 1000) {
  microbe_list <- lapply(1:n, function(x) {
    new_microbe(speed = runif(1, 1, 5), location = c(0, 0))
  })
  l <- c()
  iter <- 1
  while (iter <= t1) {
    microbe_list <- lapply(microbe_list, age)
    logic <- lapply(microbe_list, death, 1, 0, length(microbe_list), 1000)
    survive <- which(logic == TRUE)
    microbe_list <- microbe_list[survive]
    microbe_list <- c(microbe_list, lapply(microbe_list, birth, 1, 0.5, length(microbe_list), 1000))
    microbe_list <- microbe_list[!is.na(microbe_list)]
    l[iter] <- length(microbe_list)
    iter <- iter + 1
  }
  while (iter <= t2) {
    microbe_list <- lapply(microbe_list, age)
    logic <- lapply(microbe_list, death, 1, 0.5, length(microbe_list), 1000)
    survive <- which(logic == TRUE)
    microbe_list <- microbe_list[survive]
    if (length(microbe_list) == 0) {
      print(paste("All microbes die at i th iteration where i = ", iter))
      break
    }
    l[iter] <- length(microbe_list)
    iter <- iter + 1
  }
  return(plot(1:t2, l,
    type = "l", lwd = 2, xlab = "Time", ylab = "Population size",
    ylim = c(100, 1000), main = "Simulated curve"
  ))
}





##################################


# @Deprecated
amount_test <- function(init_n, total_iter) {
  pop_capacity <- 1000
  par(mfrow = c(3, 3))
  t11 <- update_microbe_list(init_n, total_iter, 1, 0.1, 1, 0.1, pop_capacity)
  plot(1:total_iter, t11[, 1],
    type = "l", lwd = 2, xlab = "Time", ylab = "Population size", ylim = c(100, pop_capacity),
    main = "b = 0.1, d = 0.1"
  )
  t12 <- update_microbe_list(init_n, total_iter, 1, 0.5, 1, 0.1, pop_capacity)
  plot(1:total_iter, t12[, 1],
    type = "l", lwd = 2, xlab = "Time", ylab = "Population size", ylim = c(100, pop_capacity),
    main = "b = 0.5, d = 0.1"
  )
  t13 <- update_microbe_list(init_n, total_iter, 1, 0.9, 1, 0.1, pop_capacity)
  plot(1:total_iter, t13[, 1],
    type = "l", lwd = 2, xlab = "Time", ylab = "Population size", ylim = c(100, pop_capacity),
    main = "b = 0.9, d = 0.1"
  )
  t21 <- update_microbe_list(init_n, total_iter, 1, 0.1, 1, 0.5, pop_capacity)
  plot(1:total_iter, t21[, 1],
    type = "l", lwd = 2, xlab = "Time", ylab = "Population size", ylim = c(100, pop_capacity),
    main = "b = 0.1, d = 0.5"
  )
  t22 <- update_microbe_list(init_n, total_iter, 1, 0.5, 1, 0.5, pop_capacity)
  plot(1:total_iter, t22[, 1],
    type = "l", lwd = 2, xlab = "Time", ylab = "Population size", ylim = c(100, pop_capacity),
    main = "b = 0.5, d = 0.5"
  )
  t23 <- update_microbe_list(init_n, total_iter, 1, 0.9, 1, 0.5, pop_capacity)
  plot(1:total_iter, t23[, 1],
    type = "l", lwd = 2, xlab = "Time", ylab = "Population size", ylim = c(100, pop_capacity),
    main = "b = 0.9, d = 0.5"
  )
  t31 <- update_microbe_list(init_n, total_iter, 1, 0.1, 1, 0.9, pop_capacity)
  plot(1:total_iter, t31[, 1],
    type = "l", lwd = 2, xlab = "Time", ylab = "Population size", ylim = c(100, pop_capacity),
    main = "b = 0.1, d = 0.9"
  )
  t32 <- update_microbe_list(init_n, total_iter, 1, 0.5, 1, 0.9, pop_capacity)
  plot(1:total_iter, t32[, 1],
    type = "l", lwd = 2, xlab = "Time", ylab = "Population size", ylim = c(100, pop_capacity),
    main = "b = 0.5, d = 0.9"
  )
  t33 <- update_microbe_list(init_n, total_iter, 1, 0.9, 1, 0.9, pop_capacity)
  plot(1:total_iter, t33[, 1],
    type = "l", lwd = 2, xlab = "Time", ylab = "Population size", ylim = c(100, pop_capacity),
    main = "b = 0.9, d = 0.9"
  )
  par(mfrow = c(1, 1))
}

amount_test(100, 100)
