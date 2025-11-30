library(ggplot2)
library(RColorBrewer)
library(MASS)

#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggtitle
#' @export
## Get the distribution of all the microbes.
age_print.microbe <- function(m) {
  a <- vector("numeric")
  for (i in 1:length(m)) {
    a[i] <- m[[i]]$age
  }
  b <- data.frame(table(a))
  colnames(b) <- c("age", "value")
  p <- ggplot(data = b, aes(x = age, y = value)) +
    geom_bar(stat = "identity", width = 0.05) +
    ggtitle("Age distribution")
  return(p)
}

## Track the location of an individual.
loc_time_print.microbe <- function(m, id, time) {
  a <- b <- vector("numeric")
  for (i in 1:time) {
    a[i] <- c(m[[i]][[id]]$location[1])
    b[i] <- c(m[[i]][[id]]$location[2])
  }
  return(plot(a, b, main = "age-loc distribution", xlab = "X", ylab = "Y", cex = 0.5, pch = 18))
}


## Get the location of all the microbes.
#' @export
plot_loc <- function(microbes, title, xlim = c(-5, 10), ylim = c(-5, 10), xlab = "", ylab = "", cex = 0.5, pch = 19,
                     xaxt = "n", yaxt = "n", resources = c(5, 5), ...) {
  loc_x <- sapply(microbes, function(m) m$location[1])
  loc_y <- sapply(microbes, function(m) m$location[2])
  # locations <- cbind(loc_x, loc_y)
  cpal <- RColorBrewer::brewer.pal(8, "Dark2")
  plot(loc_x, loc_y,
    main = title, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
    cex = cex, pch = pch, col = cpal[1],
    xaxt = xaxt, yaxt = yaxt, bty = "n", ...
  )
  points(resources[1], resources[2], col = "red", pch = 19, cex = 2)
  legend("topleft", legend = c("Active Microbes", "Resource"), col = c(cpal[1], "red"), pch = 19, cex = 1.5, bty = "n")
}


# Plotting function for population dynamics
#' @export
plot_population <- function(time, population, pop_capacity, res_name) {
  rates <- as.numeric(
    str_split(sub("b=([0-9.]+)_d=([0-9.]+)", "\\1 \\2", res_name), " ")[[1]]
  )
  main <- paste("birth =", rates[1], ", death =", rates[2])
  # main <- gsub("_", ", ", main)
  plot(time, population,
    type = "l", lwd = 2, xlab = "Iteration", ylab = "Population size", ylim = c(0, pop_capacity),
    main = main
  )
}


# Plotting function for two types of microbes.
# Active microbes: Moving
# Background microbes: Stationary
#' @importFrom graphics legend points plot
#' @importFrom MASS eqscplot
#' @export
plot_loc_two_microbes <- function(
    microbe_active, microbe_background,
    main, xlab = "", ylab = "",
    cex = 0.5, pch = 19, xaxt = "n", yaxt = "n",
    cex.legend = 1.5, ...) {
  loc_x <- c(
    sapply(microbe_background, function(m) m$location[1]),
    sapply(microbe_active, function(m) m$location[1])
  )
  loc_y <- c(
    sapply(microbe_background, function(m) m$location[2]),
    sapply(microbe_active, function(m) m$location[2])
  )
  lim_max <- max(abs(loc_x), abs(loc_y))
  # xlim <- range(loc_x)
  # ylim <- range(loc_y)
  bound_lim <- c(-lim_max, lim_max)
  # xlim <- range(loc_x)
  # ylim <- range(loc_y)
  # xlim <- c(-lim_max, lim_max)
  # ylim <- c(-lim_max, lim_max)

  cpal <- RColorBrewer::brewer.pal(8, "Dark2")
  col <- c(rep(cpal[1], length(microbe_background)), rep(cpal[2], length(microbe_active)))
  eqscplot(loc_x, loc_y,
    col = col, main = main,
    xlim = bound_lim, ylim = bound_lim, xlab = xlab, ylab = ylab,
    cex = cex, pch = pch, xaxt = xaxt, yaxt = yaxt,
    bty = "n",
    ...
  )
  # eqscplot
  legend("topleft", legend = c("Active", "Stationary"), col = c(cpal[2], cpal[1]), pch = 19, cex = cex.legend, bty = "n")
}



#' @export
plot_loc_resource <- function(all_microbes, main,
  resource = c(5, 5), boundary = 15,
  cex.legend = 1.5, ...) {
  active <- list()
  general <- list()
  for (m in all_microbes) {
    if ("background" %in% class(m)) {
      active <- c(active, list(m))
    } else {
      general <- c(general, list(m))
    }
  }
  all_microbes <- c(general, active)
  loc_x <- sapply(all_microbes, function(m) m$location[1])
  loc_y <- sapply(all_microbes, function(m) m$location[2])

  cpal <- RColorBrewer::brewer.pal(8, "Dark2")
  bound_lim <- c(-boundary, boundary)
  lim_max <- max(abs(loc_x), abs(loc_y), abs(resource))
  # xlim <- range(loc_x)
  # ylim <- range(loc_y)
  bound_lim <- c(-lim_max, lim_max)
  # ylim <- c(-lim_max, lim_max)

  eqscplot(loc_x, loc_y,
    xlim = bound_lim, ylim = bound_lim,
    col = c(rep(cpal[1], length(general)), rep(cpal[2], length(active))),
    pch = 20, main = main, cex = 0.5,
    xaxt = "n", yaxt = "n", axes = FALSE, xlab = "", ylab = "",
    ...
  )
  points(resource[1], resource[2], col = "red", pch = 19, cex = 1.5)
  legend("topleft",
    legend = c("Background Microbes", "Active Microbes", "Resources"),
    col = c(cpal[1:2], "red"), pch = 19, cex = cex.legend, bty = "n"
  )
}




#' @export
plot_real_resource <- function(all_microbes, main,
  resource = c(3, 0), plate_size = 6,
  cex.legend = 1.5, ...) {
  # active <- list()
  # general <- list()
  # for (m in all_microbes) {
  #   if ("background" %in% class(m)) {
  #     active <- c(active, list(m))
  #   } else {
  #     general <- c(general, list(m))
  #   }
  # }
  # all_microbes <- c(general, active)
  loc_x <- sapply(all_microbes, function(m) m$location[1])
  loc_y <- sapply(all_microbes, function(m) m$location[2])

  cpal <- RColorBrewer::brewer.pal(8, "Dark2")
  # bound_lim <- c(-boundary, boundary)
  # lim_max <- max(abs(loc_x), abs(loc_y), abs(resource))
  # # xlim <- range(loc_x)
  # ylim <- range(loc_y)
  bound_lim <- c(-plate_size, plate_size)
  # ylim <- c(-lim_max, lim_max)

  eqscplot(loc_x, loc_y,
    xlim = bound_lim, ylim = bound_lim,
    col = cpal[2],
    pch = 20, main = main, cex = 0.5,
    xaxt = "n", yaxt = "n", axes = FALSE, xlab = "", ylab = "",
    ...
  )
  points(c(resource[1], -resource[1]), rep(resource[2], 2), col = c("red", "blue"), pch = 19, cex = 1.5)
  points(0, 0, col = "green", pch = 19, cex = 1.5)
  cir_outer <- generate_circle_points(c(0, 0), plate_size, n_points = 720)
  polygon(cir_outer[["x"]], cir_outer[["y"]], border = "black")

  legend("topleft",
    legend = c("Active Microbes", "Resources: PDA", "Water"),
    col = c(cpal[2], "red", "blue"), pch = 19, cex = cex.legend, bty = "n"
  )
}


microbe_generate <- function(n, t, st_birth, birth_rate, st_death, death_rate, capacity) {
  microbe_list <- lapply(1:n, function(x) {
    new_microbe(speed = runif(1, 1, 5), location = c(runif(1, 0, 10), runif(1, 0, 10)))
  })
  id <- old <- x <- y <- parent <- amount <- average_age <- vector("numeric")
  microbe_table <- vector("list")
  iter <- 1
  while (iter <= t) {
    microbe_list <- lapply(microbe_list, age)
    microbe_list <- c(microbe_list, lapply(microbe_list, birth, st_birth, birth_rate, length(microbe_list), capacity))
    microbe_list <- microbe_list[!is.na(microbe_list)]
    logic <- lapply(microbe_list, death, st_death, death_rate, length(microbe_list), capacity)
    survive <- which(logic == TRUE)
    microbe_list <- microbe_list[survive]
    id <- old <- x <- y <- parent <- amount <- average_age <- vector("numeric")
    l <- length(microbe_list)
    if (l == 0) {
      print(paste("All microbes die at i th iteration where i = ", iter))
      break
    } else {
      for (i in 1:l) {
        id[i] <- microbe_list[[i]]$id
        old[i] <- microbe_list[[i]]$age
        x[i] <- microbe_list[[i]]$location[1]
        y[i] <- microbe_list[[i]]$location[2]
        parent[i] <- microbe_list[[i]]$parent
        microbe_table[[iter]] <- data.frame(id, old, x, y, parent)
      }
    }
    iter <- iter + 1
  }
  for (i in 1:length(microbe_table)) {
    amount[i] <- nrow(microbe_table[[i]])
    average_age[i] <- colMeans(microbe_table[[i]][2])
    table <- data.frame(amount, average_age)
  }
  return(table)
}


##  microbe_generate(100,100,10,15,1,1,10,1)
