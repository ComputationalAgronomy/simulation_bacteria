library(ggplot2)
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
  plot(loc_x, loc_y,
    main = title, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, cex = cex, pch = pch,
    xaxt = xaxt, yaxt = yaxt, ...
  )
  points(resources[1], resources[2], col = "red", pch = 19, cex = 2)
}


#' @export
plot_loc_two_microbes <- function(microbe_active, microbe_background, main, xlab = "", ylab = "", cex = 0.5, pch = 19, xaxt = "n", yaxt = "n", ...) {
  # a <- b <- vector("numeric")
  # for (i in 1:length(microbe_active)) {
  #   a[i] <- c(microbe_active[[i]]$location[1])
  #   b[i] <- c(microbe_active[[i]]$location[2])
  # }
  # c <- d <- vector("numeric")
  # for (i in 1:length(microbe_background)) {
  #   c[i] <- c(microbe_background[[i]]$location[1])
  #   d[i] <- c(microbe_background[[i]]$location[2])
  # }
  # loc_x_a <- sapply(microbe_active, function(m) m$location[1])
  # loc_y_a <- sapply(microbe_active, function(m) m$location[2])
  # loc_x_b <- sapply(microbe_background, function(m) m$location[1])
  # loc_y_b <- sapply(microbe_background, function(m) m$location[2])
  loc_x <- c(
    sapply(microbe_active, function(m) m$location[1]),
    sapply(microbe_background, function(m) m$location[1])
  )
  loc_y <- c(
    sapply(microbe_active, function(m) m$location[2]),
    sapply(microbe_background, function(m) m$location[2])
  )
  xlim <- range(loc_x)
  ylim <- range(loc_y)

  cpal <- RColorBrewer::brewer.pal(8, "Dark2")
  col <- c(rep(cpal[2], length(microbe_active)), rep("black", length(microbe_background)))
  eqscplot(loc_x, loc_y, col = col, main = main, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, cex = cex, pch = pch, xaxt = xaxt, yaxt = yaxt, ...)
  # eqscplot
  legend("topright", legend = c("Active", "Stationary"), col = c(cpal[2], "black"), pch = 19, cex = 0.8, bty = "n")
}



#' @export
plot_loc_resource <- function(all_microbes, main_title, resource = c(5, 5)) {
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
