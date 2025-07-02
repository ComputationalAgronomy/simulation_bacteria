test_that("if the resource factor makes sense", {
  back <- background_microbe(speed = runif(1, 0, 1), location = c(0, 0))
  iter <- 1
  angle <- vector("numeric")
  while (iter <= 10000) {
    loc <- move.background(back, c(0), c(0), 0.5, -1, pi / 12, 1, c(0, 1))$location
    angle[iter] <- angle.cal(loc, c(0, 0))
    iter <- iter + 1
  }
  expect_equal(mean(angle, na.rm = T), pi / 2, tolerance = 0.05)
})

test_that("if the density factor makes sense", {
  microbe_list <- lapply(1:3, function(x) {
    new_microbe(speed = runif(1, 0, 1), location = c(1, 1))
  })
  microbe_list[[1]]$location <- c(0, 0)
  microbe_list[[3]]$location <- c(0, 1)
  x <- y <- vector("numeric")
  for (i in 1:length(microbe_list)) {
    x[i] <- microbe_list[[i]]$location[1]
    y[i] <- microbe_list[[i]]$location[2]
  }
  distance_table <- distance.table(x, y)
  angle_table <- angle.table(x, y)
  angle <- vector("numeric")
  iter <- 1
  while (iter <= 10000) {
    angle[iter] <- angle.cal(move.general(microbe_list[[1]], distance_table[1, ], angle_table[1, ],
      sd = NA, 2,
      pi / 12, 1
    )$location, c(0, 0))
    iter <- iter + 1
  }
  expect_equal(c(
    mean(angle <= pi / 3 & angle >= pi / 6, na.rm = T), mean(angle <= 7 * pi / 12 & angle >= 5 * pi / 12, na.rm = T),
    sum(is.na(angle) == T) / 10000, mean(angle <= 10 * pi / 12 & angle >= 8 * pi / 12, na.rm = T)
  ), c(1 / 22, 1 / 22, 1 / 10, 2 / 22), tolerance = 0.2)
})

test_that("if the density factor makes sense", {
  background_list <- lapply(1:3, function(x) {
    background_microbe(speed = runif(1, 0, 1), location = c(1, 1))
  })
  background_list[[1]]$location <- c(0, 0)
  background_list[[3]]$location <- c(0, 1)
  x <- y <- vector("numeric")
  for (i in 1:length(background_list)) {
    x[i] <- background_list[[i]]$location[1]
    y[i] <- background_list[[i]]$location[2]
  }
  distance_table <- distance.table(x, y)
  angle_table <- angle.table(x, y)
  angle <- vector("numeric")
  iter <- 1
  while (iter <= 10000) {
    angle[iter] <- angle.cal(move.background(
      background_list[[1]], distance_table[1, ], angle_table[1, ], 0.5, 2, pi / 12,
      1, c(2, 2)
    )$location, c(0, 0))
    iter <- iter + 1
  }

  expect_equal(c(mean(angle[angle <= pi / 3 & angle >= pi / 6], na.rm = T), sum(is.na(angle) == T) / 10000), c(pi / 4, 0.1), tolerance = 0.05)
})
