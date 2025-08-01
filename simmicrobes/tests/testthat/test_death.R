#' @importFrom stats runif
#' @export

## Test under age 30, the death rate should be 0.
## ex: Take age 29 for test here.
test_that("if the death rate is 0 under age 30", {
  microbe_list <- lapply(1:10, function(x) {
    new_microbe(speed = runif(1, 1, 5), location = c(0, 0))
  })
  i <- 1
  while (i < 30) {
    microbe_list <- lapply(microbe_list, age)
    i <- i + 1
  }
  logic <- lapply(microbe_list, death, st_death = 30, death_rate = 1)
  expect_equal(sum(logic == FALSE), 0)
})


test_that("if the death rate fits the model", {
  microbe_list <- lapply(1:10, function(x) {
    new_microbe(speed = runif(1, 1, 5), location = c(0, 0))
  })
  iter <- 1
  while (iter < 36) {
    microbe_list <- lapply(microbe_list, age)
    iter <- iter + 1
  }
  i <- 1
  a <- vector("numeric")
  while (i <= 1000) {
    logic <- lapply(microbe_list, death, st_death = 30, 0.8, 10, 40)
    a[i] <- sum(logic == FALSE)
    i <- i + 1
  }
  death_rate <- mean(a) / 10

  expect_equal(death_rate, 0.2, tolerance = 0.05)
})
