microbe_helper <- function(n=5){
  lapply(1:n, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
}


test_that("if the death rate fits -dexp(q,1)+1 model", {
  age <- 51
  microbe_list <- microbe_helper(5)
  i <- 1
  a <- vector("numeric")
  while (i <= 1000) {
    logic <- lapply(microbe_list, death)
    a[i] <- -sum(logic == "False") / 100 + 1
    i <- i + 1
  }
  death_rate <- mean(a)
  q <- (age - 50) / 10

  expect_equal(death_rate, -dexp(q, 1) + 1, tolerance = 0.05)
  
})

