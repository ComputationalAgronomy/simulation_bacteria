## The below is a string of tests on the death function.
## Generally, according to the death function, the death rate is 0 under age 50, 
## fits -dexp(age,1)+1 above age 50.
## We give a test for each age period, with 10 individuals at a same age. 
## According to the model, the death rate would approach 1 but never reach 1.

#' @importFrom stats runif
#' @export

## Test under age 50, the death rate should be 0. 
## ex: Take age 49 for test here.
test_that("if the death rate is 0 under age 50",{
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
  i <- 1
  while(i < 50){
    microbe_list <- lapply(microbe_list,age)
    i <- i+1
  }
  logic <- lapply(microbe_list,death)
  expect_equal(sum(logic == "False"),0)
})

## Test above age 50, the death rate should fit y = -dexp(age,1)+1.
## Ten individuals are tested here.
## Bias within 0.1 is accepted. 
## ex: Take age 55 for test here. 
test_that("if the death rate fits -dexp(q,1)+1 model", {
  age <- 55
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
  iter <- 1
  while(iter< 56){
    microbe_list <- lapply(microbe_list,age)
    iter <- iter+1
  }
  i <- 1
  a <- vector("numeric")
  while (i <= 1000) {
    logic <- lapply(microbe_list, death)
    a[i] <- sum(logic == "False")
    i <- i + 1
  }
  death_rate <- mean(a)/10
  q <- (age - 50) / 10

  expect_equal(death_rate, -dexp(q, 1) + 1, tolerance = 0.05)
})