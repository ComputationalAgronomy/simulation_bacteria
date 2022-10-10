## The below is a string of tests on the death function.
## Generally, according to the death function, the death rate is 0 under st_death, 
## fits -dexp(age,b)/b+1 after the age of st_death.
## set st_death = 30 , b = 2 as example here.
## We give a test for each age period, with 10 individuals at a same age. 
## According to the model, the death rate would approach 1 but never reach 1.

#' @importFrom stats runif
#' @export

## Test under age 30, the death rate should be 0. 
## ex: Take age 29 for test here.
test_that("if the death rate is 0 under age 50",{
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
  i <- 1
  while(i < 30){
    microbe_list <- lapply(microbe_list,age)
    i <- i+1
  }
  logic <- lapply(microbe_list,death,st_death=30,d=1)
  expect_equal(sum(logic == "False"),0)
})

## Test above age 30, the death rate should fit y = -dexp(age,2)+2.
## Ten individuals are tested here.
## Bias within 0.05 is accepted. 
## ex: Take age 35 for test here. 
test_that("if the death rate fits -dexp(q,2)/2+1 model", {
  age <- 35
  q <- (age - 30) / 10
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
  iter <- 1
  while(iter < 36){
    microbe_list <- lapply(microbe_list,age)
    iter <- iter+1
  }
  i <- 1
  a <- vector("numeric")
  while (i <= 1000) {
    logic <- lapply(microbe_list, death,st_death=30,d=2)
    a[i] <- sum(logic == "False")
    i <- i + 1
  }
  death_rate <- mean(a)/10
  
  expect_equal(death_rate, -dexp(q,2)/2+1, tolerance = 0.05)
})