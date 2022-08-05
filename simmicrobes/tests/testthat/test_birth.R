## The below is a string of tests on the birth function.
## Generally, according to the birth function, the birth rate is 0 when age is under 10 or above 30,
## fits 0.15*(age-10) between 11-15, stays 0.75 between 16-20, fit 0.075*(30-age) between 21-30.
## We give a test for each age period, with 10 individuals at a same age. 

#' @importFrom stats runif
#' @export

## Test under age 10, of which the birth rate should always be 0. 
## ex: Take age 10 for test here. 
test_that("if birth rate is 0 when age is under 10",{
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
  i <- 1
  while(i < 11){
    microbe_list <- lapply(microbe_list, age)
    i <- i+1
  }
  expect_equal(sum(!is.na(lapply(microbe_list,birth))),0)
})

## Test age 11-15, of which the birth rate should fit y = 0.15(x-10).
## Ten individuals are tested here.
## Bias within 0.05 is accepted. 
## ex: Take age 11 for test here. 
test_that("if the birth rate fit y = 1.5(x-10) between 11-15",{
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
  i <- 1
  while(i < 12){
    microbe_list <- lapply(microbe_list, age)
    i <- i+1
  }
  birth_rate <- mean(replicate(1000,sum(!is.na(lapply(microbe_list,birth)))))
  expect_equal(birth_rate,0.15*10*(11-10),tolerance = 0.05)
})  

## Test age 16-20, of which the birth rate should be 0.75.
## Ten individuals are tested here.
## Bias within 0.05 is accepted. 
## ex: Take age 17 for test here. 
test_that("if the birth rate is 0.75 between age 16-20",{
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
  i <- 1
  while(i < 18){
    microbe_list <- lapply(microbe_list, age)
    i <- i+1
  }
  birth_rate <- mean(replicate(1000,sum(!is.na(lapply(microbe_list,birth)))))
  expect_equal(birth_rate,0.75*10,tolerance = 0.05)
})  

## Test age 21-30, of which the birth rate should fit y = 0.075*(30-x).
## Ten individuals are tested here.
## Bias within 0.05 is accepted. 
## ex: Take age 24 for test here. 
test_that("if the birth rate fit y = 0.075(30-x) between age 21-30",{
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
  i <- 1
  while(i < 25){
    microbe_list <- lapply(microbe_list, age)
    i <- i+1
  }
  birth_rate <- mean(replicate(1000,sum(!is.na(lapply(microbe_list,birth)))))
  expect_equal(birth_rate,0.075*10*(30-24),tolerance = 0.05)
}) 

## Test above age 30, of which the birth rate should always be 0. 
## Ten individuals are tested here.
## Bias within 0.05 is accepted.
## ex: Take age 30 for test here. 
test_that("if the birth rate is 0 above age 30",{
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
  i <- 1
  while(i < 31){
    microbe_list <- lapply(microbe_list, age)
    i <- i+1
  }
  expect_equal(sum(!is.na(lapply(microbe_list,birth))),0)
}) 