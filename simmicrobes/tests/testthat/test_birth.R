## The below is a string of tests on the birth function.
## Since there are 4 parameters, it's fair to fix all of them for test.
## set st_birth = 10, turning point = 20, b = 3, c = 2 for ex. Feel free to change parameters for extra test.

#' @importFrom stats runif
#' @export

## Since the age that microbes begin to give birth is 10, the birth rate should be 0 before age 10.
## ex:Take age 9 here.
test_that("if the birth rate is 0 under age 10",{
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
  i <- 1
  while(i < 10){
    microbe_list <- lapply(microbe_list,age)
    i <- i+1
  }
  expect_equal(sum(!is.na(lapply(microbe_list,birth,st_birth=10,turning_point=20,b=1,c=1))),0)
})

## After age 10, the birth rate fit a two-steps exponential distribution.
## Before the turning point, the birth rate should fits y = -exp(3)/3+1
## ex:Take age 15 here.
test_that("if the birth rate fits the exponential distribution after age 10",{
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
  i <- 1
  while(i < 16){
    microbe_list <- lapply(microbe_list,age)
    i <- i+1
  }
  birth_rate <- mean(replicate(1000,sum(!is.na(lapply(microbe_list,birth,st_birth=10,turning_point=20,b=3,c=2)))))/10
  expect_equal(birth_rate,-dexp(0.5,3)/3+1,tolerance = 0.05)
})

## After age 10, the birth rate fit a two-steps exponential distribution.
## After the turning point, the birth rate should fits y = exp(2)/2
## ex:Take age 25 here.
test_that("if the birth rate fits the exponential distribution after age 20",{
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
})
  i <- 1
  while(i < 26){
    microbe_list <- lapply(microbe_list,age)
    i <- i+1
  }
  birth_rate <- mean(replicate(1000,sum(!is.na(lapply(microbe_list,birth,st_birth=10,turning_point=20,b=1,c=2)))))/10
  a <- (20-1-10)/10
  k <- -dexp(a,1)/1+1
  expect_equal(birth_rate,k*dexp(0.5,2)/2,tolerance = 0.05)
})