## The below is a string of tests on the birth function.
## Since there are 4 parameters, it's fair to fix all of them for test.
## set p = 1,st_birth = 10, mean = 0 , sd = 1 for ex. Feel free to change parameters for extra test.

#' @importFrom stats runif
#' @export

## Since the age that microbes begin to give birth is 0, the birth rate should be 0 before age 0.
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
  expect_equal(sum(!is.na(lapply(microbe_list,birth,p=1,st_birth=10,mean=0,sd=1))),0)
})

## After age 10, the birth rate fit a standard normal distribution.
## Since it's N(0,1), it actually fits the right half of a standard normal distribution, with peak at 0 (age 10).
## ex:Take age 13 here.
test_that("if the birth rate fit the noraml distribution above age 10",{
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
  i <- 1
  while(i < 14){
    microbe_list <- lapply(microbe_list,age)
    i <- i+1
  }
  birth_rate <- mean(replicate(1000,sum(!is.na(lapply(microbe_list,birth,p=1,st_birth=10,mean=0,sd=1)))))/10
  expect_equal(birth_rate,dnorm(0.3,0,1),tolerance = 0.05)
})