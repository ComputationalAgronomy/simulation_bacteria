## The below is a string of tests on the birth function.
#' @importFrom stats runif
#' @export

## Suppose the age that microbes begin to give birth is 10, the birth rate should be 0 before age 10.
test_that("if the birth rate is 0 under age 10",{
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
  i <- 1
  while(i < 10){
    microbe_list <- lapply(microbe_list,age)
    i <- i+1
  }
  expect_equal(sum(!is.na(lapply(microbe_list,birth,st_birth=10,birth_rate = 0.8))),0)
})


test_that("if the birth rate fits the model",{
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
  })
  i <- 1
  while(i < 16){
    microbe_list <- lapply(microbe_list,age)
    i <- i+1
  }
  rate <- mean(replicate(1000,sum(!is.na(lapply(microbe_list,birth,st_birth=10,birth_rate=0.8,10,20)))))
  expect_equal(rate,4,tolerance = 0.05)
})