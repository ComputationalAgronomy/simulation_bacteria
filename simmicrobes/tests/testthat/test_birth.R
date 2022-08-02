microbe_list <- lapply(1:5, function(x){
    new_microbe(speed = runif(1, 1, 5), location=c(0,0))
})



## test on models
# library(testthat)

## test when age is under 11 or above 30, the birth rate is supposed to be 0.
test_under_11 <- function(m){
  test_that("if birth rate is 0 when age is under 10 or above 30",{
  expect_equal(sum(!is.na(lapply(m,birth))),0)
})
}

## Test when age is between 11-15, the linear model here is supposed to fit y = 15(x-10). 
## Take as at age 11 here. 
## Make sure initial age is 10-14. The model is proved successful here. 

test_11_15 <- function(m,age){
birth_rate <- round(mean(replicate(1000,sum(!is.na(lapply(m,birth)))))) 
test_that("if the birth rate fit y = 15(x-10) between 11-15",{
  expect_equal(birth_rate,15*(age-10))
})
}
## Similar as the former one, test 16-20, of which the birth rate is supposed to stay unchanged. 
##The initial age is 15. 
test_16_20 <- function(m){
birth_rate <- round(mean(replicate(1000,sum(!is.na(lapply(m,birth)))))) 
test_that("if the birth rate stays between age 16-20",{
  expect_equal(birth_rate_16,15*(15-10))
})
}

## Similar, test 21-30. linear model is 7.5*(30-age)
test_21_30 <- function(m,age){
birth_rate <- signif(mean(replicate(1000,sum(!is.na(lapply(m,birth))))),3)
test_that("if the birth rate decreases linearly between age 21-30",{
  expect_equal(birth_rate,7.5*(30-age),tolerance = 0.2)
})
}

## aggregate the test_birth function, which could be only used when age is straightly the same along the list.
test_birth <- function(m,age){
  if(age<=10 && age>30){
    test_that("if birth rate is 0 when age is under 10 or above 30",{
      expect_equal(sum(!is.na(lapply(m,birth))),0)
    })
  }
  if(age>=11 && age<=15){
    birth_rate <- round(mean(replicate(1000,sum(!is.na(lapply(m,birth)))))) 
    test_that("if the birth rate fit y = 15(x-10) between 11-15",{
      expect_equal(birth_rate,15*(age-10))
    })
  }
  if(age>=16 && age<=20){
    birth_rate <- round(mean(replicate(1000,sum(!is.na(lapply(m,birth)))))) 
    test_that("if the birth rate stays between age 16-20",{
      expect_equal(birth_rate,15*(15-10))
    })
  }
  if(age>=21 && age<=30){
    birth_rate <- signif(mean(replicate(1000,sum(!is.na(lapply(m,birth))))),3)
    test_that("if the birth rate decreases linearly between age 21-30",{
     expect_equal(birth_rate,7.5*(30-age),tolerance = 0.05)
    })
  }
}
# test_birth(microbe_list,14)

