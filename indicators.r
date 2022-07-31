summary.microbe <- function(m){
  output <- sprintf("ID: %d\tAge: %d\tSpeed: %.01f\tLoc: %.04f, %.04f\n", m$id, m$age, m$speed, m$location[1], m$location[2])
  return(cat(output))
}


age_print.microbe <- function(m,n){
  a <- vector("numeric")
  for (i in 1:n){
    a[i] <- m[[i]]$age
  }
  return(hist(a,xlab = "age"))
} 


loc_print.microbe <- function(m,n){
  a <- b <- vector("numeric")
  for(i in 1:n){
    a[i] <- c(m[[i]]$location[1])
    b[i] <- c(m[[i]]$location[2])
    c <- cbind(a,b)
  }
  return(plot(c,main="all_microbe distribution",xlab="X",ylab="Y",cex=0.5,pch=18))
} 


loc_time_print.microbe <- function(m,id,t){
  a <- b <- vector("numeric")
  for(i in 1:t){
    a[i] <- c(m[[i]][[id]]$location[1])
    b[i] <- c(m[[i]][[id]]$location[2])
    c <- cbind(a,b)
  } 
  return(plot(c,main="age-loc distribution",xlab="X",ylab="Y",cex=0.5,pch=18))
} 

## test on models
library(testthat)

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
test_birth(microbe_list,20)


## test death rate
## test under age 50

test_death <- function(m,age){
  i <- 1
  a <- vector("numeric")
  while(i <= 1000){
    logic <- lapply(m, death)
    a[i] <- -sum(logic == "False")/100+1
    i <- i+1
  }
  death_rate <- mean(a)
  q <- (age-50)/10
  test_that("if the death rate fits -dexp(q,1)+1 model",{
    expect_equal(death_rate,-dexp(q,1)+1,tolerance = 0.05)
  })
}

test_death(microbe_list,52)



