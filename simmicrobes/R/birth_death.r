#' @importFrom stats dexp runif
#' @export
## "True" means alive, "False" means dead. 
## st: the age microbes begin to die.
## b: the parameter of death rate. Basically, b>=0, the bigger b is, the higher death rate is.
## Due to the model,death rate will never reach 1. 
death.microbe <- function(m,st_death,b){
  if (m$age >= st_death){
  q <- (m$age-st_death)/10
  p <- runif(1)
  rate <- -dexp(q,b)/b+1
  if(rate < p){
    return("True")
  }else{
    return("False")
  }
  }else{
   return("True")
 }
}

#' @export
death <- function(m,st_death,b){
  UseMethod("death")
}

#' @export

## Assume birth rate fits a normal distribution.
## p is a constant which duplicates the birth rate p times.
## st is the age that microbes start to give birth.
## mean and sd are the parameters determine the figure of the normal distribution.
## Since there are so many parameters, it' recommended to fix mean = 0, sd = 0.5, p = 1 for a specific test.

birth.microbe <- function(m,p,st_birth,mean,sd){
  a <- m$age
  b <- runif(1,0,1)
  rate <- p*dnorm((a-st_birth)/10,mean,sd)
  if(a >= st_birth){
  if(rate > b){
    n <- new_microbe(speed = runif(1, 1,5), location=c(m$location[1]+1,m$location[2]+1),parent = m$id) 
  }else{
    n <- NA
  }
  }else{
    n <- NA
  }
  return(n)
}

#' @export
birth <- function(m,p,st_birth,mean,sd){
  UseMethod("birth")
} 
## Below is the first used model,feel free to use it again. 
#birth.microbe <- function(m){
#  q <- m$age
#  p <- runif(1)
#  if(10 < q && q <= 15){
#  rate <- 1.5*(q-10)/10
#  if(rate > p){
#    n <- new_microbe(speed = runif(1, 1,5), location=c(m$location[1]+1,m$location[2]+1),parent = m$id)
#  }else{
#    n <- NA
#  }
#  }else if(15 < q && q <= 20){
#    rate <- 0.75
#    if(rate > p){
#      n <- new_microbe(speed = runif(1, 1,5), location=c(m$location[1]+1,m$location[2]+1),parent = m$id)
#    }else{
#      n <- NA
#    }
#  }else if(20 < q && q<= 30){
#    rate <- 0.75*(30-q)/10
#    if(rate > p){
#      n <- new_microbe(speed = runif(1, 1,5), location=c(m$location[1]+1,m$location[2]+1),parent = m$id)
#    }else{
#      n <- NA
#    }
#  }else{
#    n <- NA
#  }
#  return(n)
#}