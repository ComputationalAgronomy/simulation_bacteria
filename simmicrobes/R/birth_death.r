#' @importFrom stats dexp runif
#' @export
## "True" means alive, "False" means dead. 
## st_death: the age microbes begin to die.
## b: the parameter of death rate. Basically, d>0, the bigger d is, the higher death rate is.
## Due to the model,death rate will never reach 1. 
death.microbe <- function(m,st_death,d){
  if (m$age >= st_death){
  q <- (m$age-st_death)/10
  p <- runif(1,0,1)
  rate <- -dexp(q,d)/d+1
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
death <- function(m,st_death,d){
  UseMethod("death")
}

#' @export

## Assume birth rate fits two-steps exponential distribution with a turning point.
## st_birth is the age that microbes start to give birth.
## st_birth is supposed to be smaller than turning_point, see validation for details.
## The birth rate increases before the turning point and decreases after the turning point.
## parameter b controls the increasing rate, while c controls the decreasing rate.

birth.microbe <- function(m,st_birth,turning_point,b,c){
  if(st_birth > turning_point){
    stop(
      "The age to start giving birth should be smaller than the turning point",
      call. = FALSE
    )
  }
  if(m$age < st_birth){
    n <- NA
  }else if(m$age < turning_point){
    q <- (m$age-st_birth)/10
    p <- runif(1,0,1)
    rate <- -dexp(q,b)/b+1
    if(rate > p){
      n <- new_microbe(speed = runif(1, 1,5), location=c(m$location[1]+1,m$location[2]+1),parent = m$id)
    }else{
      n <- NA
    }
  }else{
    a <- (turning_point-1-st_birth)/10
    k <- (-dexp(a,b)/b+1)
    q <- (m$age-turning_point)/10
    p <- runif(1,0,1)
    rate <- k*dexp(q,c)/c
    if(rate > p){
      n <- new_microbe(speed = runif(1, 1,5), location=c(m$location[1],m$location[2]),parent = m$id)
    }else{
      n <- NA
    }
  }
  return(n)
}
  
  

#' @export
birth <- function(m,st_birth,turning_point,b,c){
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