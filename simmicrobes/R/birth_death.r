#' @importFrom stats dexp runif
#' @export
## "True" means alive, "False" means dead. 
## st_death: the age microbes begin to die.
## d_r: the parameter of death rate. d_r > 0, the smaller d_r, the higher death rate is.
## Due to the model,death rate will never reach 1. 
death.microbe <- function(m,st_death,death_rate){
  if (m$age > st_death){
  p <- runif(1,0,1)
  rate <- exp(-death_rate/(m$age-st_death))
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
death <- function(m,st_death,death_rate){
  UseMethod("death")
}

#' @export

## Assume birth rate is constant after an age. 
## st_birth is the age that microbes start to give birth.
## parameter b_r controls the increasing rate.
birth.microbe <- function(m,st_birth,birth_rate){
    if(m$age < st_birth){
      n <- NA
    }else{
    p <- runif(1,0,1)
    if(birth_rate > p){
      n <- new_microbe(speed = runif(1,0,1), location=c(m$location[1]+0.1,m$location[2]+0.1),parent = m$id)
    }else{
      n <- NA
    }
    }
  return(n)
}
  
  

#' @export
birth <- function(m,st_birth,birth_rate){
  UseMethod("birth")
} 