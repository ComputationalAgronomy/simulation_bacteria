#' @importFrom stats dexp runif
#' @export
## "True" means alive, "False" means dead. 
## birth_rate:intrinsic growth rate of an agent
## population: number of agents
## capacity: largest number of agents could be held in environment
## st_birth/death: age start to reproducing/dying

death.microbe <- function(m,st_death,death_rate,population,capacity){
  if (m$age > st_death){
  p <- runif(1,0,1)
  death <-  death_rate/(1+exp(-m$age^2))
  rate <- death*(population/capacity)
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
death <- function(m,st_death,death_rate,population,capacity){
  UseMethod("death")
}

#' @export

birth.general <- function(m,st_birth,birth_rate,population,capacity){
    if(m$age < st_birth){
      n <- NA
    }else{
    p <- runif(1,0,1)
    rate <- birth_rate*(1-population/capacity)
    if(rate > p){
      n <- new_microbe(speed = runif(1,0,1),location=c(m$location[1]+runif(1,-0.1,0.1),m$location[2]+runif(1,-0.1,0.1)),parent = m$id)
    }else{
      n <- NA
    }
    }
  return(n)
}
  
birth.background <- function(m,st_birth,birth_rate,population,capacity){
  if(m$age < st_birth){
    n <- NA
  }else{
    p <- runif(1,0,1)
    rate <- birth_rate*(1-population/capacity)
    if(rate > p){
      n <- background_microbe(speed = runif(1,0,1),location=c(m$location[1]+runif(1,-0.1,0.1),m$location[2]+runif(1,-0.1,0.1)),parent = m$id)
    }else{
      n <- NA
    }
  }
  return(n)
}


#' @export
birth <- function(m,st_birth,birth_rate,population,capacity){
  UseMethod("birth")
} 