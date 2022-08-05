#' @importFrom stats dexp runif
#' @export
## "True" means alive, "False" means dead. 
death.microbe <- function(m){
  if (m$age>50){
  q <- (m$age-50)/10
  p <- runif(1)
  rate <- -dexp(q,1)+1
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
death <- function(m){
  UseMethod("death")
}

#' @export
birth.microbe <- function(m){
  q <- m$age
  p <- runif(1)
  if(10 < q && q <= 15){
  rate <- 1.5*(q-10)/10
  if(rate > p){
    n <- new_microbe(speed = runif(1, 1,5), location=c(m$location[1]+1,m$location[2]+1))
  }else{
    n <- NA
  }
  }else if(15 < q && q <= 20){
    rate <- 0.75
    if(rate > p){
      n <- new_microbe(speed = runif(1, 1,5), location=c(m$location[1]+1,m$location[2]+1))
    }else{
      n <- NA
    }
  }else if(20 < q && q<= 30){
    rate <- 0.75*(30-q)/10
    if(rate > p){
      n <- new_microbe(speed = runif(1, 1,5), location=c(m$location[1]+1,m$location[2]+1))
    }else{
      n <- NA
    }
  }else{
    n <- NA
  }
  return(n)
}

#' @export
birth <- function(m){
  UseMethod("birth")
}
