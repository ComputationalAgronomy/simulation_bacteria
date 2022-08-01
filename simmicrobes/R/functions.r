# move.microbe <- function(m){
#   p <- runif(1,0,1)
#   if (p < 1/2){## prop of (0,90d) = 1/2, else =  2/3
#     theta <- runif(1,0,pi/2)
#   }else {
#     theta <- runif(1,pi/2,2*pi)
#   }
#   ## prop of (0,90d) = 1/2, else =  2/3
#   m$location[1] <- m$location[1]+m$speed*cos(theta)
#   m$location[2] <- m$location[2]+m$speed*sin(theta)
#   return(m)
# }


# move <- function(m){
#   UseMethod("move")
# }


# age.microbe <- function(m){
#   m$age <- m$age + 1
#   return(m)
# }


# age <- function(m){
#   UseMethod("age")
# }

##x <- seq(0,5,0.01)
##y <- -dexp(x,1)+1
##plot(x,y)




#' @importFrom stats dexp runif
#' @export
death.microbe <- function(m){
  q <- (m$age-50)/10
  rate <- -dexp(q,1)+1
  p <- runif(1)
  if(q > p){
    return("True")
  }else{
    return("False")
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
