move.microbe <- function(m){
  p <- runif(1,0,1)
  if (p < 1/2){## prop of (0,90d) = 1/2, else =  2/3
    theta <- runif(1,0,pi/2)
  }else {
    theta <- runif(1,pi/2,2*pi)
  }
  ## prop of (0,90d) = 1/2, else =  2/3
  m$location[1] <- m$location[1]+m$speed*cos(theta)
  m$location[2] <- m$location[2]+m$speed*sin(theta)
  return(m)
}


move <- function(m){
  UseMethod("move")
}


age.microbe <- function(m){
  m$age <- m$age + 1
  return(m)
}


age <- function(m){
  UseMethod("age")
}


death.microbe <- function(m){
  q <- m$age-50
  p <- runif(1,0,5)
  if(q > p){
    return("True")
  }else{
    return("False")
  }
}

death <- function(m){
  UseMethod("death")
}

birth.microbe <- function(m){
  q <- m$age-10
  p <- runif(1,0,5)
  if(q > p){
    n <- new_microbe(speed = runif(1, 1,5), location=c(m$location[1]+1,m$location[2]+1))
  }else{
    n <- NA
  }
  return(n)
}

birth <- function(m){
  UseMethod("birth")
}

microbe_list <- lapply(1:10, function(x){
  new_microbe(speed = runif(1, 1,5), location=c(runif(1,0,10),runif(1,0,10)))
})


microbe_list <- lapply(microbe_list, age)
microbe_list <- lapply(microbe_list, move)
## birth
microbe_list <- c(microbe_list,lapply(microbe_list,birth))
microbe_list <- microbe_list[!is.na(microbe_list)]
## death
logic <- lapply(microbe_list, death)
survive <- which(logic == "False")
microbe_list <- microbe_list[survive]