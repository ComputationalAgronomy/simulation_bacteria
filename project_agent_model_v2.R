static_env <- new.env()
static_env$id <- 1


new_microbe <- function(speed = double(), location = c(0,0)) {
  stopifnot(is.double(speed))
  stopifnot(is.double(location))
  
  microbe <- structure(list(
    id = static_env$id,
    age = 0,
    speed = speed,
    location = location),
    class = c("microbe", "basic_microbe")
  )
  static_env$id <- static_env$id  + 1
  return(microbe)
}


validate_microbe <- function(m){
  values1 <- unclass(m$speed)
  values2 <- unclass(m$location)
  
  if (!all(!is.na(values1) & values1 > 0)){
    stop(
      "All `speed` values must be non-missing and greater than zero",
      call. = FALSE
    )
  }
  
  if(!length(values2) == 2){
    stop(
      "The length of location must equal to 2",
      call. = FALSE
    )
  }
}


microbe <- function(speed = double(), location = c(0,0)){
  speed <- as.double(speed)
  new_microbe(speed, location)
}


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
  q <- (m$age-50)/10
  rate <- -dexp(q,1)+1
  p <- runif(1)
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


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  