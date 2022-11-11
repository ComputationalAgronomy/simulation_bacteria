# Global static variable. Keep track of the latest microbe ID
static_env <- new.env()
static_env$id <- 1


# Note. Store location as vector instead of matrix.
#' @export
new_microbe <- function(speed = double(), location=c(0,0),parent = NA) {
  stopifnot(is.double(speed))

  microbe <- structure(list(
    id = static_env$id,
    age = 0,
    speed = speed,
    location = location,
    parent = parent),
    class = c("microbe", "general")
  )
  static_env$id <- static_env$id  + 1
  return(microbe)
}

#' @export
background_microbe <- function(speed = double(), location=c(0,0),parent = NA){
  stopifnot(is.double(speed))
  
  microbe <- structure(list(
    id = static_env$id,
    age = 0,
    speed = speed,
    location = location ,
    parent = parent),
    class = c("microbe", "background")
  )
    static_env$id <- static_env$id  + 1
    return(microbe)
}
# validate_microbe <- function(x){
#   # TODO: validator method
# }
# microbe <- function(x){
#   # TODO: Helper method
# }

#' @export
summary.microbe <- function(m){
  output <- sprintf("ID: %d\tAge: %d\tSpeed: %.01f\tLoc: %.04f, %.04f\tParent: %d\n", m$id, m$age, m$speed, m$location[1], m$location[2],m$parent)
  return(cat(output))
}

#' @export
print.microbe <- function(m){
  summary(m)
}
# Note: These two methods summary and print, already have generic method setup.
# Therefore we can call summary(m), print(m)


## Background microbes has a preference to move (0,90d)
## Movement is also influenced by the density of microbes in 4 directions.
#' @export
move <- function(m,density){
   UseMethod("move")
}

#' @export
move.background <- function(m,density){
  n1 <- density[1]
  n2 <- density[2]
  n3 <- density[3]
  n4 <- density[4]
  w <- (4/5)/(2/(n1+1)+1/(n2+1)+1/(n3+1)+1/(n4+1))
  p1 <- w*2/(n1+1)
  p2 <- w/(n2+1)
  p3 <- w/(n3+1)
  p4 <- w/(n4+1)
  p <- runif(1,0,1)
  if (p < 1/5){
    theta <- NA
  }else if(p < 1/5+p1){
    theta <- runif(1,0,pi/2)
  }else if(p < 1/5+p1+p2){
    theta <- runif(1,pi/2,pi)
  }else if(p < 1/5+p1+p2+p3){
    theta <- runif(1,pi,3*pi/2)
  }else{
    theta <- runif(1,3*pi/2,2*pi)
  }
  if(is.na(theta) == T){
    m$location[1] <- m$location[1]
    m$location[2] <- m$location[2]
  }else{
  m$location[1] <- m$location[1]+m$speed*cos(theta)
  m$location[2] <- m$location[2]+m$speed*sin(theta)
  }
  return(m)
}

#' @export
move.general <- function(m,density){
  density <- NA
  p <- runif(1,0,1)
  if(p<1/5){
    m$location[1] <- m$location[1]
    m$location[2] <- m$location[2]
  }else{
    theta <- runif(1,0,2*pi)
    m$location[1] <- m$location[1]+m$speed*cos(theta)
    m$location[2] <- m$location[2]+m$speed*sin(theta)
  }
  return(m)
}

# Age function without generic method
#' @export
age.microbe <- function(m){
  m$age <- m$age + 1
  return(m)
}

# functions and generic method with UseMethod()
age <- function(m){
  UseMethod("age")
}



## Example, create 5 microbes
# microbe_list <- lapply(1:5, function(x){
#   new_microbe(speed = runif(1, 1,5), location=c(0,0))
# })

# # generic function
# age(microbe_list[[1]])
# move(microbe_list[[1]])


# microbe_list <- lapply(microbe_list, age)
# microbe_list
