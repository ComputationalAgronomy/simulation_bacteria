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
move <- function(m,density,resource){
   UseMethod("move")
}

library(raster)

## resource should be a point, such as c(5,5)
#' @export

angle.cal <- function(r,m){
  a <- r - m
  b <- c(1,0)
  if(m[2] >= r[2]){
    angle <- 2*pi-(acos(sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)))))
  }else{
   angle <- acos(sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)))) 
  }
  return(angle)
}


move.background <- function(m,density,resource){
  angle <- angle.cal(resource,m$location)
  ori <- runif(100,0,2*pi)
  prob <- dnorm(ori,angle,1)
  for(i in 1:length(ori)){
    if(ori[i] > 0 & ori[i] < pi/2){
      prob[i] <- prob[i]*density[1]
    }
    if(ori[i] > pi/2 & ori[i] < pi){
      prob[i] <- prob[i]*density[2]
    }
    if(ori[i] > pi & ori[i] < 3*pi/2){
      prob[i] <- prob[i]*density[3]
    }else{
      prob[i] <- prob[i]*density[4]
    }
  }
  p <- runif(1,0,1)
  if(p < 0.9){
    theta <- sample(ori,1,prob = prob)
  }else{
    theta <- NA
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

## if no preference involved.
#' @export
move.general <- function(m,density,resource = NA){
  p <- runif(1,0,1)
  if(p < 0.9){
    o1 <- runif(1,0,pi/2)
    o2 <- runif(1,pi/2,pi)
    o3 <- runif(1,pi,3*pi/2)
    o4 <- runif(1,3*pi/2,2*pi)
   theta <- sample(c(o1,o2,o3,o4),1,prob = density)
  }else{
    theta <- NA
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
