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
move <- function(m,distance,angle,radius,resource){
   UseMethod("move")
}

library(raster)

## resource should be a point, such as c(5,5)
#' @export

move.background <- function(m,distance_table,angle_table,radius,resource){
  p <- runif(1,0,1)
  if(p < 0.1){
    theta <- NA
  }else{
  angle <- angle.cal(resource,m$location)
  ori <- seq(pi/180,2*pi,pi/180)
  prob <- dnorm(ori,angle,0.5)
  it <- 1
  while(it <= length(ori)){
    n <- 0
    angle_modify <- vector()
    angle_ins <- angle_table[which(distance_table <= radius)]
    if(length(angle_ins) > 0){
    angle_modify <- angle_ins - ori[it]
    for(i in 1:length(angle_modify)){
      if (angle_modify[i] < 0){
        angle_modify[i] <- angle_modify[i]+2*pi
      }else{
        angle_modify[i] <- angle_modify[i]
      }
    if(angle_modify[i] <= pi/12 | angle_modify[i] >= 23*pi/12){
      n <- n+1
    }else{
      n <- n
    }
    }
    }else{
      n <- 0
    }
    prob[it] <- (1/(n+1))*prob[it]
    it <- it+1
  }
   theta <- sample(ori,1,prob = prob/360)
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
move.general <- function(m,distance_table,angle_table,radius,resource = NA){
  p <- runif(1,0,1)
  if(p < 0.1){
    theta <- NA
  }else{
  ori <- seq(pi/180,2*pi,pi/180)
  prob <- rep(1,360)
  it <- 1
  while(it <= length(ori)){
    n <- 0
    angle_modify <- vector()
    angle_ins <- angle_table[which(distance_table <= radius)]
    if(length(angle_ins) > 0){
      angle_modify <- ori[it] - angle_ins
      
      for(i in 1:length(angle_modify)){
        if (angle_modify[i] < 0){
          angle_modify[i] <- angle_modify[i]+2*pi
        }else{
          angle_modify[i] <- angle_modify[i]
        }
        if(angle_modify[i] <= pi/12 | angle_modify[i] >= 23*pi/12){
          n <- n+1
        }else{
         n <- n
        }
      }
    }else{
      n <- 0
    }
    prob[it] <- (1/(n+1))*prob[it]
    it <- it+1
  }
  theta <- sample(ori,1,prob = prob/360)
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
