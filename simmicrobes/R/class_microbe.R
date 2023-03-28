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

#' @export
summary.microbe <- function(m){
  output <- sprintf("ID: %d\tAge: %d\tSpeed: %.01f\tLoc: %.04f, %.04f\tParent: %d\n", m$id, m$age, m$speed, m$location[1], m$location[2],m$parent)
  return(cat(output))
}

#' @export
print.microbe <- function(m){
  summary(m)
}

#' @export
move <- function(m,distance,angle,sd,radius,arc,ec,resource){
   UseMethod("move")
}

library(raster)

## resource should be a point, such as c(5,5)
#' @export

move.background <- function(m,distance_table,angle_table,sd,radius,arc,ec,resource){
  p <- runif(1,0,1)
  if(p < 0.1){
    theta <- NA
  }else{
  angle <- angle.cal(resource,m$location)
  ori <- seq(pi/180,2*pi,pi/180)
  for(i in 1:360){
    if(ori[i]-angle >= pi){
      ori[i] <- ori[i] - 2*pi
    }
    if(ori[i]-angle <= -pi){
      ori[i] <- ori[i] + 2*pi
    }
  }
  prob <- dnorm(ori,angle,sd)
  ori <- seq(pi/180,2*pi,pi/180)
  angle_ins <- angle_table[which(distance_table <= radius)]
  if(length(angle_ins) > 0){
    ag <- list()
    for(i in 1:length(angle_ins)){
      ag[[i]] <- seq(angle_ins[i]-arc,angle_ins[i]+arc,pi/180)
    }
    ag <- unlist(ag)
    for(i in 1:length(ag)){
      if(ag[i] <= 0){
        ag[i] <- ag[i]+2*pi
      }
      if(ag[i] > 2*pi){
        ag[i] <- ag[i]-2*pi
      }
      prob[which(abs(ori-ag[i]) < 0.001)] <- prob[which(abs(ori-ag[i]) < 0.001)]/ec
    }
    theta <- sample(ori,1,prob = prob)
  }else{
    theta <- sample(ori,1,prob = prob)
  }
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
move.general <- function(m,distance_table,angle_table,sd = NA,radius,arc,ec,resource = NA){
  p <- runif(1,0,1)
  if(p < 0.1){
    theta <- NA
  }else{
  ori <- seq(pi/180,2*pi,pi/180)
  prob <- rep(1,360)
  angle_ins <- angle_table[which(distance_table <= radius)]
  if(length(angle_ins) > 0){
  ag <- list()
  for(i in 1:length(angle_ins)){
    ag[[i]] <- seq(angle_ins[i]-arc,angle_ins[i]+arc,pi/180)
  }
  ag <- unlist(ag)
  for(i in 1:length(ag)){
    if(ag[i] <= 0){
      ag[i] <- ag[i]+2*pi
    }
    if(ag[i] > 2*pi){
      ag[i] <- ag[i]-2*pi
    }
    prob[which(abs(ori-ag[i]) < 0.001)] <- prob[which(abs(ori-ag[i]) < 0.001)]/ec
  }
  theta <- sample(ori,1,prob = prob)
  }else{
  theta <- sample(ori,1,prob = prob)
  }
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

## generic function
# age(microbe_list[[1]])
# move(microbe_list[[1]])


