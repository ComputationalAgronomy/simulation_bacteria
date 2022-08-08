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
    class = c("microbe", "basic_microbe")
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

#' @export
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
