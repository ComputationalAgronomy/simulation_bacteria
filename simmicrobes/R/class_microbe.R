# Global static variable. Keep track of the latest microbe ID
static_env <- new.env()
static_env$id <- 1

# Note. Store location as vector instead of matrix.
new_microbe <- function(speed = double(), location=c(0,0)) {
  stopifnot(is.double(speed))

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

validate_microbe <- function(x){
  # TODO: validator method
}
microbe <- function(x){
  # TODO: Helper method
}


summary.microbe <- function(m){
  output <- sprintf("ID: %d\tAge: %d\tSpeed: %.01f\tLoc: %.04f, %.04f\n", m$id, m$age, m$speed, m$loc[1], m$loc[2])
  return(cat(output))
}

print.microbe <- function(m){
  summary(m)
}
# Note: These two methods summary and print, already have generic method setup.
# Therefore we can call summary(m), print(m)

#TODO: fix this later
n <- 1
move.microbe <- function(m){
  p <- runif(n,0,1)
  theta <- vector("numeric",length = n)
  for(i in 1:n){
    if (p[i] < 1/2){
      theta[i] <- runif(1,0,pi/2)
    }else {
      theta[i] <- runif(1,pi/2,2*pi)
    }
  } ## prop of (0,90d) = 1/2, else =  2/3
  m$loc[1,1] <- m$loc[1,1]+m$speed*cos(theta[i])
  m$loc[1,2] <- m$loc[1,2]+m$speed*sin(theta[i])
  return(m)
}
move <- function(m){
  UseMethod("move")
}



# Age function without generic method
age.microbe <- function(m){
  m$age <- m$age + 1
  return(m)
}
# funcions and generic method with UseMethod()
age <- function(m){
  UseMethod("age")
}



# Example, create 5 microbes
microbe_list <- lapply(1:5, function(x){
  new_microbe(speed = runif(1, 1,5), location=c(0,0))
})

# generic function
age(microbe_list[[1]])
move(microbe_list[[1]])


microbe_list <- lapply(microbe_list, age)
microbe_list
