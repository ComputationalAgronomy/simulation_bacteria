# static_env <- new.env()
# static_env$id <- 1


# new_microbe <- function(speed = double(), location = c(0,0)) {
#   stopifnot(is.double(speed))
#   stopifnot(is.double(location))
  
#   microbe <- structure(list(
#     id = static_env$id,
#     age = 0,
#     speed = speed,
#     location = location),
#     class = c("microbe", "basic_microbe")
#   )
#   static_env$id <- static_env$id  + 1
#   return(microbe)
# }


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


# microbe <- function(speed = double(), location = c(0,0)){
#   speed <- as.double(speed)
#   new_microbe(speed, location)
# }
