## This constructor is used to test whether the list format is valid or not. 

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