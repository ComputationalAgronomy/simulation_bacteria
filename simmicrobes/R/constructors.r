## This constructor is used to test whether the list format is valid or not.

library("schoolmath")
validate_microbe <- function(m){
  values1 <- unclass(m$speed)
  values2 <- unclass(m$location)
  values3 <- uncalss(m$parent)
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
  
  if(is.na(values3) == FALSE && is.double(values3) == FALSE && schoolmath::is.decimal(value3) == TRUE){
    stop(
      "The 'parent' should be integer or NA",
      call. = FALSE
    )
  }
}
