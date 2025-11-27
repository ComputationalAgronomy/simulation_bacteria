#' @importFrom stats dexp runif
#' @export
## TRUE means alive, FALSE means dead.
## death_rate:intrinsic death rate of an agent
## population: number of agents
## capacity: largest number of agents could be held in environment
## st_birth/death: age start to reproducing/dying
death.microbe <- function(m, st_death, death_rate, population, capacity) {
  if (m$age > st_death) {
    p <- runif(1, 0, 1)
    death <- death_rate #/ (1 + exp(-m$age^2))
    rate <- min(1, death * (population / capacity))
    if (rate < p) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(TRUE)
  }
}

#' @export
death <- function(m, st_death, death_rate, population, capacity) {
  UseMethod("death")
}

#' @export
birth.general <- function(m, st_birth, birth_rate, population, capacity, move_dist = 0.1) {
  if (m$age < st_birth) {
    n <- NA
  } else {
    p <- runif(1, 0, 1)
    rate <- min(1, birth_rate * (1 - population / capacity))
    if (rate > p) {
      n <- new_microbe(
        speed = runif(1, 0, 1),
        location = c(m$location[1] + runif(1, -move_dist, move_dist), m$location[2] + runif(1, -move_dist, move_dist)), parent = m$id
      )
    } else {
      n <- NA
    }
  }
  return(n)
}

#' @export
birth.background <- function(m, st_birth, birth_rate, population, capacity, move_dist = 0.1) {
  if (m$age < st_birth) {
    n <- NA
  } else {
    p <- runif(1, 0, 1)
    rate <- min(1, birth_rate * (1 - population / capacity))
    if (rate > p) {
      n <- background_microbe(
        speed = runif(1, 0, 1),
        location = c(m$location[1] + runif(1, -move_dist, move_dist), m$location[2] + runif(1, -move_dist, move_dist)), parent = m$id
      )
    } else {
      n <- NA
    }
  }
  return(n)
}


#' @export
birth <- function(m, st_birth, birth_rate, population, capacity, move_dist = 0.1) {
  UseMethod("birth")
}
