# Global static variable. Keep track of the latest microbe ID
static_env <- new.env()
static_env$id <- 1

# Helper function for creating microbes to avoid code duplication.
# Not exported.
internal_new_microbe <- function(speed, location, parent, class_name) {
  stopifnot(is.double(speed))

  microbe <- structure(
    list(
      id = static_env$id,
      age = 0,
      speed = speed,
      location = location,
      parent = parent
    ),
    class = c("microbe", class_name)
  )
  static_env$id <- static_env$id + 1
  return(microbe)
}

# Note. Store location as vector instead of matrix.
#' @export
new_microbe <- function(speed = double(), location = c(0, 0), parent = NA) {
  internal_new_microbe(speed, location, parent, "general")
}

#' @export
background_microbe <- function(speed = double(), location = c(0, 0), parent = NA) {
  internal_new_microbe(speed, location, parent, "background")
}

#' @export
summary.microbe <- function(m) {
  output <- sprintf("ID: %d\tAge: %d\tSpeed: %.01f\tLoc: %.04f, %.04f\tParent: %d\n", m$id, m$age, m$speed, m$location[1], m$location[2], m$parent)
  return(cat(output))
}

#' @export
print.microbe <- function(m) {
  summary(m)
}

#' @export
move <- function(m, distance, angle, sd, radius, arc, ec, resource) {
  UseMethod("move")
}

ANGLE_THRESHOLD <- pi/180/2
.apply_repulsion_and_sample_angle_old <- function(prob, ori, angle_ins, arc, ec) {
  if (length(angle_ins) > 0) {
    ag <- list()
    for (i in seq_along(angle_ins)) {
      ag[[i]] <- seq(angle_ins[i] - arc, angle_ins[i] + arc, pi / 180)
    }
    ag <- unlist(ag)
    for (i in seq_along(ag)) {
      if (ag[i] <= 0) {
        ag[i] <- ag[i] + 2 * pi
      }
      if (ag[i] > 2 * pi) {
        ag[i] <- ag[i] - 2 * pi
      }
      index <- which(abs(ori - ag[i]) < ANGLE_THRESHOLD)
      prob[index] <- prob[index] / ec
    }
  }
  sample(ori, 1, prob = prob)

}

# Helper function to apply repulsion from neighbors and sample a final angle.
# This vectorized implementation is much more efficient than the original loops.
DEG_TO_RAD <- pi / 180
.apply_repulsion_and_sample_angle <- function(prob, ori, angle_ins, arc, ec) {
  if (length(angle_ins) > 0) {
    # Vectorized calculation of all angles in the repulsion arcs
    arc_rad_steps <- seq(-arc, arc, by = pi / 180)
    all_forbidden_angles <- as.vector(outer(angle_ins, arc_rad_steps, `+`))

    # Normalize angles to be within [0, 2*pi) and convert to integer indices (1 to 360)
    all_forbidden_angles <- all_forbidden_angles %% (2 * pi)

    indices_to_penalize <- table(round(all_forbidden_angles / DEG_TO_RAD))
    # indices_to_penalize <- indices_to_penalize[!is.na(indices_to_penalize)]
    # indices_to_penalize[indices_to_penalize == 0] <- 360 # Map 0 degrees to index 360
    if ("0" %in% names(indices_to_penalize)) {
      if (!"360" %in% names(indices_to_penalize)) {
        indices_to_penalize[["360"]] <- 0
      }
      indices_to_penalize[["360"]] <- indices_to_penalize[["360"]] + indices_to_penalize[["0"]] # Map 0 degrees to index 360
      #remove the 0 degree index
      indices_to_penalize <- indices_to_penalize[names(indices_to_penalize) != "0"]
    }
    # Apply repulsion penalty in a single vectorized step
    # prob[indices_to_penalize] <- prob[indices_to_penalize] / ec
    index <- as.numeric(names(indices_to_penalize))
    prob[index] <- prob[index] / (ec^indices_to_penalize)
  }

  # Sample the final angle from the (potentially modified) probability distribution
  sample(ori, 1, prob = prob)
}


## resource should be a point, such as c(5,5)
#' @export

move.background <- function(m, distance_table, angle_table, sd, radius, arc, ec, resource) {
  p <- runif(1, 0, 1)
  if (p < 0.1) {
    theta <- NA
  } else {
    angle <- cal_angle_each(resource, m$location)
    ori <- seq(pi / 180, 2 * pi, pi / 180)
    for (i in 1:360) {
      if (ori[i] - angle >= pi) {
        ori[i] <- ori[i] - 2 * pi
      }
      if (ori[i] - angle <= -pi) {
        ori[i] <- ori[i] + 2 * pi
      }
    }
    prob <- dnorm(ori, angle, sd)
    ori <- seq(pi / 180, 2 * pi, pi / 180)
    angle_ins <- angle_table[which(distance_table <= radius)]
    theta <- .apply_repulsion_and_sample_angle(prob, ori, angle_ins, arc, ec)
  }
  if (is.na(theta) == T) {
    m$location[1] <- m$location[1]
    m$location[2] <- m$location[2]
  } else {
    m$location[1] <- m$location[1] + m$speed * cos(theta)
    m$location[2] <- m$location[2] + m$speed * sin(theta)
  }
  return(m)
}

## if no preference involved.
#' @export
move.general <- function(m, distance_table, angle_table, sd = NA, radius, arc, ec, resource = NA) {
  p <- runif(1, 0, 1)
  if (p < 0.1) {
    theta <- NA
  } else {
    ori <- seq(pi / 180, 2 * pi, pi / 180)
    prob <- rep(1, 360)
    angle_ins <- angle_table[which(distance_table <= radius)]
    theta <- .apply_repulsion_and_sample_angle(prob, ori, angle_ins, arc, ec)
  }
  if (is.na(theta) == T) {
    m$location[1] <- m$location[1]
    m$location[2] <- m$location[2]
  } else {
    m$location[1] <- m$location[1] + m$speed * cos(theta)
    m$location[2] <- m$location[2] + m$speed * sin(theta)
  }
  return(m)
}


# Age function without generic method
#' @export
age.microbe <- function(m) {
  m$age <- m$age + 1
  return(m)
}

# functions and generic method with UseMethod()
age <- function(m) {
  UseMethod("age")
}



## Example, create 5 microbes
# microbe_list <- lapply(1:5, function(x){
#   new_microbe(speed = runif(1, 1,5), location=c(0,0))
# })

## generic function
# age(microbe_list[[1]])
# move(microbe_list[[1]])
