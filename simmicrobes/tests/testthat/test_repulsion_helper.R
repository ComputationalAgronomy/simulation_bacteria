# Gemini code
# It's good practice to test internal helper functions.
# We copy the function here to make the test self-contained.
apply_repulsion_and_sample_angle_v2 <- function(prob, ori, angle_ins, arc, ec) {
  if (length(angle_ins) > 0) {
    # Vectorized calculation of all angles in the repulsion arcs
    arc_rad_steps <- seq(-arc, arc, by = pi / 180)
    all_forbidden_angles <- as.vector(outer(angle_ins, arc_rad_steps, `+`))

    # Normalize angles to be within [0, 2*pi) and convert to integer indices (1 to 360)
    all_forbidden_angles <- all_forbidden_angles %% (2 * pi)
    DEG_TO_RAD <- pi / 180
    indices_to_penalize <- unique(round(all_forbidden_angles / DEG_TO_RAD))
    indices_to_penalize[indices_to_penalize == 0] <- 360 # Map 0 degrees to index 360

    # Apply repulsion penalty in a single vectorized step
    prob[indices_to_penalize] <- prob[indices_to_penalize] / ec
  }

  # Sample the final angle from the (potentially modified) probability distribution
  sample(ori, 1, prob = prob)
}
apply_repulsion_and_sample_angle <- function(prob, ori, angle_ins, arc, ec) {
  if (length(angle_ins) > 0) {
    ag <- list()
    for (i in 1:length(angle_ins)) {
      ag[[i]] <- seq(angle_ins[i] - arc, angle_ins[i] + arc, pi / 180)
    }
    ag <- unlist(ag)
    for (i in 1:length(ag)) {
      if (ag[i] <= 0) {
        ag[i] <- ag[i] + 2 * pi
      }
      if (ag[i] > 2 * pi) {
        ag[i] <- ag[i] - 2 * pi
      }
      # Note: The use of `which` with float comparison is fragile, but we test the function as-is.
      indices <- which(abs(ori - ag[i]) < 0.001)
      if (length(indices) > 0) {
        prob[indices] <- prob[indices] / ec
      }
    }
    theta <- sample(ori, 1, prob = prob)
  } else {
    theta <- sample(ori, 1, prob = prob)
  }
  return(theta)
}

# Common setup for statistical tests
N_SAMPLES <- 5000
ORI <- seq(pi / 180, 2 * pi, pi / 180)

test_that("it samples uniformly when no neighbors are present", {
  set.seed(123)
  prob <- rep(1, 360)
  angle_ins <- numeric(0) # No neighbors

  samples <- replicate(N_SAMPLES, {
    apply_repulsion_and_sample_angle_local(prob, ORI, angle_ins, arc = pi / 6, ec = 100)
  })

  # A simple check that samples are not all clustered in one quadrant
  quadrants <- cut(samples, breaks = c(0, pi / 2, pi, 3 * pi / 2, 2 * pi), include.lowest = TRUE)
  counts <- table(quadrants)
  expect_true(all(counts > N_SAMPLES / 8), "Samples should be distributed across all quadrants")
})

test_that("it avoids a single neighbor's repulsion zone", {
  set.seed(456)
  prob <- rep(1, 360)
  neighbor_angle <- pi # 180 degrees
  repulsion_arc <- pi / 4 # 45 degrees

  samples <- replicate(N_SAMPLES, {
    apply_repulsion_and_sample_angle_local(prob, ORI, neighbor_angle, repulsion_arc, ec = 1e6)
  })

  forbidden_zone_start <- neighbor_angle - repulsion_arc
  forbidden_zone_end <- neighbor_angle + repulsion_arc
  proportion_forbidden <- mean(samples >= forbidden_zone_start & samples <= forbidden_zone_end)

  # With a high ec, this proportion should be very close to zero
  expect_lt(proportion_forbidden, 0.01)
})

test_that("it correctly handles angle wrapping around 2*pi", {
  set.seed(101)
  prob <- rep(1, 360)
  neighbor_angle <- 10 * pi / 180 # 10 degrees
  repulsion_arc <- 20 * pi / 180 # 20 degrees

  samples <- replicate(N_SAMPLES, {
    apply_repulsion_and_sample_angle_local(prob, ORI, neighbor_angle, repulsion_arc, ec = 1e6)
  })

  # Forbidden zone is from -10 to 30 degrees, which wraps around 0/2pi
  in_forbidden_zone <- (samples >= (350 * pi / 180)) | (samples <= (30 * pi / 180))
  proportion_forbidden <- mean(in_forbidden_zone)

  expect_lt(proportion_forbidden, 0.01)
})

test_that("it avoids multiple neighbors' repulsion zones", {
  set.seed(789)
  prob <- rep(1, 360)
  neighbor_angles <- c(pi / 2, 3 * pi / 2) # 90 and 270 degrees
  repulsion_arc <- pi / 6 # 30 degrees

  samples <- replicate(N_SAMPLES, {
    apply_repulsion_and_sample_angle_local(prob, ORI, neighbor_angles, repulsion_arc, ec = 1e6)
  })

  # Zone 1: 90 +/- 30 degrees -> [60, 120] degrees
  in_zone1 <- (samples >= pi / 3) & (samples <= 2 * pi / 3)
  # Zone 2: 270 +/- 30 degrees -> [240, 300] degrees
  in_zone2 <- (samples >= 4 * pi / 3) & (samples <= 5 * pi / 3)

  proportion_forbidden <- mean(in_zone1 | in_zone2)
  expect_lt(proportion_forbidden, 0.01, "Should avoid both repulsion zones")
})

test_that("repulsion strength (ec) correctly modifies probability", {
  set.seed(111)
  prob <- rep(1, 360)
  neighbor_angle <- pi
  repulsion_arc <- pi / 6 # 30 degrees on each side, so a 60 degree total arc

  # Low repulsion (ec=2) should reduce but not eliminate probability
  samples_low_ec <- replicate(N_SAMPLES, {
    apply_repulsion_and_sample_angle_local(prob, ORI, neighbor_angle, repulsion_arc, ec = 2)
  })

  forbidden_zone_start <- neighbor_angle - repulsion_arc
  forbidden_zone_end <- neighbor_angle + repulsion_arc

  prop_low_ec <- mean(samples_low_ec >= forbidden_zone_start & samples_low_ec <= forbidden_zone_end)

  # Theoretical proportion for ec=2 is (60/2)/(300 + 60/2) = 30/330 ~ 0.09
  expect_lt(prop_low_ec, 0.15, "Probability should be less than uniform chance (0.167)")
  expect_gt(prop_low_ec, 0.05, "Probability should be significantly greater than zero")
})

test_that("Original and V2 differ on overlapping zones (V2 is correct)", {
  # This test reveals a bug in the original function: it penalizes overlapping zones multiple times.
  # V2's use of `unique()` on the indices handles this correctly.

  # Helper to return the probability vector instead of sampling
  get_prob_vec <- function(fun, prob, ori, angle_ins, arc, ec) {
    local_env <- new.env(parent = environment(fun))
    # Temporarily replace `sample` to capture the `prob` vector
    local_env$sample <- function(x, size, prob) {
      return(prob)
    }
    environment(fun) <- local_env
    fun(prob, ori, angle_ins, arc, ec)
  }

  prob <- rep(1, 360)
  # Two neighbors close together, so their repulsion arcs overlap
  neighbor_angles <- c(pi, pi + 4 * pi / 180) # 180 and 184 degrees
  repulsion_arc <- 5 * pi / 180 # 5 degree arc
  ec <- 10

  # The original function is `apply_repulsion_and_sample_angle`
  prob_v1 <- get_prob_vec(apply_repulsion_and_sample_angle, prob, ORI, neighbor_angles, repulsion_arc, ec)
  prob_v2 <- get_prob_vec(apply_repulsion_and_sample_angle_v2, prob, ORI, neighbor_angles, repulsion_arc, ec)

  # Check a point in the overlapping zone (e.g., 182 degrees)
  overlap_idx <- 182
  # Original function penalizes twice: 1 / (ec*ec)
  expect_equal(prob_v1[overlap_idx], 1 / (ec^2))
  # V2 penalizes once: 1 / ec
  expect_equal(prob_v2[overlap_idx], 1 / ec)
})

test_that("Original and V2 are statistically equivalent for simple, non-overlapping cases", {
  set.seed(321)
  prob <- rep(1, 360)
  neighbor_angle <- pi # 180 degrees
  repulsion_arc <- pi / 6 # 30 degrees
  ec <- 10

  # No overlap, simple case
  samples_v1 <- replicate(N_SAMPLES, apply_repulsion_and_sample_angle(prob, ORI, neighbor_angle, repulsion_arc, ec))
  samples_v2 <- replicate(N_SAMPLES, apply_repulsion_and_sample_angle_v2(prob, ORI, neighbor_angle, repulsion_arc, ec))

  # Bin the results into categories and use a Chi-squared test to compare distributions.
  breaks <- seq(0, 2 * pi, length.out = 13)
  test_result <- chisq.test(rbind(table(cut(samples_v1, breaks)), table(cut(samples_v2, breaks))))

  # A high p-value means we can't reject the null hypothesis that the distributions are the same.
  expect_gt(test_result$p.value, 0.05)
})
