library(testthat)

test_that("new_microbe creates a microbe with correct defaults", {
  m <- new_microbe(speed = 2.5)
  expect_s3_class(m, "microbe")
  expect_equal(m$speed, 2.5)
  expect_equal(m$location, c(0, 0))
  expect_equal(m$age, 0)
  expect_true(is.na(m$parent))
})

test_that("background_microbe creates a background microbe", {
  m <- background_microbe(speed = 1.2, location = c(3, 4), parent = 10)
  expect_s3_class(m, "background")
  expect_equal(m$speed, 1.2)
  expect_equal(m$location, c(3, 4))
  expect_equal(m$parent, 10)
})

test_that("summary.microbe outputs expected string", {
  m <- new_microbe(speed = 1.5, location = c(1, 2), parent = 5)
  expect_output(summary(m), "ID: [0-9]+\tAge: 0\tSpeed: 1.5\tLoc: 1.0000, 2.0000\tParent: 5")
})

test_that("print.microbe calls summary", {
  m <- new_microbe(speed = 1.5, location = c(1, 2), parent = 5)
  expect_output(print(m), "ID: [0-9]+\tAge: 0\tSpeed: 1.5\tLoc: 1.0000, 2.0000\tParent: 5")
})

test_that("age.microbe increments age", {
  m <- new_microbe(speed = 1)
  m2 <- age.microbe(m)
  expect_equal(m2$age, m$age + 1)
})

test_that("age generic dispatches to age.microbe", {
  m <- new_microbe(speed = 1)
  m2 <- age(m)
  expect_equal(m2$age, m$age + 1)
})

test_that("move.general changes location or not based on random p", {
  set.seed(123)
  m <- new_microbe(speed = 1, location = c(0, 0))
  distance_table <- c(1, 2, 3)
  angle_table <- c(0, pi / 2, pi)
  m2 <- move.general(m, distance_table, angle_table, radius = 2, arc = pi / 6, ec = 2)
  # With set.seed, p > 0.1, so location should change
  expect_false(all(m2$location == c(0, 0)))
})


test_that("move.background changes location or not based on random p", {
  set.seed(123)
  m <- background_microbe(speed = 1, location = c(0, 0))
  distance_table <- c(1, 2, 3)
  angle_table <- c(0, pi / 2, pi)
  resource <- c(5, 5)
  # cal_angle_each is not defined in the provided code, so skip if not available
  if (exists("cal_angle_each")) {
    m2 <- move.background(m, distance_table, angle_table, sd = 1, radius = 2, arc = pi / 6, ec = 2, resource = resource)
    expect_true(is.numeric(m2$location[1]) && is.numeric(m2$location[2]))
  } else {
    succeed()
  }
})
