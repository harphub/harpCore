# Tests for circular statistics

test_that("circle_bias() gives the correct values", {
  expect_equal(
    circle_bias(c(180, 355, 5, 20, 210), c(30, 5, 355, 210, 20)),
    c(150, -10, 10, 170, -170)
  )
})

test_that("circle_mean() gives the correct values for vectors", {
  expect_equal(circle_mean(c(350, 355, 5, 10)), 0)
  expect_equal(circle_mean(c(355, 0, 5, 10)), 2.5)
  expect_equal(circle_mean(c(90, 135, 225, 270)), 180)
  expect_equal(circle_mean(c(90, 45, 315, 270)), 0)
})

test_that("circle_mean() gives the correct values for data frame", {
  x <- data.frame(
    a = c(350, 355, 90, 90),
    b = c(355, 0, 135, 45),
    c = c(5, 5, 225, 315),
    d = c(10, 10, 270, 270)
  )
  expect_equal(circle_mean(x), c(0, 2.5, 180, 0))
})

test_that("circle_var() gives the correct values for vectors", {
  expect_equal(circle_var(c(350, 355, 5, 10)), 0.0094987744)
  expect_equal(circle_var(c(355, 0, 5, 10)), 0.0047534585)
  expect_equal(circle_var(c(180, 180, 180, 180)), 0)
  expect_equal(circle_var(c(0, 90, 180, 270)), 1)
})

test_that("circle_var() gives the correct values for data frame", {
  x <- data.frame(
    a = c(350, 355, 180, 0),
    b = c(355, 0, 180, 90),
    c = c(5, 5, 180, 180),
    d = c(10, 10, 180, 270)
  )
  expect_equal(circle_var(x), c(0.0094987744, 0.0047534585, 0, 1))
})

test_that("circle_sd() gives the correct values for vectors", {
  expect_equal(circle_sd(c(350, 355, 5, 10)), 7.9160192094)
  expect_equal(circle_sd(c(355, 0, 5, 10)), 5.5931904816)
  expect_equal(circle_sd(c(180, 180, 180, 180)), 0)
  expect_equal(circle_sd(c(0, 90, 180, 270)), 497.37301046)
})

test_that("circle_sd() gives the correct values for data frame", {
  x <- data.frame(
    a = c(350, 355, 180, 0),
    b = c(355, 0, 180, 90),
    c = c(5, 5, 180, 180),
    d = c(10, 10, 180, 270)
  )
  # rowMeans uses slightly different ordering than mean, so the tolerance
  # needs to be lower
  expect_equal(
    circle_sd(x), c(7.9160192094, 5.5931904816, 0, 497.37301046),
    tolerance = 1e-5
  )
})

test_that("circle_sd_sq() gives the correct values for vectors", {
  expect_equal(circle_sd_sq(c(350, 355, 5, 10)), 1.0936819545)
  expect_equal(circle_sd_sq(c(355, 0, 5, 10)), 0.5460049593)
  expect_equal(circle_sd_sq(c(180, 180, 180, 180)), 0)
  expect_equal(circle_sd_sq(c(0, 90, 180, 270)), 4317.5939596)
})

test_that("circle_sd_sq() gives the correct values for data frame", {
  x <- data.frame(
    a = c(350, 355, 180, 0),
    b = c(355, 0, 180, 90),
    c = c(5, 5, 180, 180),
    d = c(10, 10, 180, 270)
  )
  # rowMeans uses slightly different ordering than mean, so the tolerance
  # needs to be lower
  expect_equal(
    circle_sd_sq(x), c(1.0936819545, 0.5460049593, 0, 4317.5939596),
    tolerance = 1e-5
  )
})

test_that("circle_sd() can be related to circle_sd_sq() for vectors", {
  x <- c(350, 355, 5, 10)
  expect_equal(sqrt(circle_sd_sq(x) * pi / 180), circle_sd(x) * pi / 180)

  x <- c(355, 0, 5, 10)
  expect_equal(sqrt(circle_sd_sq(x) * pi / 180), circle_sd(x) * pi / 180)

  x <- c(180, 180, 180, 180)
  expect_equal(sqrt(circle_sd_sq(x) * pi / 180), circle_sd(x) * pi / 180)

  x <- c(0, 90, 180, 270)
  expect_equal(sqrt(circle_sd_sq(x) * pi / 180), circle_sd(x) * pi / 180)
})

test_that("circle_sd() can be related to circle_sd_sq() for data frame", {
  x <- data.frame(
    a = c(350, 355, 180, 0),
    b = c(355, 0, 180, 90),
    c = c(5, 5, 180, 180),
    d = c(10, 10, 180, 270)
  )
  expect_equal(sqrt(circle_sd_sq(x) * pi / 180), circle_sd(x) * pi / 180)
})
