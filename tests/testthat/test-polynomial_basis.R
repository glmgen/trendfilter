test_that("Polynomial basis creation works", {
  # This will never be called unless k + 1 < n
  n <- 15L
  x <- 1:n
  expect_identical(polynomial_basis(x, 0, min(x), max(x)), matrix(1, n))
  xa <- 2 * (x - min(x)) / diff(range(x)) - 1
  expect_identical(polynomial_basis(x, 1, min(x), max(x)), unname(cbind(1, xa)))

  exp3 <- unname(cbind(1, xa, 0.5 * (3 * xa^2 - 1), 0.5 * (5 * xa^3 - 3 * xa)))
  expect_equal(polynomial_basis(x, 3, min(x), max(x)), exp3)

  # max dimension
  expect_equal(polynomial_basis(x, 5, min(x), max(x), 3), exp3)

  # irregular spacing
  x <- 5 * sort(runif(n)) - 2.5
  xa <- 2 * (x - min(x)) / diff(range(x)) - 1
  expect_identical(polynomial_basis(x, 1, min(x), max(x)), unname(cbind(1, xa)))

  exp3 <- unname(cbind(1, xa, 0.5 * (3 * xa^2 - 1), 0.5 * (5 * xa^3 - 3 * xa)))
  expect_equal(polynomial_basis(x, 3, min(x), max(x)), exp3)

  # max dimension
  expect_equal(polynomial_basis(x, 5, min(x), max(x), 3), exp3)
})
