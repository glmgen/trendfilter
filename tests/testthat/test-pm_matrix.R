test_that("pm_matrix produces correct structure", {
  # Define input
  n <- 10
  x <- seq(0, 10, length.out = n)  # 10 evenly spaced points
  m1 <- 3
  m2 <- 3

  # Compute pm_matrix
  Pm <- pm_matrix(x, m1, m2)

  # 1. Check dimensions: (n, n-m1-m2)
  expect_equal(dim(Pm), c(n, n - m1 - m2), info = "Matrix dimensions should be (n, n-m1-m2)")

  # 2. Check middle section: Identity matrix
  middle_section <- Pm[(m1 + 1):(n - m2), ]
  middle_dense <- as.matrix(middle_section)   # Convert to dense matrix
  expect_equal(middle_dense, diag(n - m1 - m2), info = "Middle section should be an identity matrix")

  # 3. Check upper-left boundary matrix: (m1 x m1) non-zero
  upper_left <- Pm[1:m1, 1:m1]
  expect_true(all(upper_left != 0), info = "Upper-left matrix should be non-zero")

  # 4. Check lower-right boundary matrix: (m2 x m2) non-zero
  lower_right <- Pm[(n - m2 + 1):n, (n - m1 - 2 * m2 + 1):(n - m1 - m2)]
  expect_true(all(lower_right != 0), info = "Lower-right matrix should be non-zero")
})
