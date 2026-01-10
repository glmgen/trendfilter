library(Matrix)

# Scenario 1: n = 30, k = 2 ----
n1 <- 30L
k1 <- 2L
# signal locations
x1 <- 1:n1 / (n1 + 1) * 2 * pi
# random standard-normally distributed signals with size n1
y0 <- c(
  0.006,
  -0.119,
  0.262,
  0.043,
  1.114,
  -1.020,
  -0.350,
  -0.889,
  0.476,
  0.865,
  0.645,
  -0.631,
  0.277,
  -0.480,
  0.364,
  0.599,
  0.608,
  1.755,
  -0.041,
  -0.124,
  -0.031,
  0.195,
  -0.093,
  -0.207,
  2.405,
  -0.026,
  -1.115,
  0.474,
  0.129,
  0.222
)
# transform to sinusoidal signals with white noise
y1 <- sin(x1) + .2 * y0
# random Poisson signals with mean 5 for arbitrary weights on signals
weig1 <- c(
  5,
  5,
  1,
  5,
  8,
  3,
  2,
  6,
  4,
  5,
  4,
  3,
  4,
  6,
  5,
  2,
  9,
  4,
  1,
  2,
  5,
  4,
  3,
  5,
  3,
  3,
  9,
  4,
  6,
  7
)
# random standard-normally distributed signals as mean adjustment
mn1 <- c(
  0.485,
  -0.047,
  -0.200,
  1.646,
  -0.594,
  1.167,
  -0.816,
  0.295,
  0.784,
  1.446,
  1.328,
  0.852,
  -0.571,
  1.007,
  -0.741,
  1.114,
  0.232,
  -1.886,
  -0.542,
  2.111,
  -0.222,
  0.507,
  -0.111,
  0.199,
  0.181,
  -0.363,
  0.505,
  -0.868
)

# Scenario 2: n = 50, k = 3 ----
n2 <- 50L
k2 <- 3L
# signal locations
x2 <- 1:n2 / (n2 + 1) * 2 * pi
# random signals from Normal(0,1) with size n2
y0 <- c(
  -0.065,
  0.654,
  1.397,
  0.626,
  1.233,
  0.818,
  0.586,
  -0.693,
  -0.345,
  -0.550,
  0.618,
  -0.216,
  -1.355,
  0.131,
  -0.461,
  0.581,
  -1.709,
  -0.881,
  0.506,
  1.749,
  -0.449,
  1.856,
  -0.840,
  -2.980,
  -0.355,
  -1.221,
  0.450,
  0.006,
  0.350,
  0.790,
  0.405,
  1.467,
  0.890,
  0.282,
  -1.038,
  0.208,
  -0.039,
  1.166,
  1.192,
  0.027,
  1.078,
  2.363,
  -0.080,
  -0.074,
  -0.638,
  -0.539,
  -1.721,
  0.646,
  -0.899,
  0.600
)
y2 <- sin(x2) + .2 * y0
# random signals from Poisson(5) for arbitrary weights on signals
weig2 <- c(
  10,
  2,
  7,
  9,
  6,
  0,
  4,
  5,
  4,
  6,
  6,
  3,
  7,
  3,
  2,
  4,
  3,
  4,
  4,
  7,
  4,
  3,
  5,
  4,
  5,
  7,
  7,
  5,
  4,
  5,
  7,
  2,
  6,
  3,
  6,
  1,
  7,
  6,
  7,
  6,
  4,
  7,
  2,
  3,
  5,
  9,
  6,
  3,
  4,
  3
)
# random signals from Normal(0,1) as mean adjustment
mn2 <- c(
  0.194,
  -0.579,
  0.240,
  -1.641,
  -0.009,
  0.865,
  -0.362,
  -0.739,
  0.463,
  0.854,
  0.103,
  -0.718,
  -0.110,
  0.367,
  0.131,
  0.368,
  -1.452,
  2.719,
  -0.314,
  0.846,
  0.017,
  -0.899,
  -0.569,
  0.116,
  -0.849,
  0.969,
  -0.614,
  1.181,
  1.432,
  0.369,
  0.427,
  0.200,
  0.218,
  -0.678,
  -0.513,
  -1.233,
  -1.161,
  -0.601,
  1.064,
  -3.186,
  -1.311,
  0.603,
  -0.092,
  -0.429,
  0.997,
  -0.419,
  -2.362
)

test_that("test a single iterate of linear system solvers yields same results", {
  rho <- 2
  # Scenario 1
  Dkx <- dspline::d_mat(k1, x1, FALSE)
  theta <- as.vector(Matrix::solve(
    Diagonal(n1, weig1) + rho * Matrix::crossprod(Dkx),
    weig1 * y1 + rho * Matrix::crossprod(Dkx, mn1)
  ))
  theta_sparse_qr <- linear_single_solve_test(1, y1, weig1, x1, rho, mn1)
  theta_kf <- linear_single_solve_test(2, y1, weig1, x1, rho, mn1)
  theta_cholesky <- linear_single_solve_test(3, y1, weig1, x1, rho, mn1)
  expect_equal(theta_kf, theta)
  expect_equal(theta_sparse_qr, theta)
  expect_equal(theta_cholesky, theta)
  # Scenario 2
  Dkx <- dspline::d_mat(k2, x2, FALSE)
  theta <- as.vector(Matrix::solve(
    Diagonal(n2, weig2) + rho * Matrix::crossprod(Dkx),
    weig2 * y2 + rho * Matrix::crossprod(Dkx, mn2)
  ))
  theta_sparse_qr <- linear_single_solve_test(1, y2, weig2, x2, rho, mn2)
  theta_kf <- linear_single_solve_test(2, y2, weig2, x2, rho, mn2)
  theta_cholesky <- linear_single_solve_test(3, y2, weig2, x2, rho, mn2)
  expect_equal(theta_kf, theta)
  expect_equal(theta_sparse_qr, theta)
  expect_equal(theta_cholesky, theta)
})

test_that("test linear solvers yield same estimates for single lambda", {
  lam <- 5
  # Scenario 1
  mod_kf <- trendfilter(
    y1,
    x1,
    weights = weig1,
    k = k1,
    lambda = lam,
    control = trendfilter_control_list(
      admm_control = admm_control_list(linear_solver = "kalman_filter")
    )
  )
  mod_sparse_qr <- trendfilter(
    y1,
    x1,
    weights = weig1,
    k = k1,
    lambda = lam,
    control = trendfilter_control_list(
      admm_control = admm_control_list(linear_solver = "sparse_qr")
    )
  )
  mod_cholesky <- trendfilter(
    y1,
    x1,
    weights = weig1,
    k = k1,
    lambda = lam,
    control = trendfilter_control_list(
      admm_control = admm_control_list(linear_solver = "sparse_cholesky")
    )
  )
  expect_equal(mod_kf$theta, mod_sparse_qr$theta)
  expect_equal(mod_kf$theta, mod_cholesky$theta)
  # Scenario 2
  mod_kf <- trendfilter(
    y2,
    x2,
    weights = weig2,
    k = k2,
    lambda = lam,
    control = trendfilter_control_list(
      admm_control = admm_control_list(linear_solver = "kalman_filter")
    )
  )
  mod_sparse_qr <- trendfilter(
    y2,
    x2,
    weights = weig2,
    lambda = lam,
    k = k2,
    control = trendfilter_control_list(
      admm_control = admm_control_list(linear_solver = "sparse_qr")
    )
  )
  mod_cholesky <- trendfilter(
    y2,
    x2,
    weights = weig2,
    lambda = lam,
    k = k2,
    control = trendfilter_control_list(
      admm_control = admm_control_list(linear_solver = "sparse_cholesky")
    )
  )
  expect_equal(mod_kf$theta, mod_sparse_qr$theta)
  expect_equal(mod_kf$theta, mod_cholesky$theta)
})

test_that("test linear solvers run with no errors for lambda sequences", {
  nlam <- 10
  # Scenario 1
  expect_no_error(
    mod_kf <- trendfilter(
      y1,
      x1,
      weights = weig1,
      k = k1,
      nlambda = nlam,
      control = trendfilter_control_list(
        admm_control = admm_control_list(linear_solver = "kalman_filter")
      )
    )
  )
  expect_no_error(
    mod_sparse_qr <- trendfilter(
      y1,
      x1,
      weights = weig1,
      k = k1,
      nlambda = nlam,
      control = trendfilter_control_list(
        admm_control = admm_control_list(linear_solver = "sparse_qr")
      )
    )
  )
  expect_no_error(
    mod_cholesky <- trendfilter(
      y1,
      x1,
      weights = weig1,
      k = k1,
      nlambda = nlam,
      control = trendfilter_control_list(
        admm_control = admm_control_list(linear_solver = "sparse_cholesky")
      )
    )
  )
  # equal estimates from the first model
  expect_equal(mod_kf$theta[, 1], mod_sparse_qr$theta[, 1])
  expect_equal(mod_kf$theta[, 1], mod_cholesky$theta[, 1])
  # Scenario 2
  # skip("The rest of this takes too long to run.")
  expect_no_error(
    mod_kf <- trendfilter(
      y2,
      x2,
      weights = weig2,
      k = k2,
      nlambda = 2,
      control = trendfilter_control_list(
        admm_control = admm_control_list(linear_solver = "kalman_filter")
      )
    )
  )
  expect_no_error(
    mod_sparse_qr <- trendfilter(
      y2,
      x2,
      weights = weig2,
      k = k2,
      nlambda = 2,
      control = trendfilter_control_list(
        admm_control = admm_control_list(linear_solver = "sparse_qr")
      )
    )
  )
  expect_no_error(
    mod_cholesky <- trendfilter(
      y2,
      x2,
      weights = weig2,
      k = k2,
      nlambda = 2,
      control = trendfilter_control_list(
        admm_control = admm_control_list(linear_solver = "sparse_cholesky")
      )
    )
  )
  expect_equal(mod_kf$theta[, 1], mod_sparse_qr$theta[, 1])
  expect_equal(mod_kf$theta[, 1], mod_cholesky$theta[, 1])
})

test_that("test marginal cases", {
  x <- 1:n1 / (n1 + 1) * 2 * pi
  w0 <- rep(0, n1) # zero weights on all signals
  expect_no_error(trendfilter(
    y = rep(1, n1),
    x,
    weights = w0,
    k = k1,
    lambda = 10,
    control = trendfilter_control_list(
      admm_control = admm_control_list(linear_solver = "kalman_filter")
    )
  ))
  expect_no_error(trendfilter(
    y = rep(1, n1),
    x,
    weights = w0,
    k = k1,
    lambda = 10,
    control = trendfilter_control_list(
      admm_control = admm_control_list(linear_solver = "sparse_qr")
    )
  ))
  expect_no_error(trendfilter(
    y = rep(1, n1),
    x,
    weights = w0,
    k = k1,
    lambda = 10,
    control = trendfilter_control_list(
      admm_control = admm_control_list(linear_solver = "sparse_cholesky")
    )
  ))
})
