bw_dense <- function(y, x, w, k, lam) {
  n <- length(y)
  wsc <- w / sum(w)
  xsc <- (x - min(x)) / diff(range(x)) * n
  D <- as.matrix(dspline::d_mat(k + 1, xsc, tf_weighting = TRUE))
  W <- diag(wsc)
  A <- W + 2 * lam * crossprod(D)
  list(theta = drop(solve(A, W %*% y)), dof = sum(diag(solve(A, W))))
}

set.seed(12345)
n <- 50L
x_even <- 1:n
x_uneven <- sort(runif(n, 0, 10))
y <- sin(x_even / 5) + rnorm(n, sd = .3)
w <- rpois(n, 4) + 1
lam <- c(5, .5, .05)

test_that("bwfilter matches the closed-form solution for all solvers", {
  for (k in 0:3) {
    for (x in list(x_even, x_uneven)) {
      for (s in c("kalman_filter", "sparse_qr", "sparse_cholesky")) {
        fit <- bwfilter(
          y, x, w,
          k = k, lambda = lam, standardize = FALSE, compute_dof = TRUE,
          control = bwfilter_control_list(linear_solver = s)
        )
        for (i in seq_along(lam)) {
          r <- bw_dense(y, x, w, k, lam[i])
          expect_equal(fit$theta[, i], r$theta, tolerance = 1e-5)
          expect_equal(fit$dof[i], r$dof, tolerance = 1e-5)
        }
      }
    }
  }
})

test_that("standardization does not change the fit", {
  f1 <- bwfilter(y, lambda = lam, standardize = TRUE)
  f2 <- bwfilter(y, lambda = lam, standardize = FALSE)
  expect_equal(f1$theta, f2$theta, tolerance = 1e-8)
})

test_that("default lambda path follows the (scaled) glmnet ridge rule", {
  ys <- (y - mean(y)) / sd(y)
  for (k in 0:3) {
    fit <- bwfilter(y, k = k, compute_dof = TRUE)
    xsc <- (x_even - 1) / (n - 1) * n
    lmax <- get_lambda_max(xsc, ys, rep(1 / n, n), k) / 1e-3
    expect_length(fit$lambda, 50L)
    expect_equal(fit$lambda[1], lmax)
    expect_equal(fit$lambda[50], lmax * 1e-4^(k + 1))
    expect_false(is.unsorted(fit$dof))
    expect_true(all(fit$dof > k + 1))
  }
})

test_that("lambda = 0 interpolates and nlambda = 1 works", {
  fit <- bwfilter(y, lambda = 0, compute_dof = TRUE)
  expect_equal(fit$theta, y)
  expect_equal(fit$dof, n)
  expect_null(bwfilter(y, lambda = 0)$dof)
  fit <- bwfilter(y, k = 1, lambda_min_ratio = 1e-4)
  expect_equal(fit$lambda[50] / fit$lambda[1], 1e-4)
  fit <- bwfilter(y, nlambda = 1L)
  expect_length(fit$lambda, 1L)
  expect_gt(fit$lambda, 0)
})

test_that("unsorted x is handled and zero weights are allowed", {
  ord <- sample(n)
  f1 <- bwfilter(y, x_uneven, w, lambda = lam)
  f2 <- bwfilter(y[ord], x_uneven[ord], w[ord], lambda = lam)
  expect_equal(f1$theta, f2$theta)

  w0 <- w
  w0[c(1, 3, 20, n)] <- 0
  for (k in 0:3) {
    for (s in c("kalman_filter", "sparse_qr", "sparse_cholesky")) {
      fit <- bwfilter(
        y, x_even, w0,
        k = k, lambda = lam, standardize = FALSE, compute_dof = TRUE,
        control = bwfilter_control_list(linear_solver = s)
      )
      for (i in seq_along(lam)) {
        r <- bw_dense(y, x_even, w0, k, lam[i])
        expect_equal(fit$theta[, i], r$theta, tolerance = 1e-6)
        expect_equal(fit$dof[i], r$dof, tolerance = 1e-6)
      }
    }
  }
  # default lambda sequence is finite with zero weights
  lam0 <- bwfilter(y, weights = w0)$lambda
  expect_true(all(is.finite(lam0) & lam0 > 0))
  lam0 <- trendfilter(y, weights = w0)$lambda
  expect_true(all(is.finite(lam0) & lam0 > 0))
})

test_that("bwfilter objects work with trendfilter methods", {
  fit <- bwfilter(y, lambda = lam)
  expect_s3_class(fit, c("bwfilter", "trendfilter"))
  expect_equal(dim(predict(fit, newx = c(2.5, 10.1))), c(2L, 3L))
  expect_s3_class(plot(fit), "ggplot")
})

test_that("bwfilter argument checks", {
  expect_error(bwfilter(y, k = n - 1L))
  expect_error(bwfilter(y, x = c(1, 1:(n - 1))))
  expect_error(bwfilter(y, weights = c(rep(0, n - 2), 1, 1), k = 1))
  expect_error(bwfilter(y, control = trendfilter_control_list()))
  expect_error(bwfilter_control_list(linear_solver = "foo"))
})

test_that("Kalman filter handles zero weights in the diffuse initialization", {
  w0 <- rep(1, n)
  w0[1:2] <- 0
  fit <- trendfilter(y, weights = w0, k = 2, lambda = c(1, .1))
  expect_false(anyNA(fit$theta))
})
