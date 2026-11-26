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
          k = k, lambda = lam, standardize = FALSE,
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

test_that("default lambda path spans polynomial to interpolation", {
  for (k in 0:3) {
    fit <- bwfilter(y, k = k)
    expect_length(fit$lambda, 50L)
    expect_false(is.unsorted(rev(fit$lambda)))
    expect_lt(fit$dof[1], k + 2)
    expect_gt(fit$dof[50], n - 2)
    expect_false(is.unsorted(fit$dof))
    # largest lambda is (nearly) a polynomial of degree k
    pfit <- if (k == 0) rep(mean(y), n) else fitted(lm(y ~ poly(x_even, k)))
    expect_equal(fit$theta[, 1], unname(pfit), tolerance = 0.05)
  }
})

test_that("lambda = 0 interpolates and nlambda = 1 works", {
  fit <- bwfilter(y, lambda = 0)
  expect_equal(fit$theta, y)
  expect_equal(fit$dof, n)
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
  w0[c(3, 20)] <- 0
  fit <- bwfilter(y, x_even, w0, k = 2, lambda = lam, standardize = FALSE)
  for (i in seq_along(lam)) {
    r <- bw_dense(y, x_even, w0, 2, lam[i])
    expect_equal(fit$theta[, i], r$theta, tolerance = 1e-6)
    expect_equal(fit$dof[i], r$dof, tolerance = 1e-6)
  }
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
