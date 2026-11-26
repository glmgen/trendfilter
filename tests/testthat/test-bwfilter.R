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

test_that("default path starts at the polynomial fit, then bw_lambda_max()", {
  no_stop <- bwfilter_control_list(devmax = 1, fdev = 0)
  for (k in 0:3) {
    fit <- bwfilter(y, x_uneven, w, k = k, compute_dof = TRUE, control = no_stop)
    expect_length(fit$lambda, 50L)
    expect_identical(fit$lambda[1], Inf)
    # lambda = Inf is the weighted least squares polynomial fit
    pfit <- if (k == 0) {
      rep(weighted.mean(y, w), n)
    } else {
      fitted(lm(y ~ poly(x_uneven, k), weights = w))
    }
    expect_equal(fit$theta[, 1], unname(pfit))
    expect_equal(fit$dof[1], k + 1)
    expect_equal(fit$objective[1], 0.5 * sum(w / sum(w) * (fit$theta[, 1] - y)^2) / var(y))
    lmax <- bw_lambda_max(n, k)
    expect_equal(fit$lambda[2], lmax)
    expect_equal(fit$lambda[50], 0.008 / (n * 4^(k + 1)))
    expect_false(is.unsorted(fit$dof))
    expect_lt(fit$dof[2], k + 1.5)
    expect_gt(fit$dof[50], 0.85 * n)
  }
})

test_that("bw_lambda_max() is the smaller of the formula and the solver cap", {
  expect_equal(bw_lambda_max(50, 3), 8 * 50^7 / (2.5 * pi)^8)
  expect_equal(bw_lambda_max(5000, 3), 1e16 / (2 * 5000 * 4^4))
  expect_equal(bw_lambda_max(5000, 3, "sparse_qr"), 1e12 / (2 * 5000 * 4^4))
  fit <- bwfilter(
    y,
    nlambda = 3L,
    control = bwfilter_control_list(linear_solver = "sparse_cholesky")
  )
  expect_equal(fit$lambda[2], bw_lambda_max(n, 3, "sparse_cholesky"))
})

test_that("prediction handles lambda = Inf and interpolates in log(lambda)", {
  fit <- bwfilter(y, k = 2)
  p <- predict(fit, lambda = c(Inf, fit$lambda[2], fit$lambda[5]))
  expect_equal(p, fit$theta[, c(1, 2, 5)])
  # between Inf and lambda[2], weight lambda[2] / lambda on the second fit
  p <- predict(fit, lambda = 4 * fit$lambda[2])
  expect_equal(p, 0.75 * fit$theta[, 1] + 0.25 * fit$theta[, 2])
  mid <- sqrt(fit$lambda[30] * fit$lambda[31])
  expect_equal(
    predict(fit, lambda = mid),
    (fit$theta[, 30] + fit$theta[, 31]) / 2
  )
  expect_no_warning(predict(fit))
})

test_that("lambda = 0 interpolates and nlambda = 1 works", {
  fit <- bwfilter(y, lambda = 0, compute_dof = TRUE)
  expect_equal(fit$theta, y)
  expect_equal(fit$dof, n)
  expect_null(bwfilter(y, lambda = 0)$dof)
  fit <- bwfilter(
    y,
    k = 1, lambda_min_ratio = 1e-4,
    control = bwfilter_control_list(devmax = 1, fdev = 0)
  )
  expect_equal(fit$lambda[50] / fit$lambda[2], 1e-4)
  expect_identical(bwfilter(y, nlambda = 1L)$lambda, Inf)
  expect_identical(bwfilter(y, nlambda = 2L)$lambda, c(Inf, bw_lambda_max(n, 3)))
  # Inf may also be supplied by the user
  fit <- bwfilter(y, lambda = c(1, Inf))
  expect_equal(fit$lambda, c(Inf, 1))
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
  # default lambda sequence is valid with zero weights
  fit0 <- bwfilter(y, weights = w0)
  expect_identical(fit0$lambda[1], Inf)
  expect_true(all(is.finite(fit0$lambda[-1]) & fit0$lambda[-1] > 0))
  expect_false(anyNA(fit0$theta))
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

test_that("default path stops early as in glmnet, user lambda does not", {
  fit <- bwfilter(y, k = 3)
  nfit <- length(fit$lambda)
  expect_lt(nfit, 50L)
  expect_gte(nfit, 5L)
  expect_equal(ncol(fit$theta), nfit)
  expect_length(fit$objective, nfit)
  expect_length(fit$dev_ratio, nfit)
  expect_false(is.unsorted(fit$dev_ratio))
  # stopped by one of the two rules, and not before
  dr <- fit$dev_ratio
  expect_true(dr[nfit] > 0.999 || dr[nfit] - dr[nfit - 1] < 1e-5 * dr[nfit])
  expect_true(all(dr[-nfit] <= 0.999))
  # dev_ratio is the weighted fraction of deviance explained
  expect_equal(
    fit$dev_ratio[3],
    1 - sum((y - fit$theta[, 3])^2) / sum((y - mean(y))^2)
  )
  # a stricter devmax stops sooner; mnlam is respected
  fit2 <- bwfilter(y, k = 3, control = bwfilter_control_list(devmax = 0.9))
  expect_lt(length(fit2$lambda), nfit)
  fit3 <- bwfilter(
    y,
    k = 3, control = bwfilter_control_list(devmax = 0, mnlam = 7)
  )
  expect_length(fit3$lambda, 7L)
  # no early stopping for a user-supplied lambda
  fit4 <- bwfilter(y, k = 3, lambda = bwfilter(
    y,
    k = 3, control = bwfilter_control_list(devmax = 1, fdev = 0)
  )$lambda)
  expect_length(fit4$lambda, 50L)
  expect_no_warning(bwfilter(y, k = 3))
})
