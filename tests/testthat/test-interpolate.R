# The previous implementation, which normalized lambda to [0, 1] first
interpolate_lambda_normalized <- function(lambda, new_lambda) {
  k <- length(lambda)
  sfrac <- (lambda[1] - new_lambda) / (lambda[1] - lambda[k])
  lambda <- (lambda[1] - lambda) / (lambda[1] - lambda[k])
  sfrac[sfrac < min(lambda)] <- min(lambda)
  sfrac[sfrac > max(lambda)] <- max(lambda)
  coord <- stats::approx(lambda, seq(lambda), sfrac)$y
  left <- floor(coord)
  right <- ceiling(coord)
  sfrac <- (sfrac - lambda[right]) / (lambda[left] - lambda[right])
  sfrac[left == right] <- 1
  sfrac[abs(lambda[left] - lambda[right]) < .Machine$double.eps] <- 1
  list(left = left, right = right, frac = sfrac)
}

test_that("interpolation matches the normalized version on a typical range", {
  lambda <- 10^seq(2, -3, length.out = 30)
  new <- c(lambda[c(1, 7, 30)], 10^runif(20, -3, 2), 1e3, 1e-4)
  expect_equal(
    interpolate_lambda(lambda, new),
    interpolate_lambda_normalized(lambda, new)
  )
})

test_that("linear interpolation stays exact over a very wide range", {
  lambda <- 10^seq(0, -24, by = -1)
  new <- c(3e-20, 1e-20, 5.5e-23, 1)
  out <- interpolate_lambda(lambda, new)
  # a linear function of lambda is reproduced exactly
  vals <- out$frac * lambda[out$left] + (1 - out$frac) * lambda[out$right]
  expect_equal(vals, new, tolerance = 1e-12)
  expect_equal(out$left, c(20, 21, 23, 1))
  expect_equal(out$right, c(21, 21, 24, 1))
  expect_no_warning(interpolate_lambda(lambda, new))
})

test_that("trendfilter predictions are linear in lambda between grid values", {
  set.seed(1)
  y <- sin(1:60 / 8) + rnorm(60, sd = .2)
  fit <- trendfilter(y, k = 2, nlambda = 10)
  expect_equal(predict(fit, lambda = fit$lambda[c(2, 7)]), fit$theta[, c(2, 7)])
  mid <- (fit$lambda[4] + fit$lambda[5]) / 2
  expect_equal(
    predict(fit, lambda = mid),
    (fit$theta[, 4] + fit$theta[, 5]) / 2
  )
})
