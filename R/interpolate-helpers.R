interpolate_lambda <- function(lambda, new_lambda, log_scale = FALSE) {
  # A leading Inf (the polynomial fit in `bwfilter()`): for large lambda the
  # fit departs from its limit like 1 / lambda, so between Inf and lambda[2]
  # weight the second fit by lambda[2] / new_lambda.
  if (length(lambda) > 1 && is.infinite(lambda[1])) {
    above <- new_lambda >= lambda[2]
    out <- list(
      left = rep(1, length(new_lambda)),
      right = rep(2, length(new_lambda)),
      frac = 1 - lambda[2] / new_lambda
    )
    if (any(!above)) {
      rest <- interpolate_lambda(lambda[-1], new_lambda[!above], log_scale)
      out$left[!above] <- rest$left + 1
      out$right[!above] <- rest$right + 1
      out$frac[!above] <- rest$frac
    }
    return(out)
  }
  if (length(lambda) == 1) {
    nums <- length(new_lambda)
    left <- rep(1, nums)
    right <- left
    sfrac <- rep(1, nums)
  } else {
    # Interpolate in log(lambda) when the sequence spans many orders of
    # magnitude (`bwfilter()`); a linear scale can't distinguish the small
    # values there.
    if (log_scale && all(lambda > 0)) {
      lambda <- log(lambda)
      new_lambda <- log(new_lambda)
    }
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
  }
  list(left = left, right = right, frac = sfrac)
}

interpolate_mat <- function(mat, lam_list, lambda) {
  m <- length(lambda)
  out <- mat[, lam_list$left, drop = FALSE] %*% diag(lam_list$frac, m, m) +
    mat[, lam_list$right, drop = FALSE] %*% diag(1 - lam_list$frac, m, m)
  drop(out)
}
