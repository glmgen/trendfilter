#' Estimate the Butterworth-type (squared ell-2) filter
#'
#' @description
#' Given observations \eqn{y} at locations \eqn{x}, `bwfilter()` estimates the
#' regression function \eqn{\theta(x) = E[Y\ |\ X = x]} by solving the same
#' problem as [trendfilter()], but with a squared \eqn{\ell_2} penalty in place
#' of the \eqn{\ell_1} penalty:
#'
#' \deqn{\hat{\theta} = \arg\min_{\theta} \frac{1}{2} \sum_{i=1}^n w_i (y_i -
#'   \theta_i)^2 + \lambda\Vert \mathbb{D}_n^{(k+1)}\theta\Vert_2^2, }
#'
#' where \eqn{\lambda} controls the balance between the fit to the data and the
#' amount of smoothness, \eqn{\mathbb{D}_n^{(k+1)}} is the \eqn{(k+1)}-th order
#' discrete derivative matrix (a function of \eqn{x}), and the weights are
#' rescaled to sum to 1. For evenly spaced data and `k = 1` this is the
#' Hodrick-Prescott filter; more generally it is a discrete analogue of a
#' smoothing spline (or Butterworth filter).
#'
#' Because the penalty is quadratic, the solution is available in closed form,
#' \eqn{\hat\theta = (W + 2\lambda D^\top D)^{-1} W y}, so each value of
#' `lambda` requires only a single banded linear solve (no ADMM iterations).
#'
#' @inheritParams trendfilter
#' @param k Integer. Order of the penalty is `k + 1`, as in [trendfilter()].
#'   Must be at most `n - 2`.
#' @param lambda Vector. A user supplied sequence of tuning parameters. It is
#'   sorted in decreasing order and may include `Inf`, which gives the
#'   weighted least squares polynomial fit of degree `k`. The default, `NULL`,
#'   uses `Inf` followed by `nlambda - 1` values decreasing geometrically from
#'   `lambda_max` to `lambda_min`.
#' @param nlambda Integer. Number of `lambda` values in the default sequence,
#'   including `Inf`.
#' @param lambda_max The largest finite `lambda` in the default sequence. The
#'   default, [bw_lambda_max()], depends only on `n`, `k`, and the solver.
#' @param lambda_min Optional value for the smallest `lambda` to use (> 0).
#' @param lambda_min_ratio If neither `lambda` nor `lambda_min` is specified,
#'   `lambda_min = lambda_max * lambda_min_ratio`. The default gives
#'   \eqn{\lambda_{\min} = 0.008 / (n 4^{k+1})}, which shrinks even the
#'   roughest component of `y` by under 2%, so the fit nearly interpolates.
#'   The effective degrees of freedom grow roughly like
#'   \eqn{\lambda^{-1/(2(k+1))}}, so the default sequence is spread evenly in
#'   log(dof). It usually stops before reaching `lambda_min` (see
#'   [bwfilter_control_list()]). This argument has no effect if there is a
#'   user-defined `lambda` sequence.
#' @param compute_dof Logical. Should the effective degrees of freedom be
#'   computed? This requires the eigenvalues of a banded \eqn{n \times n}
#'   generalized eigenproblem, which costs \eqn{O(n^2 k)} time (compared to
#'   \eqn{O(n k^2)} per `lambda` for the fit itself), so it is off by default.
#'   Once computed, the degrees of freedom for all `lambda` values are cheap.
#' @param control A list of control parameters for the linear solver.
#'   See the constructor [bwfilter_control_list()].
#'
#' @return An object with S3 class `bwfilter` (inheriting from `trendfilter`).
#'   Among the list components:
#' * `y` the input data.
#' * `x` the vector of positions at which the data have been observed.
#' * `weights` the vector of observation weights
#' * `theta` the estimated curve evaluated at `x`. This is a matrix with
#'     each column corresponding to one value of `lambda`.
#' * `lambda` the values of `lambda` actually used in the algorithm. With the
#'     default sequence, this may be shorter than `nlambda`: as in `glmnet`,
#'     the path stops early once the fits are nearly interpolating (see
#'     [bwfilter_control_list()]).
#' * `dev_ratio` the fraction of (weighted) deviance explained,
#'     \eqn{1 - \sum_i w_i (y_i - \hat\theta_i)^2 / \sum_i w_i (y_i -
#'     \bar{y})^2}, for each `lambda`.
#' * `k` order of the penalty minus 1.
#' * `dof` if `compute_dof = TRUE`, the (exact) effective degrees of freedom
#'     of the linear smoother, \eqn{\mathrm{tr}((W + 2\lambda D^\top D)^{-1}
#'     W)}, for each `lambda`. Otherwise `NULL`.
#' * `objective` the value of the objective function for each value of `lambda`
#'     (on the standardized scale if `standardize = TRUE`).
#' @export
#'
#' @seealso [trendfilter()]
#'
#' @references
#' Tibshirani (2022), "Divided differences, falling factorials, and
#'   discrete splines: Another look at trend filtering and related problems,"
#'  _Foundations and Trends® in Machine Learning_, **15**(6):694-846.
#'  [Link](https://www.stat.berkeley.edu/~ryantibs/papers/trendfilter.pdf)
#'
#' @examples
#' x <- 1:100 / 101 * 2 * pi
#' y <- sin(x) + .2 * rnorm(100)
#' out <- bwfilter(y, x)
#' bwfilter(y, x, nlambda = 5, compute_dof = TRUE)$dof
#'
#' plot(out)
bwfilter <- function(
  y,
  x = seq_along(y),
  weights = rep(1, n),
  k = 3L,
  lambda = NULL,
  nlambda = 50L,
  lambda_max = bw_lambda_max(n, k, control$linear_solver),
  lambda_min = NULL,
  lambda_min_ratio = 0.008 / (n * 4^(k + 1) * lambda_max),
  standardize = TRUE,
  compute_dof = FALSE,
  control = bwfilter_control_list()
) {
  n <- length(y)
  assert_numeric(y, finite = TRUE)
  assert_numeric(x, finite = TRUE, len = n)
  assert_numeric(weights, lower = 0, finite = TRUE, len = n)
  assert_integerish(k, lower = 0L, upper = n - 2L, len = 1L)
  if (sum(weights > 0) < k + 2L) {
    cli_abort("At least {.val {k + 2}} observations must have positive weight.")
  }
  if (anyDuplicated(x)) {
    cli_abort("The values of {.arg x} must be unique.")
  }
  assert_integerish(nlambda, lower = 1L, len = 1L)
  assert_numeric(
    lambda_max,
    len = 1L,
    lower = lambda_min %||% 0,
    finite = TRUE
  )
  assert_numeric(
    lambda_min,
    len = 1L,
    lower = 0,
    upper = lambda_max %||% Inf,
    finite = TRUE,
    null.ok = TRUE
  )
  assert_numeric(
    lambda_min_ratio,
    lower = 0,
    upper = 1,
    len = 1L
  )
  assert_logical(compute_dof, len = 1L, any.missing = FALSE)
  assert_numeric(lambda, lower = 0, any.missing = FALSE, null.ok = TRUE)
  assert_class(control, "bwfilter_control")

  if (is.unsorted(x)) {
    ord <- order(x)
    y <- y[ord]
    x <- x[ord]
    weights <- weights[ord]
  }

  wsc <- weights / sum(weights)
  xsc <- (x - min(x)) / diff(range(x)) * n
  ym <- 0
  ys <- 1
  if (standardize) {
    ym <- mean(y)
    ys <- stats::sd(y)
    y <- (y - ym) / ys
  }

  early_stop <- is.null(lambda)
  if (early_stop) {
    # Start from the polynomial fit (lambda = Inf), computed directly
    lambda <- if (nlambda == 1L) {
      Inf
    } else if (nlambda == 2L) {
      c(Inf, lambda_max)
    } else {
      c(Inf, get_lambda_seq_r(
        double(nlambda - 1L),
        lambda_max,
        lambda_min %||% -1.0,
        lambda_min_ratio,
        nlambda - 1L
      ))
    }
  } else {
    lambda <- sort(lambda, decreasing = TRUE)
  }

  linear_solver_int <- match(
    control$linear_solver,
    c("sparse_qr", "kalman_filter", "sparse_cholesky")
  )
  out <- bw_lambda_seq(
    xsc,
    y,
    wsc,
    k,
    lambda,
    linear_solver_int,
    control$space_tolerance_ratio,
    early_stop,
    control$devmax,
    control$fdev,
    control$mnlam
  )

  structure(
    enlist(
      y = y * ys + ym,
      x,
      weights,
      k,
      theta = drop(out$theta) * ys + ym,
      lambda = out$lambda,
      objective = out$objective,
      dev_ratio = out$dev_ratio,
      dof = if (compute_dof) bw_dof(bw_spectrum(xsc, wsc, k), out$lambda, k),
      call = match.call()
    ),
    class = c("bwfilter", "trendfilter")
  )
}

# Exact effective degrees of freedom tr(S_lambda), where S_lambda has
# eigenvalues (1 - tau) / (1 - tau + 2 * lambda * tau / c)
bw_dof <- function(spec, lambda, k) {
  num <- 1 - spec$tau
  vapply(
    lambda,
    function(lam) {
      if (is.infinite(lam)) {
        return(k + 1)
      }
      den <- num + 2 * lam * spec$tau / spec$c
      sum(ifelse(num > 0, num / den, 0))
    },
    double(1)
  )
}

#' Default largest finite lambda for `bwfilter()`
#'
#' The default sequence in [bwfilter()] starts at `lambda = Inf` (the
#' polynomial fit, computed directly). The next value is the smaller of
#' * \eqn{8 n^{2k+1} / ((k+2)\pi/2)^{2(k+1)}}, which gives a fit with
#'   about `k + 1.1` effective degrees of freedom (from the smallest nonzero
#'   eigenvalue of \eqn{D^\top D}, roughly \eqn{((k+2)\pi / (2n))^{2(k+1)}}),
#' * \eqn{\kappa / (2 n 4^{k+1})}, which keeps the condition number of
#'   \eqn{W + 2\lambda D^\top D} below about \eqn{\kappa}, where the
#'   solver is still accurate: \eqn{\kappa = 10^{16}} for the Kalman filter
#'   and \eqn{10^{12}} for sparse QR or Cholesky.
#'
#' For small `n` and `k` the first is smaller. Otherwise there is a gap in the
#' path between the polynomial fit and the second fit.
#'
#' Internally `x` is rescaled to span `n` and the weights to sum to 1, so
#' these values do not depend on the scale of `x`, `y`, or `weights`.
#'
#' @param n Number of observations.
#' @param k Integer, as in [bwfilter()].
#' @param linear_solver Character, as in [bwfilter_control_list()].
#'
#' @return A single positive number.
#' @export
#' @examples
#' bw_lambda_max(100, 3)
#' bw_lambda_max(5000, 3, "sparse_qr")
bw_lambda_max <- function(
  n,
  k,
  linear_solver = c("kalman_filter", "sparse_qr", "sparse_cholesky")
) {
  linear_solver <- rlang::arg_match(linear_solver)
  near_poly <- 8 * n^(2 * k + 1) / ((k + 2) * pi / 2)^(2 * k + 2)
  kappa <- if (linear_solver == "kalman_filter") 1e16 else 1e12
  min(near_poly, kappa / (2 * n * 4^(k + 1)))
}
