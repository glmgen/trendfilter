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
#' @param lambda_max Optional value for the largest `lambda` to use. By
#'   default, this follows the rule of thumb used for ridge regression in
#'   `glmnet`: the largest `lambda` that would be used by [trendfilter()] (the
#'   smallest value giving a polynomial fit under the \eqn{\ell_1} penalty)
#'   divided by `0.001`.
#' @param lambda_min Optional value for the smallest `lambda` to use (> 0).
#' @param lambda_min_ratio If neither `lambda` nor `lambda_min` is specified,
#'   `lambda_min = lambda_max * lambda_min_ratio`. The default, `NULL`, uses
#'   `1e-4^(k + 1)`: `glmnet`'s ridge default raised to the power `k + 1`. The
#'   effective degrees of freedom grow roughly like
#'   \eqn{\lambda^{-1/(2(k+1))}}, so this gives a similar range of fits for
#'   every `k`. This argument has no effect if there is a user-defined
#'   `lambda` sequence.
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
#' * `lambda` the values of `lambda` actually used in the algorithm.
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
  lambda_max = NULL,
  lambda_min = NULL,
  lambda_min_ratio = NULL,
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
    finite = TRUE,
    null.ok = TRUE
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
    len = 1L,
    null.ok = TRUE
  )
  assert_logical(compute_dof, len = 1L, any.missing = FALSE)
  assert_numeric(lambda, finite = TRUE, lower = 0, null.ok = TRUE)
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

  if (is.null(lambda)) {
    # glmnet's ridge rule of thumb: the l1 lambda_max divided by 1e-3
    lambda_max <- lambda_max %||% (get_lambda_max(xsc, y, wsc, k) / 1e-3)
    lambda_min_ratio <- lambda_min_ratio %||% 1e-4^(k + 1)
    lambda <- if (nlambda == 1L) {
      lambda_max
    } else {
      get_lambda_seq_r(
        double(nlambda),
        lambda_max,
        lambda_min %||% -1.0,
        lambda_min_ratio,
        nlambda
      )
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
    control$space_tolerance_ratio
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
      dof = if (compute_dof) bw_dof(bw_spectrum(xsc, wsc, k), out$lambda),
      call = match.call()
    ),
    class = c("bwfilter", "trendfilter")
  )
}

# Exact effective degrees of freedom tr(S_lambda), where S_lambda has
# eigenvalues (1 - tau) / (1 - tau + 2 * lambda * tau / c)
bw_dof <- function(spec, lambda) {
  num <- 1 - spec$tau
  vapply(
    lambda,
    function(lam) {
      den <- num + 2 * lam * spec$tau / spec$c
      sum(ifelse(num > 0, num / den, 0))
    },
    double(1)
  )
}
