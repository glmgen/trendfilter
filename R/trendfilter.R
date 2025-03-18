#' Estimate the trendfilter
#'
#' @description
#' Given observations \eqn{y} at locations \eqn{x} the trendfilter
#' estimates the regression function \eqn{\theta(x) = E[Y\ |\ X = x]} by
#' by minimizing the smoothness-penalized negative log-likelihood
#' of the form:
#'
#' \deqn{\hat{\theta} = \arg\min_{\theta} \frac{1}{n} \sum_{i=1}^n (y_i -
#'   \theta_i)^2 + \lambda\Vert \mathbb{D}_n^{(k+1)}\theta\Vert_1, }
#'
#' where \eqn{\lambda} controls the balance between the fit to the data and the
#' amount of smoothness, and \eqn{\mathbb{D}_n^{(k+1)}} is the \eqn{(k+1)}-th order
#' discrete derivative matrix (a function of \eqn{x}).
#'
#' @param y vector of observations of length `n`
#' @param x vector of positions at which the `y` have been observed, defaults
#'   to `1:n`. These should be in increasing order, but will be sorted if
#'   necessary.
#' @param weights vector of weights for the observations, defaults to `rep(1, n)`.
#'   Note that internally, these are rescaled to sum to 1.
#' @param k Integer. Degree of the piecewise polynomial curve to be
#'   estimated. For example, `k = 0` corresponds to a piecewise constant
#'   curve.
#'
#' @param boundary_condition Logical. If `TRUE`, uses the Newton polynomial
#'   interpolation with divided difference at the boundary instead of the
#'   standard trend filtering formulation. Defaults to `FALSE`.
#' @param left_boundary_m Integer, optional value. Value has to be between
#'   1 to k-1, If Null, uses value round(k/2). Defaults to `FALSE`.
#' @param right_boundary_m Integer, optional value. Value has to be between
#'   1 to k-1, If Null, uses value round(k/2). Defaults to `FALSE`.
#'
#' @param family Character or function. Specifies the loss function
#'   to use. Valid options are:
#'   * `"gaussian"` - least squares loss (the default),
#'   * `"binomial"` - logistic loss (classification),
#'   * `"poisson"`  - Poisson loss for count data
#'
#'   For any other type, a valid [stats::family()] object may be passed. Note
#'   that these will generally be much slower to estimate than the built-in
#'   options passed as strings. So for example, `family = "gaussian"` and
#'   `family = gaussian()` will produce the same results, but the first
#'   will be much faster.character.
#'
#' @param method Character. Specifies the estimation algorithm to use.
#' @param nlambda Integer. Number of lambda values to use in the sequence.
#' @param lambda Vector. A user supplied sequence of tuning parameters which
#'   determines the balance between data fidelity and smoothness of the
#'   estimated curve; larger `lambda` results in a smoother estimate. The
#'   default, `NULL` results in an automatic computation based on `nlambda`,
#'   the largest value of `lambda` that would result in a maximally smooth
#'   estimate, and `lambda_min_ratio`. Supplying a value of `lambda` overrides
#'   this behaviour. It is likely better to supply a
#'   decreasing sequence of `lambda` values than a single (small) value. If
#'   supplied, the user-defined `lambda` sequence is automatically sorted in
#'   decreasing order.
#' @param lambda_max Optional value for the largest `lambda` to use.
#' @param lambda_min Optional value for the smallest `lambda` to use (> 0).
#' @param lambda_min_ratio If neither `lambda` nor `lambda_min` is specified,
#'   `lambda_min = lambda_max * lambda_min_ratio`.
#'   A very small value will lead to the solution `theta = y` (for the Gaussian
#'   loss). This argument has no effect if there is a user-defined `lambda`
#'   sequence.
#' @param standardize If `TRUE` (the default), `y` will be centered and scaled
#'   by its mean and standard deviation (and the operation inverted for `theta`
#'   internally). This can significantly speed convergence of the algorithm.
#' @param control A list of control parameters for the estimation algorithm.
#'   See the constructor [trendfilter_control_list()].

#'
#' @return An object with S3 class `trendfilter`. Among the list components:
#' * `y` the input data.
#' * `x` the vector of positions at which the data have been observed.
#' * `weights` the vector of observation weights
#' * `theta` the estimated curve evaluated at `x`. This is a matrix with
#'     each column corresponding to one value of `lambda`.
#' * `lambda` the values of `lambda` actually used in the algorithm.
#' * `korder` degree of the estimated piecewise polynomial curve.
#' * `dof` the estimated degrees of freedom of the solution
#' * `iters` the required number of iterations for each value of `lambda`.
#' * `objective` the value of the objective function for each value of `lambda`.
#' @export
#'
#' @seealso [tvdenoising::tvdenoising()]
#'
#' @references
#' Tibshirani (2014). "Adaptive piecewise polynomial estimation via trend
#'   filtering," _Annals of Statistics_, **42**(1):285–323.
#'   [Link](https://www.stat.berkeley.edu/~ryantibs/papers/dspline.pdf)
#'
#' Tibshirani (2022), "Divided differences, falling factorials, and
#'   discrete splines: Another look at trend filtering and related problems,"
#'  _Foundations and Trends® in Machine Learning_, **15**(6):694-846.
#'  [Link](https://www.stat.berkeley.edu/~ryantibs/papers/trendfilter.pdf)
#'
#' @examples
#' x <- 1:100 / 101 * 2 * pi
#' y <- sin(x) + .2 * rnorm(100)
#' out <- trendfilter(y, x)
#'
#' plot(out)
trendfilter <- function(y,
                        x = seq_along(y),
                        weights = rep(1, n),
                        k = 3L,
                        boundary_condition = FALSE,
                        left_boundary_m = NULL,
                        right_boundary_m = NULL,
                        family = c("gaussian", "logistic", "poisson"),
                        method = c("admm", "pdip", "hybrid"),
                        lambda = NULL,
                        nlambda = 50L,
                        lambda_max = NULL,
                        lambda_min = NULL,
                        lambda_min_ratio = 1e-5,
                        standardize = TRUE,
                        control = trendfilter_control_list()) {

  family <- arg_match(family)
  if (family != "gaussian") {
    cli_abort("Data family {.val {family}} is not yet implemented.")
  }
  method <- arg_match(method)
  if (method != "admm") {
    cli_abort("Estimation algorithm {.val {method}} is not yet implemented.")
  }
  n <- length(y)
  assert_numeric(y, finite = TRUE)
  assert_numeric(x, finite = TRUE, len = n)
  assert_numeric(weights, lower = 0, finite = TRUE, len = n)
  assert_integerish(k, lower = 0L, upper = n - 1L, len = 1L)
  assert_integerish(nlambda, lower = 1L, len = 1L)
  assert_numeric(lambda_max,
    len = 1L, lower = lambda_min %||% 0, finite = TRUE,
    null.ok = TRUE
  )
  assert_numeric(lambda_min,
    len = 1L, lower = 0, upper = lambda_max %||% Inf,
    finite = TRUE, null.ok = TRUE
  )
  assert_numeric(lambda_min_ratio, lower = 0, upper = 1, len = 1L)
  assert_numeric(lambda, finite = TRUE, lower = 0, null.ok = TRUE)
  assert_class(control, "trendfilter_control")

  lambda_min <- lambda_min %||% -1.0
  lambda_max <- lambda_max %||% -1.0
  lambda <- sort(lambda, decreasing = TRUE) %||% double(nlambda)
  nlambda <- length(lambda)

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


  ####
  # Ensure left_boundary_m is always set
  if (is.null(left_boundary_m)) {
    left_boundary_m <- ceiling(k/2)
  } else if (boundary_condition) {
    if (!is.numeric(left_boundary_m) || left_boundary_m != as.integer(left_boundary_m) ||
        left_boundary_m < 1 || left_boundary_m >= k) {
      cli_abort("Error: {.var left_boundary_m} must be an integer between 1 and (k-1), or NULL to use the default.")
    }
    left_boundary_m <- as.integer(left_boundary_m)
  }

  # Ensure right_boundary_m is always set
  if (is.null(right_boundary_m)) {
    right_boundary_m <- ceiling(k/2)
  } else if (boundary_condition) {
    if (!is.numeric(right_boundary_m) || right_boundary_m != as.integer(right_boundary_m) ||
        right_boundary_m < 1 || right_boundary_m >= k) {
      cli_abort("Error: {.var right_boundary_m} must be an integer between 1 and (k-1), or NULL to use the default.")
    }
    right_boundary_m <- as.integer(right_boundary_m)
  }



  out <- admm_lambda_seq(
    xsc, y, wsc, k, boundary_condition, left_boundary_m, right_boundary_m,
    lambda, nlambda, lambda_max, lambda_min, lambda_min_ratio,
    control$admm_control$max_iter, control$admm_control$rho_scale,
    control$admm_control$tolerance,
    if (k == 1L) 0L else match(control$admm_control$linear_solver, c("sparse_qr", "kalman_filter")),
    control$admm_control$space_tolerance_ratio
  )

  alpha <- NULL
  if (!is.null(out$alpha)) alpha <- drop(out$alpha) * ys

  structure(enlist(
    y = y * ys + ym,
    x,
    weights,
    k,
    theta = drop(out$theta) * ys + ym,
    alpha = alpha,
    lambda = out$lambda,
    iters = out$iters,
    objective = out$tf_objective,
    dof = out$dof,
    call = match.call()
  ), class = "trendfilter")

  }
