#' ADMM control list
#'
#' These arguments are used to handle the internals of the ADMM algorithm
#' used to solve the trendfilter optimization problem.
#'
#' @param max_iter Integer. Maximum number of iterations per `lambda` value.
#' @param rho_scale Double. The ratio of `lambda` divided by the augmented
#'   Lagrangian penalty parameter \eqn{\rho}.
#' @param tolerance Double. The convergence tolerance for the ADMM algorithm.
#' @param linear_solver Integer. Solver for the linear system in ADMM when
#'   k > 1: `kalman_filter` for Kalman filter, `sparse_qr` for sparse QR
#'   decomposition, or `sparse_cholesky` for Cholesky.
#' @param space_tolerance_ratio Double. The tolerance ratio of to detect that
#'   the signal is equally spaced.
#' @param ... not used
#'
#' @return an object of class `admm_control`
#' @export
#'
#' @examples
#' admm_control_list()
#' admm_control_list(max_iter = 10L)
#' admm_control_list(tolerance = 1e-8)
admm_control_list <- function(
  max_iter = 1e4,
  rho_scale = 1.0,
  tolerance = 1e-4,
  linear_solver = c("kalman_filter", "sparse_qr", "sparse_cholesky"),
  space_tolerance_ratio = sqrt(.Machine$double.eps),
  ...
) {
  rlang::check_dots_empty()
  assert_integerish(max_iter, lower = 1L, len = 1L)
  assert_numeric(
    rho_scale,
    lower = .Machine$double.eps,
    finite = TRUE,
    len = 1L
  )
  assert_numeric(
    tolerance,
    lower = .Machine$double.eps,
    finite = TRUE,
    len = 1L
  )
  linear_solver <- rlang::arg_match(linear_solver)
  assert_numeric(space_tolerance_ratio, lower = 0, finite = TRUE, len = 1L)
  structure(
    enlist(
      max_iter,
      rho_scale,
      tolerance,
      linear_solver,
      space_tolerance_ratio
    ),
    class = "admm_control"
  )
}

#' @export
#' @method print admm_control
print.admm_control <- function(x, prefix = "An", ...) {
  rlang::check_dots_empty()
  cli::cli_h2(paste(prefix, "ADMM configuration"))
  d <- cli::cli_div(theme = list(span.dt = list(color = "cornflowerblue")))
  cli::cli_dl(x)
  cli::cli_end(d)
}

#' Trendfilter control list
#'
#' These arguments are used to handle the internals of [trendfilter()]
#'
#' @param obj_tol Double. The convergence tolerance for the objective function.
#' @param x_cond Double. The conditioning of the `x` values
#' @param admm_control An object of class `admm_control` used to control
#'   as created by [admm_control_list()]
#' @param ... not used
#'
#' @return An object of class `trendfilter_control`
#' @export
#'
#' @examples
#' trendfilter_control_list()
#' trendfilter_control_list(obj_tol = 1e-12)
#' trendfilter_control_list(admm_control = admm_control_list(tolerance = 1e-5))
trendfilter_control_list <- function(
  obj_tol = 1e-6,
  x_cond = 1e11,
  admm_control = admm_control_list(),
  ...
) {
  rlang::check_dots_empty()
  assert_numeric(obj_tol, lower = .Machine$double.eps, finite = TRUE, len = 1L)
  assert_numeric(x_cond, lower = 1, finite = TRUE, len = 1L)
  assert_class(admm_control, "admm_control")
  structure(
    enlist(obj_tol, x_cond, admm_control),
    class = "trendfilter_control"
  )
}

#' @export
#' @method print trendfilter_control
print.trendfilter_control <- function(x, ...) {
  rlang::check_dots_empty()
  cli::cli_h2("A `trendfilter()` configuration")
  d <- cli::cli_div(theme = list(span.dt = list(color = "cornflowerblue")))
  cli::cli_dl(x[1:2])
  cli::cli_end(d)
  print(x[[3]], prefix = "with an")
}
