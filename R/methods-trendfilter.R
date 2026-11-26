#' Predict with trendfilter at new (interior) design points
#'
#' @param object the result of [trendfilter()]
#' @param newx numeric vector of new design points at which to evaluate the. The
#'   default, `NULL` returns the estimates at the original `x` values.
#' @inheritParams trendfilter
#' @param deriv integer; the order of the derivative to be evaluated. Default is 0.
#' @param ... not used
#'
#' @return a vector or matrix with rows corresponding to `newx` and columns
#'   corresponding to `lambda`
#'
#' @importFrom stats predict
#' @export
#' @examples
#' x <- 1:100 / 101 * 2 * pi
#' y <- sin(x) + .2 * rnorm(100)
#' out <- trendfilter(y, x, nlambda = 20L)
#' predict(out, newx = 1:6, lambda = out$lambda[10])
#' predict(out, newx = 1:6)
predict.trendfilter <- function(object, newx = NULL, lambda = NULL, deriv = 0L, ...) {
  rlang::check_dots_empty()
  newx <- newx %||% object$x
  lambda <- lambda %||% object$lambda
  assert_numeric(newx, lower = min(object$x), upper = max(object$x))
  assert_numeric(lambda, lower = min(object$lambda), upper = max(object$lambda))

  if (is.null(newx) && is.null(lambda) && deriv == 0L) {
    return(object$theta)
  }

  # 1. we interpolate at newx for ALL lambda
  interp <- apply(as.matrix(object$theta), 2, function(th) {
    dspline_interp(th, object$k, object$x, newx)
  })
  # 2. interpolate the solutions to lambdas off the original values
  lam_list <- interpolate_lambda(
    object$lambda,
    lambda,
    log_scale = inherits(object, "bwfilter")
  )
  fits <- interpolate_mat(interp, lam_list, lambda)
  drop(fits)
}

#' @method summary trendfilter
#' @export
summary.trendfilter <- function(object, ...) {
  rlang::check_dots_empty()
  ns <- length(object$lambda)
  if (ns > 5) {
    xlam <- round(stats::quantile(1:ns))
    names(xlam) <- c("Max.", "3rd Qu.", "Median", "1st Qu.", "Min.")
  } else {
    xlam <- seq_len(ns)
    names(xlam) <- paste0("s", seq_len(ns))
  }
  tab <- with(object, data.frame(
    lambda = lambda[xlam],
    index = xlam
    # approx_dof = dof[xlam],
    # niterations = niter[xlam]
  ))
  rownames(tab) <- names(xlam)
  out <- structure(
    list(call = object$call, table = tab, k = object$k, nlam = ns),
    class = "summary.trendfilter"
  )
  out
}

#' @method print summary.trendfilter
#' @export
print.summary.trendfilter <- function(x,
                                      digits = max(3, getOption("digits") - 3),
                                      ...) {
  rlang::check_dots_empty()
  cat("\nCall: ", deparse(x$call), fill = TRUE)
  cat("\nDegree of the estimated piecewise polynomial curve (k):", x$k, "\n")
  cat("\nSummary of the", x$nlam, "estimated models:\n")
  print(x$tab, digits = digits)
  cat("\n")
}

#' @method print trendfilter
#' @export
print.trendfilter <- function(x, digits = min(3, getOption("digits") - 3), ...) {
  rlang::check_dots_empty()
  print(summary(x), digits = digits)
}

#' Plot estimated values from a `trendfilter` object
#'
#' Produces a figure showing some or all estimated values for different
#' values of the penalty. The result is a [ggplot2::ggplot()]. Additional user
#' modifications can be added as desired.
#'
#'
#' @param x output of the function [trendfilter()] of class `trendfilter`
#' @param lambda select which estimates to plot. If not provided,
#'   all estimates are plotted.
#' @param ... Not used.
#'
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' x <- 1:100 / 101 * 2 * pi
#' y <- sin(x) + .2 * rnorm(100)
#' out <- trendfilter(y, x, lambda = c(12, 1.2, .12))
#' plot(out)
plot.trendfilter <- function(x, lambda = NULL, ...) {
  rlang::check_dots_empty()
  assert_numeric(lambda, lower = min(x$lambda), upper = max(x$lambda), null.ok = TRUE)

  n <- length(x$y)
  lambda <- lambda %||% x$lambda
  theta <- predict(x, lambda = lambda)

  nlambda <- length(lambda)

  df <- data.frame(
    theta = c(theta),
    lambda = rep(lambda, each = n),
    x = rep(x$x, nlambda)
  )

  plt <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = df,
      ggplot2::aes(.data$x, .data$theta, colour = .data$lambda, group = .data$lambda)
    ) +
    ggplot2::ylab(expression(hat(theta))) +
    ggplot2::theme_bw() +
    ggplot2::scale_colour_viridis_c(trans = "log10", name = expression(lambda))
  if (nlambda == 1) plt <- plt + ggplot2::theme(legend.position = "none")
  plt <- plt +
    ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5))
  return(plt)
}
