
<!-- README.md is generated from README.Rmd. Please edit that file -->

# trendfilter

<!-- badges: start -->

[![R-CMD-check](https://github.com/glmgen/trendfilter/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/glmgen/trendfilter/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of trendfilter is to **solve** nonparametric regression.

## Installation

You can install the development version of trendfilter from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("glmgen/trendfilter")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(trendfilter)
library(ggplot2)
x <- 1:100 / 101 * 2 * pi
y <- sin(x) + .2 * rnorm(100)
out <- trendfilter(y, x, nlambda = 15)
plot(out) +
  geom_point(data = data.frame(x = x, y = y), aes(x, y), color = "black")
```

<img src="man/figures/README-example-1.png" width="100%" />

## Boundary Condition Option

`boundary_condition = TRUE` enables boundary correction using Newton
polynomials and divided differences. `left_boundary_m` and
`right_boundary_m` determine how the boundary conditions are applied
(order of polynomial). By default, `left_boundary_m` =
`right_boundary_m` = `round(k/2)`, which simplifies to (k+1)/2 when k is
odd (natural spline at the boundary).

``` r
#remotes::install_github("hughjonesd/ggmagnify")
library(ggplot2)
library(tidyr)
library(grid)
library(ggmagnify)

x <- 1:100 / 101 * 2 * pi
y <- sin(x) + .2 * rnorm(50)

# Standard trend filtering (without boundary conditions)
out1 <- trendfilter(y, x, nlambda = 15)
df1 <- data.frame(
  x = 1:100 / 101 * 2 * pi, 
  y = as.vector(out1$theta), 
  lambda = rep(out1$lambda, each = length(x))
)

# Trend filtering with natural spline boundary conditions
out2 <- trendfilter(y, x, nlambda = 15, boundary_condition = TRUE)
df2 <- data.frame(
  x = 1:100 / 101 * 2 * pi, 
  y = as.vector(out2$theta), 
  lambda = rep(out2$lambda, each = length(x))
)
```

``` r
# ---- Plots ----
# Standard Trend Filtering Plot with Magnification on Boundaries
ggp1 <- ggplot(df1, aes(x, y, group = lambda, color = as.factor(lambda))) +
  geom_line() +
  geom_point(data = data.frame(x = x, y = y), aes(x, y), color = "black", inherit.aes = FALSE) +
  ggtitle("Standard Trend Filtering") + coord_cartesian(xlim = c(-0.5, 6.5), ylim = c(-2, 2)) +
  theme_minimal()

# Trend filtering with Natural Spline Plot with Magnification on Boundaries
ggp2 <- ggplot(df2, aes(x, y, group = lambda, color = as.factor(lambda))) +
  geom_line() +
  geom_point(data = data.frame(x = x, y = y), aes(x, y), color = "black", inherit.aes = FALSE) +
  ggtitle("Trend filtering with NS Boundary") + coord_cartesian(xlim = c(-0.5, 6.5), ylim = c(-2, 2)) +
  theme_minimal()

# Define the zoom-in regions
from_left <- c(xmin = 0, xmax = 0.5, ymin = -0.2, ymax = 0.4)
to_left <- c(xmin = -0.5, xmax = 0.5,  ymin = -2, ymax = -1)  

from_right <- c(xmin = 5.8, xmax = 6.5, ymin = -0.6, ymax = 0.2)
to_right <- c(xmin = 5.5, xmax = 6.5, ymin = 1, ymax = 2)

# Apply magnification
ggp1 + 
  geom_magnify(from = from_left, to = to_left) + 
  geom_magnify(from = from_right, to = to_right)
```

<img src="man/figures/README-boundary-constraint-plots-1.png" width="100%" />

``` r

ggp2 + 
  geom_magnify(from = from_left, to = to_left) + 
  geom_magnify(from = from_right, to = to_right)
```

<img src="man/figures/README-boundary-constraint-plots-2.png" width="100%" />
