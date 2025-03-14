#include <Rcpp.h>
#include "pm_matrix.h"
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix compute_P_matrix(NumericVector x_target, NumericVector x_support) {
  int m = x_target.size();
  int n = x_support.size();
  NumericMatrix P(m, n);

  for (int i = 0; i < m; i++) {
    P(i, 0) = 1.0;
    for (int j = 1; j < n; j++) {
      P(i, j) = P(i, j - 1) * (x_target[i] - x_support[j - 1]);
    }
  }
  return P;
}

// [[Rcpp::export]]
NumericMatrix compute_C_withoutTheta(NumericVector x_support) {
  int n = x_support.size();
  NumericMatrix C(n, n);

  for (int t = 0; t < n; t++) {
    for (int w = 0; w <= t; w++) {
      double prod_denom = 1.0;
      for (int u = 0; u <= t; u++) {
        if (u != w) {
          prod_denom *= (x_support[w] - x_support[u]);
        }
      }
      C(t, w) = 1.0 / prod_denom;
    }
  }
  return C;
}

// [[Rcpp::export]]
NumericMatrix compute_A_matrix(NumericVector x_target, NumericVector x_support) {
  NumericMatrix P = compute_P_matrix(x_target, x_support);
  NumericMatrix C_withoutTheta = compute_C_withoutTheta(x_support);

  int m = P.nrow();
  int n = C_withoutTheta.ncol();

  NumericMatrix A(m, n);

  for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
      A(i, j) = 0;
      for (int k = 0; k < n; k++) {
        A(i, j) += P(i, k) * C_withoutTheta(k, j);
      }
    }
  }
  return A;
}

// [[Rcpp::export]]
NumericMatrix pm_matrix(NumericVector x, int m1, int m2) {
  int n = x.size();
  if ((m1+m2) > n) stop("m1 + m2 must be <= length of x");

  // Define target and support points
  NumericVector x_target_left = x[Range(0, m1 - 1)];
  NumericVector x_support_left = x[Range(m1, 2*m1 - 1)];
  NumericVector x_target_right = x[Range(n-m2, n-1)];
  NumericVector x_support_right = x[Range(n - 2*m2, n-m2-1)];

  // Compute A matrices for left and right
  NumericMatrix A_left = compute_A_matrix(x_target_left, x_support_left);
  NumericMatrix A_right = compute_A_matrix(x_target_right, x_support_right);
  NumericMatrix A_middle(n - m1 - m2, n - m1 - m2);

  // Identity matrix for middle part
  for (int i = 0; i < A_middle.nrow(); i++) {
    A_middle(i, i) = 1.0;
  }

  // Initialize full A matrix
  NumericMatrix A(n, n-m1-m2);

  // Fill left, middle, and right portions
  for (int i = 0; i < A_left.nrow(); i++) {
    for (int j = 0; j < A_left.ncol(); j++) {
      A(i, j) = A_left(i, j);
    }
  }

  for (int i = 0; i < A_middle.nrow(); i++) {
    for (int j = 0; j < A_middle.ncol(); j++) {
      A(m1 + i, j) = A_middle(i, j);
    }
  }

  for (int i = 0; i < A_right.nrow(); i++) {
    for (int j = 0; j < A_right.ncol(); j++) {
      A(n - m2 + i, n - m1 - 2*m2 + j) = A_right(i, j);
    }
  }

  return A;
}
