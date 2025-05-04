#ifndef PM_MATRIX_H  // Prevents multiple inclusions
#define PM_MATRIX_H

#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;

// Declare the function(s) you implemented in `pm_matrix.cpp`
Eigen::SparseMatrix<double> pm_matrix(const NumericVector& xd, int m1, int m2);
#endif

