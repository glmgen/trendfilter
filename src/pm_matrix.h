#ifndef PM_MATRIX_H  // Prevents multiple inclusions
#define PM_MATRIX_H

#include <Rcpp.h>  // Include Rcpp dependency

using namespace Rcpp; // Optional (can be removed if using explicit scoping)

// Declare the function(s) you implemented in `ns_matrix.cpp`
NumericMatrix pm_matrix(NumericVector x, int m1, int m2);

#endif
