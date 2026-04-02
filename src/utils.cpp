#include <stdexcept>
#include <Eigen/Sparse>
#include <Rcpp.h>
#include <RcppEigen.h>
#include <dspline.h>
#include <tvdenoising.h>
#include "utils.h"

// [[Rcpp::depends(RcppEigen)]]

typedef Eigen::COLAMDOrdering<int> Ord;

using Eigen::SparseMatrix;
using Eigen::SparseQR;
using Eigen::ArrayXd;
using Eigen::MatrixXd;
using Eigen::VectorXd;

/* General utilities */
Eigen::SparseMatrix<double> row_scale(
    Eigen::SparseMatrix<double> A,
    Eigen::ArrayXd v) {
  return v.matrix().asDiagonal() * A;
}
Eigen::MatrixXd row_scale(Eigen::MatrixXd A, Eigen::ArrayXd v) {
  return v.matrix().asDiagonal() * A;
}
Eigen::SparseMatrix<double> col_scale(
    Eigen::SparseMatrix<double> A,
    Eigen::ArrayXd v) {
  return A * v.matrix().asDiagonal();
}
Eigen::MatrixXd col_scale(Eigen::MatrixXd A, Eigen::ArrayXd v) {
  return A * v.matrix().asDiagonal();
}

/* Matrix construction */
Eigen::SparseMatrix<double> identity(int n) {
  SparseMatrix<double> Id(n, n);
  Id.setIdentity();
  return Id;
}

Eigen::SparseMatrix<double> diagonal(Eigen::ArrayXd diag) {
  int n = diag.size();
  SparseMatrix<double> D(n, n);
  D.diagonal() = diag;
  return D;
}

// [[Rcpp::export]]
Eigen::SparseMatrix<double> get_dk_mat(
    int k, NumericVector xd,
    bool tf_weighting) {
  int n = xd.size();
  return dspline::rcpp_b_mat(k, xd, tf_weighting, Rcpp::seq(0, n - k - 1), true);
}

// [[Rcpp::export]]
Eigen::SparseMatrix<double> get_penalty_mat(int k, NumericVector xd) {
  int n = xd.size();
  return dspline::rcpp_b_mat(k, xd, true, Rcpp::seq(0, n - k - 1), true);
}

// [[Rcpp::export]]
Eigen::MatrixXd polynomial_basis(
    const Eigen::VectorXd& x,
    int k,
    double a = 0.0,
    double b = 1.0,
    int max_dim = 10
  ) {
  int n = x.size();
  ArrayXd xa;
  max_dim = max_dim < n ? max_dim : n - 1;
  k = k > max_dim ? max_dim : k;
  MatrixXd P(n, k + 1);
  P.col(0) = VectorXd::Ones(n);
  if (k > 0) {
    xa = 2 * (x.array() - a) / (b - a) - 1;
    P.col(1) = xa.matrix();
  }
  for (int j = 2; j < k + 1; j++) {
    // https://en.wikipedia.org/wiki/Legendre_polynomials#Recurrence_relations
    int nn = j - 1;
    P.col(j) = (2*nn + 1)*(xa * P.col(nn).array()).matrix() - nn * P.col(nn-1);
    P.col(j) /= j;
  }
  return P;
}

// [[Rcpp::export]]
Eigen::VectorXd project_polynomials(
    const NumericVector& x, // should this be Eigen?
    const Eigen::VectorXd& y,
    const Eigen::ArrayXd& weights,
    int k) {
  Eigen::ColPivHouseholderQR<MatrixXd> qr;
  ArrayXd sqrt_weights = weights.sqrt();
  VectorXd x_vec(Rcpp::as<Eigen::VectorXd>(x));
  MatrixXd basis_mat = polynomial_basis(
    x_vec, k, x_vec.minCoeff(), x_vec.maxCoeff()
  );
  // If this isn't accurate enough, can also use SVD.
  qr.compute(row_scale(basis_mat, sqrt_weights));
  VectorXd beta = qr.solve((y.array()*sqrt_weights).matrix());
  VectorXd projection = basis_mat*beta;
  // if (qr.info() > 0) {
  //  std::cerr << "Eigen QR solve returned nonzero exit status.\n";
  // }
  return projection;
}

/* Tridiagonal matrix solve */
Eigen::VectorXd tridiag_forward(
    const Eigen::VectorXd& a,
    const Eigen::VectorXd& b,
    const Eigen::VectorXd& c) {
  int n = a.size();
  Eigen::VectorXd cp(n - 1);

  // Forward sweep part 1
  cp[0] = c[0] / b[0];
  for (int i = 1; i < n - 1; i++) {
    cp[i] = c[i] / (b[i] - a[i]*cp[i - 1]);
  }
  return cp;
}

// Technically, construction of dp is also part of the forward sweep,
// but it makes sense to include it in "backsolve" since d can change
// over iterations.
Eigen::VectorXd tridiag_backsolve(
    const Eigen::VectorXd& a,
    const::VectorXd& b,
    const Eigen::VectorXd& cp,
    const Eigen::VectorXd& d) {
  int n = d.size();
  Eigen::VectorXd dp(n);
  Eigen::VectorXd x(n);

  // Forward sweep part 2
  dp[0] = d[0] / b[0];
  for (int i = 1; i < n; i++) {
    dp[i] = (d[i] - a[i]*dp[i - 1]) / (b[i] - a[i]*cp[i - 1]);
  }

  // Backsolve
  x[n - 1] = dp[n - 1];
  for (int i = n - 2; i >= 0; i--) {
    x[i] = dp[i] - cp[i]*x[i + 1];
  }
  return x;
}

std::tuple<Eigen::VectorXd,Eigen::VectorXd,Eigen::VectorXd> extract_tridiag(
    Eigen::SparseMatrix<double> A) {
  int n = A.cols();
  VectorXd a(n);
  VectorXd b(n);
  VectorXd c(n);
  // Extract diagonal into b
  b = A.diagonal();
  // Extract (-1)-diagonal into a
  a[0] = 0;
  for (int i = 1; i < n; i++) {
    a[i] = A.coeff(i, i - 1);
  }
  // Extract (+1)-diagonal into c
  for (int i = 0; i < n-1; i++) {
    c[i] = A.coeff(i, i + 1);
  }
  c[n - 1] = 0;
  return std::make_tuple(a, b, c);
}

/* Miscellaneous */
// [[Rcpp::export]]
double get_lambda_max(
    const NumericVector& x,
    const Eigen::VectorXd& y,
    const Eigen::ArrayXd& weights,
    int k) {
  ArrayXd sqrt_weights = weights.sqrt();
  SparseMatrix<double> ck1_mat = get_dk_mat(k + 1, x, true);
  SparseQR<SparseMatrix<double>, Ord> qr;
  qr.compute(col_scale(ck1_mat, sqrt_weights.inverse()).transpose());
  VectorXd u_infty = qr.solve((y.array() * sqrt_weights).matrix());
  return u_infty.lpNorm<Eigen::Infinity>();
}

// [[Rcpp::export]]
int calc_degrees_of_freedom(Eigen::VectorXd const &v, int k, double tol) {
  int dof = k + 1;
  for (int i = 1; i < v.size(); i++) {
    if (abs(v(i) - v(i - 1)) > tol) {
      dof++;
    }
  }
  return dof;
}

// [[Rcpp::export]]
Eigen::VectorXd get_lambda_seq_r(
    Eigen::VectorXd lambda,
    double lambda_max,
    double lambda_min,
    double lambda_min_ratio,
    int n_lambda) {

  get_lambda_seq(lambda, lambda_max, lambda_min, lambda_min_ratio, n_lambda);
  return lambda;
}

void get_lambda_seq(
    Eigen::VectorXd& lambda,
    double lambda_max,
    double lambda_min = -1.0,
    double lambda_min_ratio = 1e-5,
    int n_lambda = 50) {

  if (lambda_max > 0 && n_lambda > 1) {
    double lmpad = lambda_min_ratio * lambda_max;
    lambda_min = (lambda_min < 0) ? lmpad : lambda_min;
    double ns = static_cast<double>(n_lambda) - 1;
    double p = 0.0;
    lambda[0] = lambda_max;
    if (lambda_min > 1e-20) {
      p = pow(lambda_min / lambda_max, 1 / ns);
      for (int i = 1; i < n_lambda; i++) lambda[i] = lambda[i - 1] * p;
    } else {
      ns -= 1;
      p = pow(lmpad / lambda_max, 1 / ns);
      for (int i = 1; i < n_lambda - 1; i++) lambda[i] = lambda[i - 1] * p;
      lambda[n_lambda - 1] = lambda_min;
    }
  }
}


// ---- Workarounds to access NumericVector utilities in dspline
// ---- Do we alter the function signatures in dspline?

// [[Rcpp::export]]
Eigen::VectorXd Dkv(Eigen::VectorXd v, int k, const NumericVector& xd,
                    bool tf_weighting) {
  Rcpp::NumericVector nv(Rcpp::wrap(v));
  Rcpp::NumericVector out = dspline::rcpp_d_mat_mult(nv, k, xd, tf_weighting, false);
  return Rcpp::as<Eigen::Map<VectorXd> >(out);
}


Eigen::VectorXd Dktv(Eigen::VectorXd v, int k, const NumericVector& xd) {
  Rcpp::NumericVector nv(Rcpp::wrap(v));
  Rcpp::NumericVector out = dspline::rcpp_d_mat_mult(nv, k, xd, false, true);
  return Rcpp::as<Eigen::Map<VectorXd> >(out);
}

// ---- Workarounds to access NumericVector utilities in tvdenoising
// ---- We may be able to refactor here (replacing Eigen::VectorXd with NumericVector)

Eigen::VectorXd tf_dp(Eigen::VectorXd v, double lambda) {
   Rcpp::NumericVector nv(Rcpp::wrap(v));
   Rcpp::NumericVector out = tvdenoising::rcpp_tvd(nv, lambda);
   return Rcpp::as<Eigen::Map<VectorXd> >(out);
 }

Eigen::VectorXd tf_dp_weight(Eigen::VectorXd v, double lambda,
                             Eigen::ArrayXd w) {
  Rcpp::NumericVector nv(Rcpp::wrap(v));
  Rcpp::NumericVector nw(Rcpp::wrap(w));
  Rcpp::NumericVector out = tvdenoising::rcpp_wtvd(nv, lambda, nw);
  return Rcpp::as<Eigen::Map<VectorXd> >(out);
}

// [[Rcpp::export]]
bool is_equal_space(Rcpp::NumericVector x, double space_tolerance_ratio) {
  bool equal_space = TRUE;
  int n = x.size();
  double averaged_diff = (x[n-1] - x[0]) / (n - 1);
  double space_tolerance = space_tolerance_ratio * averaged_diff;
  Rcpp::NumericVector diff = tail(x, n - 1) - head(x, n - 1);
  for (int i = 0; i < n - 1; ++i)
    // if any signal distance is greater or smaller than the averaged difference with a tolerance, return False
    if (diff[i] < averaged_diff - space_tolerance || diff[i] > averaged_diff + space_tolerance) {
      equal_space = false;
      break;
    }
  return equal_space;
}
