// Must precede any R header so that LAPACK character-length arguments
// are passed correctly (FCONE).
#define USE_FC_LEN_T
#include <cmath>
#include <tuple>
#include <vector>
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <Rcpp.h>
#include <RcppEigen.h>
#include <R_ext/Lapack.h>
#include "utils.h"
#include "linearsystem.h"
#include "kf_utils.h"
#ifndef FCONE
#define FCONE
#endif

// [[Rcpp::depends(RcppEigen)]]

using Rcpp::NumericVector;
using Eigen::SparseMatrix;
using Eigen::ArrayXd;
using Eigen::VectorXd;
using Eigen::MatrixXd;

/*
 * Butterworth-type (squared ell_2 penalized) filter.
 *
 * For each lambda, solves
 *
 *   min_theta  1/2 sum_i w_i (y_i - theta_i)^2 + lambda ||D theta||_2^2,
 *
 * where D = D^{(k+1)} is the same (k+1)-th order discrete derivative
 * matrix used in the trend filtering penalty. The solution satisfies
 *
 *   (W + 2 lambda D^T D) theta = W y,
 *
 * which is a single linear solve per lambda (no ADMM iterations). We reuse
 * `LinearSystem` with "k" set to k + 1 and a zero ADMM adjustment.
 *
 * lambda = Inf is allowed: the solution is then the weighted least squares
 * projection of y onto polynomials of degree k (the null space of D), which
 * we compute directly rather than through a badly conditioned solve.
 *
 * If `early_stop`, the path is truncated as in glmnet: after `mnlam` fits,
 * stop once the fraction of (weighted) deviance explained exceeds `devmax`
 * or increases by less than `fdev` times its current value.
 */

// [[Rcpp::export]]
Rcpp::List bw_lambda_seq(
    NumericVector x,
    Eigen::VectorXd const y,
    Eigen::ArrayXd const weights,
    int k,
    Eigen::VectorXd lambda,
    int linear_solver = 2,
    double space_tolerance_ratio = -1.0,
    bool early_stop = false,
    double devmax = 0.999,
    double fdev = 1e-5,
    int mnlam = 5) {
  int n = x.size();
  int nlambda = lambda.size();
  int kk = k + 1; // order of the penalty

  SparseMatrix<double> dmat = get_dk_mat(kk, x, true);
  SparseMatrix<double> dmat_sq = dmat.transpose() * dmat;

  // tridiagonal system when the penalty is first order
  if (kk == 1) linear_solver = 0;

  bool equal_space = false;
  MatrixXd denseD;
  VectorXd s_seq;
  if (linear_solver == 2) {
    equal_space = is_equal_space(x, space_tolerance_ratio < 0 ? std::sqrt(
      Eigen::NumTraits<double>::epsilon()) : space_tolerance_ratio);
    denseD = MatrixXd::Zero(n - kk, kk + 1);
    s_seq = equal_space ? VectorXd::Zero(1) : VectorXd::Zero(n);
    configure_denseD(x, denseD, s_seq, dmat, kk, equal_space);
  }

  MatrixXd theta(n, nlambda);
  Eigen::VectorXd objective(nlambda);
  Eigen::VectorXd dev_ratio(nlambda);
  double ybar = (y.array() * weights).sum() / weights.sum();
  double nulldev = ((y.array() - ybar).square() * weights).sum();
  double rsq_old = 0.0;
  int nfit = nlambda;
  VectorXd zeros = VectorXd::Zero(n - kk);
  VectorXd sol(n);
  LinearSystem linear_system;
  int info;

  for (int i = 0; i < nlambda; i++) {
    Rcpp::checkUserInterrupt();
    double rho = 2.0 * lambda[i];
    bool is_inf = std::isinf(lambda[i]);
    if (is_inf) {
      theta.col(i) = project_polynomials(x, y, weights, k);
    } else if (rho > Eigen::NumTraits<double>::epsilon()) {
      linear_system.construct(y, weights, kk, rho, dmat_sq, denseD, s_seq,
                              linear_solver);
      linear_system.compute(linear_solver);
      std::tie(sol, info) = linear_system.solve(y, weights, zeros,
          kk, x, rho, denseD, s_seq, linear_solver, equal_space);
      theta.col(i) = sol;
    } else {
      theta.col(i) = y;
    }
    VectorXd Dth = dmat * theta.col(i);
    double rss = ((y - theta.col(i)).array().square() * weights).sum();
    // D theta = 0 when lambda = Inf, and Inf * 0 is NaN
    objective[i] = 0.5 * rss + (is_inf ? 0.0 : lambda[i] * Dth.squaredNorm());
    double rsq = nulldev > 0 ? 1.0 - rss / nulldev : 1.0;
    dev_ratio[i] = rsq;
    if (early_stop && i + 1 >= mnlam) {
      if (rsq - rsq_old < fdev * rsq || rsq > devmax) {
        nfit = i + 1;
        break;
      }
    }
    rsq_old = rsq;
  }
  theta.conservativeResize(Eigen::NoChange, nfit);
  lambda.conservativeResize(nfit);
  objective.conservativeResize(nfit);
  dev_ratio.conservativeResize(nfit);

  return Rcpp::List::create(
    Rcpp::Named("theta") = theta,
    Rcpp::Named("lambda") = lambda,
    Rcpp::Named("objective") = objective,
    Rcpp::Named("dev_ratio") = dev_ratio
  );
}

/*
 * Generalized eigenvalues tau of the banded pencil
 *
 *   (c D^T D) v = tau (W + c D^T D) v,   0 <= tau <= 1,
 *
 * with D = D^{(k+1)}. If D^T D v = mu W v, then tau = c mu / (1 + c mu), so
 * the eigenvalues of the smoother matrix S = (W + 2 lambda D^T D)^{-1} W are
 *
 *   (1 - tau) / (1 - tau + 2 lambda tau / c).
 *
 * The right-hand matrix is positive definite even with some zero weights
 * (as long as at least k + 1 weights are positive), and zero-weight
 * directions correspond to tau = 1 (they contribute nothing to the
 * degrees of freedom). Uses LAPACK's banded solver, O(n^2 k).
 */

// [[Rcpp::export]]
Rcpp::List bw_spectrum(NumericVector x, Eigen::ArrayXd const weights, int k) {
  int n = x.size();
  int kk = k + 1;
  SparseMatrix<double> dmat = get_dk_mat(kk, x, true);
  SparseMatrix<double> dmat_sq = dmat.transpose() * dmat;
  // Rough balance between the two pieces: diag(W) ~ 1/n, ||D^T D|| ~ 4^(k+1)
  double c = 1.0 / (n * std::pow(4.0, kk));

  int ka = kk, kb = kk, ld = kk + 1;
  std::vector<double> ab(ld * n, 0.0), bb(ld * n, 0.0);
  // Upper band storage: element (i, j), i <= j, goes to ab[(ka + i - j) + j * ld]
  for (int j = 0; j < dmat_sq.outerSize(); ++j) {
    for (SparseMatrix<double>::InnerIterator it(dmat_sq, j); it; ++it) {
      int i = it.row();
      if (i <= j && j - i <= ka) {
        ab[(ka + i - j) + j * ld] = c * it.value();
        bb[(kb + i - j) + j * ld] = c * it.value();
      }
    }
    bb[kb + j * ld] += weights[j];
  }

  std::vector<double> tau(n), work(3 * n);
  double z = 0.0;
  int ldz = 1, info = 0;
  F77_CALL(dsbgv)("N", "U", &n, &ka, &kb, ab.data(), &ld, bb.data(), &ld,
           tau.data(), &z, &ldz, work.data(), &info FCONE FCONE);
  if (info != 0) {
    Rcpp::stop("LAPACK dsbgv failed with info = %i.", info);
  }

  // tau is in ascending order. The first k + 1 correspond to the
  // polynomial null space of D; set them to exactly 0 and clamp the rest.
  for (int i = 0; i < n; i++) {
    if (i < kk) tau[i] = 0.0;
    else tau[i] = std::fmin(std::fmax(tau[i], 0.0), 1.0);
  }
  return Rcpp::List::create(
    Rcpp::Named("tau") = tau,
    Rcpp::Named("c") = c
  );
}
