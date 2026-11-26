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
 */

// [[Rcpp::export]]
Rcpp::List bw_lambda_seq(
    NumericVector x,
    Eigen::VectorXd const y,
    Eigen::ArrayXd const weights,
    int k,
    Eigen::VectorXd lambda,
    int linear_solver = 2,
    double space_tolerance_ratio = -1.0) {
  int n = x.size();
  int nlambda = lambda.size();
  int kk = k + 1; // order of the penalty

  SparseMatrix<double> dmat = get_dk_mat(kk, x, true);
  SparseMatrix<double> dmat_sq = dmat.transpose() * dmat;

  // tridiagonal system when the penalty is first order
  if (kk == 1) linear_solver = 0;
  // The Kalman filter uses observation variance 1 / w, so it cannot handle
  // zero weights; fall back to sparse Cholesky.
  if (linear_solver == 2 && (weights <= 0).any()) linear_solver = 3;

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
  NumericVector objective(nlambda);
  VectorXd zeros = VectorXd::Zero(n - kk);
  VectorXd sol(n);
  LinearSystem linear_system;
  int info;

  for (int i = 0; i < nlambda; i++) {
    Rcpp::checkUserInterrupt();
    double rho = 2.0 * lambda[i];
    if (rho > Eigen::NumTraits<double>::epsilon()) {
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
    objective[i] = 0.5 * ((y - theta.col(i)).array().square() * weights).sum() +
      lambda[i] * Dth.squaredNorm();
  }

  return Rcpp::List::create(
    Rcpp::Named("theta") = theta,
    Rcpp::Named("lambda") = lambda,
    Rcpp::Named("objective") = objective
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
