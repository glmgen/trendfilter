#include <cmath>
#include <stdexcept>
#include <tuple>
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <Rcpp.h>
#include <RcppEigen.h>
#include "utils.h"
#include "kf_utils.h"
#include "linearsystem.h"

// [[Rcpp::depends(RcppEigen)]]

typedef Eigen::COLAMDOrdering<int> Ord;

using Rcpp::NumericVector;
using Rcpp::IntegerVector;
using Eigen::SparseMatrix;
using Eigen::SparseQR;
using Eigen::ArrayXd;
using Eigen::VectorXd;
using Eigen::MatrixXd;
using Eigen::Map;

void LinearSystem::construct(const Eigen::VectorXd& y, const Eigen::ArrayXd& weights, 
    int k, double rho, const Eigen::SparseMatrix<double>& dk_mat_sq, 
    const Eigen::MatrixXd& Dseq, const Eigen::VectorXd& s_seq, int solver) {
  switch(solver) {
    case 0: [[fallthrough]];
    case 1: [[fallthrough]];
    case 3: {
      wy = (y.array() * weights).matrix();
      // Form Gram matrix and set up linear system for theta update
      A = rho * dk_mat_sq;
      A.diagonal().array() += weights;
      A.makeCompressed();
      break;
    }
    case 2: {
      int n = y.size();
      LinearSystem::kf_init(n, k, rho, Dseq, s_seq);
      break;
    }
  } 
}

void LinearSystem::compute(int solver) {
  switch(solver) {
    case 0: {
      std::tie(a, b, c) = extract_tridiag(A);
      cp = tridiag_forward(a, b, c);
      break;
    }
    case 1: {
      // Setting the pivot threshold to be negative forces SparseQR to be
      // maximally conservative in dropping columns, which is important when
      // the Gram matrix is ill-conditioned (which often is the case for
      // unequally spaced inputs.
      qr.setPivotThreshold(-1.0);
      qr.compute(A);
      break;
    }
    case 2:
      break;
    case 3: {
      cholesky.compute(A);
      break;
    }
  }
}

std::tuple<VectorXd,int> LinearSystem::solve(const Eigen::VectorXd& y, 
    const Eigen::ArrayXd& weights, const Eigen::VectorXd& adj_mean, int k, 
    Rcpp::NumericVector x, double rho, const Eigen::MatrixXd& Dseq, 
    const Eigen::VectorXd& s_seq, int solver, bool equal_space) {
  int info = 0;
  switch(solver) {
    case 0: {
      VectorXd v = wy + rho * Dktv(adj_mean, k, x); 
      sol = tridiag_backsolve(a, b, cp, v);
      break;
    }
    case 1: {
      VectorXd v = wy + rho * Dktv(adj_mean, k, x);
      sol = qr.solve(v);
      info = int(qr.info());
      break;
    }
    case 2: {
      LinearSystem::kf_iter(y, weights, adj_mean, Dseq, s_seq, equal_space);
      break;
    }
    case 3: {
      VectorXd v = wy + rho * Dktv(adj_mean, k, x);
      sol = cholesky.solve(v);
      info = int(cholesky.info());
      break;
    }
  }
  return std::make_tuple(sol, info);
}

// [[Rcpp::export]]
Eigen::VectorXd linear_single_solve_test(int linear_solver, const Eigen::VectorXd y, 
    const Eigen::ArrayXd weights, const Rcpp::NumericVector x, double rho, 
    const Eigen::VectorXd adj_mean) {
  int n = y.size();
  int k = n - adj_mean.size();
  SparseMatrix<double> dk_mat = get_dk_mat(k, x, false);
  SparseMatrix<double> dk_mat_sq = dk_mat.transpose() * dk_mat;
  // check if `x` is equally spaced
  bool equal_space = false;
  // configure `denseD` if using Kalman filter
  MatrixXd denseD;
  VectorXd s_seq;
  if (linear_solver == 2) {
    equal_space = is_equal_space(x, std::sqrt(Eigen::NumTraits<double>::epsilon()));
    denseD = MatrixXd::Zero(n - k, k + 1);
    s_seq = equal_space ? VectorXd::Zero(1) : VectorXd::Zero(n);
    configure_denseD(x, denseD, s_seq, dk_mat, k, equal_space);
  }

  VectorXd sol = VectorXd::Zero(n);
  LinearSystem linear_system;
  int info = 0;
  linear_system.construct(y, weights, k, rho, dk_mat_sq, denseD, s_seq, linear_solver);
  linear_system.compute(linear_solver);
  std::tie(sol, info) = linear_system.solve(y, weights, adj_mean, k, x, rho, denseD, 
    s_seq, linear_solver, equal_space);
  return sol;
}

void LinearSystem::kf_init(int n, int k, double rho, const Eigen::MatrixXd& Dseq, 
    const Eigen::VectorXd& s_seq) {
  // construct transition matrix A:
  T = MatrixXd::Zero(k, k);
  T.block(1, 0, k - 1, k - 1).diagonal() = VectorXd::Ones(k - 1);
  // other initialization: 
  a1 = VectorXd::Zero(k);
  at = MatrixXd::Zero(k, n + 1);
  P1 = MatrixXd::Zero(k, k);
  Pt = MatrixXd::Zero(k * k, n + 1);
  P1inf = MatrixXd::Identity(k, k);
  Pinf = MatrixXd::Zero(k * k, n + 1);
  Pinf.col(0) = Map<Eigen::VectorXd>(P1inf.data(), k * k);
  // forward: 
  d = 0;
  rankp = k;
  vt_b = 0.0;
  Ft_b = 0.0;
  Finf_b = 0.0;
  vt = VectorXd::Zero(n);
  Ft = VectorXd::Zero(n);
  Finf = VectorXd::Zero(n);
  Kt = MatrixXd::Zero(k, n);
  Kinf = MatrixXd::Zero(k, n);
  Kt_b = VectorXd::Zero(k);
  Kinf_b = VectorXd::Zero(k);
  RQR = s_seq.array().square().inverse();
  RQR *= 1 / rho; 
  T.row(0) = Dseq.row(0);
  // backward: 
  r = VectorXd::Zero(k);
  r1 = VectorXd::Zero(k);
  rtmp = VectorXd::Zero(k);
  L0 = MatrixXd::Zero(k, k);
  L1 = MatrixXd::Zero(k, k);
  Ptemp = MatrixXd::Zero(k, k);
}

void LinearSystem::kf_iter(const Eigen::VectorXd& y, const Eigen::ArrayXd& w, 
    const Eigen::VectorXd& c, const Eigen::MatrixXd& Dseq, 
    const Eigen::VectorXd& s_seq, bool equal_space) {
  int n = y.size();
  int k = a1.size();
  sol = VectorXd::Zero(n);
  // initialize or reset iterator
  d = 0;
  rankp = k;
  double rqr = RQR(0);
  double s = s_seq(0);
  // initialize or reset states and covariance matrices
  a1 = VectorXd::Zero(k);
  P1 = MatrixXd::Zero(k, k);
  P1inf = MatrixXd::Identity(k, k);
  Pinf.col(0) = Map<Eigen::VectorXd>(P1inf.data(), k * k);
  while (rankp > 0 && d < n) {
    df1step(y(d), 1, 1 / w(d), T, rqr, a1, P1, P1inf, rankp, vt_b, Ft_b,
            Finf_b, Kt_b, Kinf_b);
    at.col(d + 1) = a1;
    vt(d) = vt_b;
    Ft(d) = Ft_b;
    Finf(d) = Finf_b;
    Kt.col(d) = Kt_b;
    Kinf.col(d) = Kinf_b;
    Pt.col(d + 1) = Map<VectorXd>(P1.data(), k * k);
    Pinf.col(d + 1) = Map<VectorXd>(P1inf.data(), k * k);
    d++;
  }
  for (int i = d; i < n; i++) {
    if (!equal_space) 
        rqr = RQR(i - d);
    f1step(y(i), c(i - d) / s, 1, 1 / w(i), T, rqr, a1, P1,
           vt_b, Ft_b, Kt_b); 
    vt(i) = vt_b;
    Ft(i) = Ft_b;
    Kt.col(i) = Kt_b;
    at.col(i + 1) = a1;
    Pt.col(i + 1) = Map<VectorXd>(P1.data(), k * k);
    // update transition matrix T for next iterate:
    if (!equal_space && i < n - 1) {
      T.row(0) = Dseq.row(i - d + 1);
      s = s_seq(i - d + 1);
    }
  }
  for (int i = n - 1; i >= d; i--) {
    P1 = Map<MatrixXd>(Pt.col(i + 1).data(), k, k);
    sol(i) = at.col(i + 1)(0) - P1.row(0) * r;
    L0.col(0) = Kt.col(i) / Ft(i);
    r1 = r - L0.transpose() * r;
    r1(0) -=  vt(i) / Ft(i); 
    r = T.transpose() * r1;
    // update transition matrix T for next iterate:
    if (!equal_space && i > d)
      T.row(0) = Dseq.row(i - d - 1);
  }
  for (int i = d - 1; i >= 0; i--) {
    P1 = Map<MatrixXd>(Pt.col(i + 1).data(), k, k);
    P1inf = Map<MatrixXd>(Pinf.col(i + 1).data(), k, k);
    a1 = at.col(i + 1);
    sol(i) = a1(0) - P1.row(0) * r - P1inf.row(0) * r1;
    if (i > 0) {
      if (Finf(i) > 0) {
        // simple version w/o mat multiplication:
        L0.col(0) = Kinf.col(i) / Finf(i);
        L1.col(0) = L0.col(0) * Ft(i) / Finf(i);
        L1.col(0) -= Kt.col(i) / Finf(i);
        rtmp = r1 - L0.transpose() * r1 + L1.transpose() * r;
        rtmp(0) -= vt(i) / Finf(i); 
        r1 = T.transpose() * rtmp;
        rtmp = r - L0.transpose() * r;
        r = T.transpose() * rtmp;
      } else {
        L1.col(0) = Kt.col(i) / Ft(i);
        rtmp = r - L1.transpose() * r;
        rtmp(0) -= vt(i) / Ft(i);
        r = T.transpose() * rtmp;
        rtmp = r1 - L1.transpose() * r1;
        r1 = T.transpose() * rtmp;
      }
    }
  }
}
