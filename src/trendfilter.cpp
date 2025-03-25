#include <cmath>
#include <stdexcept>
#include <tuple>
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <Rcpp.h>
#include <RcppEigen.h>
#include <tvdenoising.h>
#include "utils.h"
#include "linearsystem.h"
#include "kf_utils.h"
#include "pm_matrix.h"


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

LinearSystem linear_system;

double tf_gauss_loss(const VectorXd& y,
                     const VectorXd& theta,
                     const ArrayXd& weights) {
  return 0.5 * ((y - theta).array() * weights.sqrt()).matrix().squaredNorm();
}

double tf_penalty(const VectorXd& theta, const NumericVector& xd, double lam, int k) {
  Eigen::VectorXd Dv = Dkv(theta, k + 1, xd, true);
  return lam * Dv.lpNorm<1>();
}


// Gaussian only
double tf_objective(const VectorXd& y, const VectorXd& theta,
                    const NumericVector& xd,
                    const ArrayXd& weights, double lam, int k) {
  return tf_gauss_loss(y, theta, weights) + tf_penalty(theta, xd, lam, k);
}

// Matches initialization from package glmgen
// Not used in these ADMM updates (projection via Legendre polynomials is
// more stable, but left here as a reference implementation).
std::tuple<VectorXd,int> init_theta_nullspace(const VectorXd& y, const
    SparseMatrix<double>& penalty_mat) {
  SparseQR<SparseMatrix<double>, Ord> qr;
  qr.compute(penalty_mat.transpose());
  // Project y onto rowspace of C^{k+1}
  VectorXd beta = qr.solve(y);
  // Subtract residual off rowspace-projection to obtain nullspace projection.
  return std::make_tuple(y - (penalty_mat.transpose()) * beta, int(qr.info()));
}

VectorXd init_u(const VectorXd& residual, const NumericVector& xd, int k,
    const ArrayXd& weights) {
  // (Dk)^T u = y - \theta, from stationarity
  SparseQR<SparseMatrix<double>, Ord> qr;
  SparseMatrix<double> dk_mat = get_dk_mat(k, xd, false);
  qr.compute(dk_mat.transpose());
  return qr.solve((residual.array()*weights).matrix());
}

//////////////////////////////////////////////////////////////// main update begins
void admm_single_lambda(int n, const Eigen::VectorXd& y, const NumericVector& xd,
                        const Eigen::ArrayXd& weights, int k,
                        bool boundary_condition, int left_boundary_m, int right_boundary_m,
                        Eigen::Ref<Eigen::VectorXd> theta,
                        Eigen::Ref<Eigen::VectorXd> alpha, Eigen::Ref<Eigen::VectorXd> u, int& iter,
                        double& obj_val, const Eigen::SparseMatrix<double>& dk_mat_sq, const Eigen::SparseMatrix<double>& dk_mat,
                        const Eigen::MatrixXd& denseD, const Eigen::VectorXd& s_seq, double lam,
                        int max_iter, double rho, double tol = 1e-5, int linear_solver = 2,
                        bool equal_space = false) {


  // Initialize
  VectorXd tmp(n-k);
  VectorXd Dth_tmp(alpha.size());
  VectorXd alpha_old(alpha);
  VectorXd wy = (y.array()*weights).matrix();
  double rr, ss;

  Eigen::SparseMatrix<double> Pm;
  if (boundary_condition) {
    Pm = pm_matrix(xd, left_boundary_m, right_boundary_m);
  }
  // Set up linear system
  linear_system.construct(y, weights, k, rho, dk_mat_sq, denseD, s_seq, linear_solver);
  linear_system.compute(linear_solver);


    // ADMM loop
  int computation_info;
  iter = 0;
  for (iter = 1; iter < max_iter; iter++) {
    if (iter % 1000 == 0) Rcpp::checkUserInterrupt();

    // Update if boundary condition=False)
    if (!boundary_condition) {
      std::tie(theta, computation_info) = linear_system.solve(y, weights,
                 alpha + u, k, xd, rho, denseD, s_seq, linear_solver, equal_space);
    } else { // Theta Update (Gamma update if ns=True)
      // Check if dimensions match before multiplying
      if (u.size() != alpha.size()) {
        Rcpp::Rcerr << "ERROR: u.size = " << u.size() << " does not match alpha.size() = " << alpha.size() << std::endl;
        Rcpp::stop("Matrix size mismatch:  alpha + u");
      }

      if (dk_mat.rows() != alpha.size()) {
        Rcpp::Rcerr << "ERROR: dk_mat.transpose().cols() = " << dk_mat.transpose().cols() << " does not match alpha.size() = " << alpha.size() << std::endl;
        Rcpp::stop("Matrix size mismatch: dk_mat.transpose() * (alpha + u)");
      }

      //Rcpp::Rcout << "Calculating LHS and RHS matrix..." << std::endl; // Print
      Eigen::MatrixXd LHS = (Pm.transpose() * Pm) + rho * Pm.transpose() * (dk_mat_sq) * Pm;
      Eigen::VectorXd RHS = Pm.transpose() * y + rho * Pm.transpose() * dk_mat.transpose() * (alpha + u);

      Eigen::ColPivHouseholderQR<Eigen::MatrixXd> solver(LHS);
      Eigen::VectorXd gamma = solver.solve(RHS);
      theta = Pm * gamma;
      }

      // Compute Dk*theta, D1*alpha
      Dth_tmp = Dkv(theta, k, xd);
      tmp = Dth_tmp - u;

      // Alpha Update
      if (!boundary_condition) {
        alpha = tf_dp(tmp, lam / rho);
      } else {
        alpha = tf_dp(tmp, lam / rho);
      }

      // Dual Variable Update (u)
      if (!boundary_condition) {
        u += alpha - Dth_tmp;
      } else {
        u += alpha - Dth_tmp;
      }

      // Convergence check
      rr = (Dth_tmp - alpha).norm() / alpha.size();
      ss = rho * Dktv(alpha - alpha_old, k, xd).norm() / dk_mat_sq.rows();
      alpha_old = alpha;
      if (rr < tol && ss < tol) break;
    }

    obj_val = tf_objective(y, theta, xd, weights, lam, k);


}
//////////////////////////////////////////////////////////////////////// main update ends
// [[Rcpp::export]]
Rcpp::List admm_lambda_seq(
    NumericVector x,
    Eigen::VectorXd const y,
    Eigen::ArrayXd const weights,
    int k,
    bool boundary_condition,
    int left_boundary_m,
    int right_boundary_m,
    Eigen::VectorXd lambda,
    int nlambda = 50,
    double lambda_max = -1.0,
    double lambda_min = -1.0,
    double lambda_min_ratio = 1e-5,
    int max_iter = 200,
    double rho_scale = 1.0,
    double tol = 1e-5,
    int linear_solver = 2,
    double space_tolerance_ratio = -1.0) {

  int n = x.size();


  if (lambda[0] < tol / 100 && lambda_max <= 0) {
    lambda_max = get_lambda_max(x, y, weights, k);
  }
  get_lambda_seq(lambda, lambda_max, lambda_min, lambda_min_ratio, nlambda);


  Eigen::MatrixXd theta(n, nlambda);
  Rcpp::NumericVector objective_val(nlambda);
  Rcpp::IntegerVector iters(nlambda);
  Rcpp::IntegerVector dof(nlambda);

  // Use DP solution for k=0.
  if (k == 0) {
    for (int i = 0; i < nlambda; i++) {
      theta.col(i) = tf_dp_weight(y, lambda[i], weights);
      objective_val[i] = tf_objective(y, theta.col(i), x, weights, lambda[i], k);
      dof[i] = calc_degrees_of_freedom(theta.col(i), k);
    }
    Rcpp::List out = Rcpp::List::create(
      Rcpp::Named("theta") = theta,
      Rcpp::Named("lambda") = lambda,
      Rcpp::Named("tf_objective") = objective_val,
      Rcpp::Named("iters") = iters
    );
    return out;
  }

  // Initialize difference matrices and other helper objects
  SparseMatrix<double> dk_mat = get_dk_mat(k, x, false);
  SparseMatrix<double> dk_mat_sq = dk_mat.transpose() * dk_mat;
  // Kalman filter objects
  bool equal_space = false;
  MatrixXd denseD;
  VectorXd s_seq;
  // configure `denseD` if using Kalman filter to contain the information in
  //  the first k nonzero columns in `dk_mat`. If evenly spaced, resize it to 1*k.
  if (linear_solver == 2) {
    // check if `x` is equally spaced
    equal_space = is_equal_space(x, space_tolerance_ratio < 0 ? std::sqrt(
      Eigen::NumTraits<double>::epsilon()) : space_tolerance_ratio);
    // initialize with the size of nonzero values in `dk_mat`
    denseD = MatrixXd::Zero(n - k, k + 1);
    // if using Kalman filter, save the rightmost nonzero value per row in
    //  `dk_mat` for unevenly space signals. For equally spaced signals,
    //  simplify it to one value.
    s_seq = equal_space ? VectorXd::Zero(1) : VectorXd::Zero(n);
    configure_denseD(x, denseD, s_seq, dk_mat, k, equal_space);
  }

  Eigen::MatrixXd alpha(n-k, nlambda);

  // Initialize ADMM variables
  // Project onto Legendre polynomials to initialize for largest lambda.
  theta.col(0) = project_polynomials(x, y, weights, k);
  alpha.col(0) = Dkv(theta.col(0), k, x);
  VectorXd u = init_u((theta.col(0) - y)/(lambda[0]*rho_scale), x, k, weights);


  for (int i = 0; i < nlambda; i++) {
    Rcpp::checkUserInterrupt();
    admm_single_lambda(n, y, x, weights, k, boundary_condition, left_boundary_m, right_boundary_m,
                       theta.col(i), alpha.col(i), u,
                       iters[i], objective_val[i],
                                              dk_mat_sq, dk_mat, denseD, s_seq, lambda[i], max_iter, lambda[i]*rho_scale,
                                              tol, linear_solver, equal_space);
    dof[i] = calc_degrees_of_freedom(alpha.col(i), k);
    if (i + 1 < nlambda) {
      theta.col(i + 1) = theta.col(i);
      alpha.col(i + 1) = alpha.col(i);
    }
  }
  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("theta") = theta,
    Rcpp::Named("alpha") = alpha,
    Rcpp::Named("lambda") = lambda,
    Rcpp::Named("tf_objective") = objective_val,
    Rcpp::Named("iters") = iters,
    Rcpp::Named("dof") = dof
  );
  return out;


}

// The below is legacy code, currently unused.

// Rcpp::List admm_single_lambda_with_tracking(NumericVector x,
//     Eigen::VectorXd& y, const Eigen::ArrayXd& weights, int k,
//     double lam, int max_iter, double rho,
//     bool tridiag=false) {
//
//   int n = x.size();
//
//   // Initialize difference matrices and other helper objects
//   SparseMatrix<double> penalty_mat = get_penalty_mat(k+1, x);
//   SparseMatrix<double> dk_mat = get_dk_mat(k, x, false);
//   VectorXd wy = (y.array()*weights).matrix();
//
//   // Initialize theta, alpha, u
//   VectorXd theta = project_polynomials(x, y, weights, k);
//   VectorXd alpha = dk_mat * theta;
//   VectorXd u = init_u((theta-y)/rho, x, k, weights);
//   VectorXd tmp(n-k);
//
//   // Form Gram matrix and set up linear system for theta update
//   SparseMatrix<double> A = rho * (dk_mat.transpose() * dk_mat);
//   A.diagonal().array() += weights;
//   A.makeCompressed();
//
//   if (tridiag & (k != 1)) {
//     throw std::invalid_argument("`tridiag` can only be used with k=1.");
//   }
//   LinearSystem linear_system;
//   linear_system.compute(A, tridiag);
//
//   // Perform ADMM updates
//   int computation_info;
//   MatrixXd theta_mat(n, max_iter);
//   VectorXd objective_vec(max_iter);
//   int iter = 0;
//   theta_mat.col(iter) = theta;
//   objective_vec[iter] = tf_objective(y, theta, x, weights, lam, k);
//   VectorXd best_theta = theta;
//   double best_objective = objective_vec[iter];
//   for (iter = 1; iter < max_iter; iter++) {
//     // theta update
//     std::tie(theta, computation_info) = linear_system.solve(
//         wy + rho * (dk_mat.transpose() * (alpha + u)),
//         tridiag);
//     // if (computation_info > 1) {
//     //   std::cerr << "Eigen Sparse QR solve returned nonzero exit status.\n";
//     // }
//     tmp = dk_mat*theta - u;
//     // alpha update
//     alpha = tf_dp(tmp, lam/rho);
//     // u update
//     u += alpha - dk_mat*theta;
//     objective_vec[iter] = tf_objective(y, theta, x, weights, lam, k);
//     theta_mat.col(iter) = theta;
//     if (objective_vec[iter] < best_objective) {
//       best_objective = objective_vec[iter];
//       best_theta = theta;
//     }
//   }
//   // TODO: Implement stopping criterion
//   Rcpp::List return_list = Rcpp::List::create(
//       Rcpp::Named("theta") = best_theta,
//       Rcpp::Named("objective") = objective_vec,
//       Rcpp::Named("theta_mat") = theta_mat,
//       Rcpp::Named("computation_info") = computation_info);
//   return return_list;
// }

