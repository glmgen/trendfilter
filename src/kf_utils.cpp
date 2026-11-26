#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <tuple>
#include <cmath>
#include <limits>
#include <Rcpp.h>
#include <RcppEigen.h>
#include "utils.h"
#include "kf_utils.h"

// [[Rcpp::depends(RcppEigen)]]

using Eigen::SparseMatrix;
using Eigen::SparseQR;
using Eigen::ArrayXd;
using Eigen::VectorXd;
using Eigen::MatrixXd;
using Eigen::Map;

// in-place computation of Ptemp given P and A
Eigen::MatrixXd computePtemp(Eigen::MatrixXd A, Eigen::MatrixXd P) {
  int k = P.rows();
  Eigen::MatrixXd temp = A.row(0) * P;
  Eigen::MatrixXd var = A.row(0) * temp.transpose();

  // in-place block replacement starting from the last component:
  P.block(1, 1, k - 1, k - 1).reverse() = P.block(0, 0, k - 1, k - 1).reverse();
  P(0, 0) = var(0, 0);
  temp.conservativeResize(1, k - 1);  // drop the last component
  P.block(0, 1, 1, k - 1) = temp;
  P.block(1, 0, k - 1, 1) = temp.transpose();
  return P;
}

// [[Rcpp::export]]
Eigen::MatrixXd smat_to_mat(const Eigen::SparseMatrix<double>& sparseMat, int k, bool equal_spaced) {
  int rows = sparseMat.rows(); // n-k
  Eigen::MatrixXd denseMat(rows, k + 1);
  std::vector<double> rowNonzeros;
  // Iterate over nonzero coefficients in each row of the sparse matrix
  for (int i = 0; i < sparseMat.outerSize(); ++i) {
    std::vector<double> rowNonzeros;
    for (SparseMatrix<double>::InnerIterator it(sparseMat, i); it; ++it) 
      rowNonzeros.push_back(it.value());
    int m = rowNonzeros.size();
    for (int j = 0; j < m; j++) {
      if (i < rows) 
        denseMat(i - j, j) = rowNonzeros[m - 1 - j];
      else  
        denseMat(rows - 1 - j, j + i - rows + 1) = rowNonzeros[m - 1 - j];
    }
    if (equal_spaced && i == k) {
      denseMat.conservativeResize(1, k + 1);
      return denseMat;
    } 
  }
  return denseMat;
}

void configure_denseD(Rcpp::NumericVector x, Eigen::MatrixXd& denseD, Eigen::VectorXd& s_seq, Eigen::SparseMatrix<double>& dk_mat, int k, bool equal_space) {
    int n = x.size();
    // First, construct denseD with only nonzero entries from sparse matrix `dk_mat`:
    denseD = smat_to_mat(dk_mat, k, equal_space);
    // ideally, construct `denseD` directly without computing and transforming the sparse matrix: 
    // denseD = b_mat(k, x, VectorXi::LinSpaced(n - k, 0, n - k - 1));

    // Second, resize and re-construct `denseD` so that each row contains the values 
    //    for the first row of the transition matrix per iterate: 
    // `s_seq` contains the multipliers of the latest states for each transition
    if (equal_space) { // the nonzero band is the same per row
      s_seq = denseD.block(0, k, 1, 1); // assign the rightmost in the first row in `denseD` to `s_seq`
      denseD.conservativeResize(1, k); // keep only the first row and drop the rightmost column
    } else {
      s_seq = denseD.block(0, k, n - k, 1); // assign the rightmost column in `denseD` to `s_seq`
      denseD.conservativeResize(n - k, k); // drop the rightmost column
    }
    // 2. reverse the order of values in each row and divide them by the negative `s_seq` per row
    MatrixXd firstRow(1, k);
    int m = denseD.rows();
    for (int i = 0; i < m; i++) {
      firstRow = -denseD.row(i) / s_seq(i);
      std::reverse(firstRow.data(), firstRow.data() + k);
      denseD.row(i) = firstRow;
    }
}

// [[Rcpp::export]]
Rcpp::List configure_denseD_test(Rcpp::NumericVector x, int k) {
    SparseMatrix<double> dk_mat = get_dk_mat(k, x, false);
    int n = x.size();
    bool equal_space = is_equal_space(x, 0.1);
    MatrixXd denseD = MatrixXd::Zero(n, k + 1);
    VectorXd s_seq = equal_space ? VectorXd::Zero(1) : VectorXd::Zero(n);
  
    configure_denseD(x, denseD, s_seq, dk_mat, k, equal_space);
    
    return Rcpp::List::create(Rcpp::Named("Dk") = dk_mat, Rcpp::Named("s_seq") = s_seq, Rcpp::Named("dense_D") = denseD);
}

void f1step(double y, double c, double Z, double H, const Eigen::MatrixXd& A, 
  double RQR, Eigen::VectorXd& a, Eigen::MatrixXd& P, double& vt, double& Ft,
  Eigen::VectorXd& Kt) {
  VectorXd a_temp = A * a;
  a_temp(0) += c;
  MatrixXd Ptemp = computePtemp(A, P);  // A * P * A.transpose();
  Ptemp(0, 0) += RQR;

  vt = y - Z * a_temp(0);
  Ft = pow(Z, 2) * Ptemp(0, 0) + H;
  Kt = Ptemp.col(0) * Z;
  a = a_temp + Kt * vt / Ft;
  P = Ptemp - Kt * Z * Ptemp.row(0) / Ft;
  // symmetrize
  Ptemp = P;
  Ptemp += P.transpose();
  P = Ptemp / 2;

  // Some entries of P can be _really_ small in magnitude, set them to 0
  // but maintain symmetric / posdef
  for (int i = 0; i < P.rows(); i++) {
    for (int j = 0; j <= i; j++) {
      if (abs(P(i, j)) < 1e-30) {
        if (i == j) {
          P.row(i).setZero();
          P.col(i).setZero();
        } else {
          P(i, j) = 0;
          P(j, i) = 0;
        }
      }
    }
  }
}

void df1step(double y, double Z, double H, const Eigen::MatrixXd& A, double RQR,
  Eigen::VectorXd& a, Eigen::MatrixXd& P, Eigen::MatrixXd& Pinf, int& rankp,
  double& vt, double& Ft, double& Finf, Eigen::VectorXd& Kt, Eigen::VectorXd& Kinf) {
  double tol = Eigen::NumTraits<double>::epsilon();
  tol = std::sqrt(tol);
  int k = a.size();
  MatrixXd Ptemp(k, k);

  a = A * a;
  P = computePtemp(A, P);  // A * P * A.transpose();
  P(0, 0) += RQR;
  Pinf = computePtemp(A, Pinf);  // A * Pinf * A.transpose();

  vt = y - Z * a(0);
  Kt = P.col(0) * Z;
  Ft = Z * Kt(0) + H;
  Kinf = Pinf.col(0) * Z;
  Finf = Z * Kinf(0);

  if (!std::isfinite(H)) {
    // Zero weight (H = 1 / w = Inf): treat as a missing observation, so
    // prediction only, no update. Ft = Inf zeroes the gain in the smoother.
    Finf = 0;
    Ft = std::numeric_limits<double>::infinity();
  } else if (Finf > tol) {  // should always happen
    a += vt * Kinf / Finf;
    P += Ft * Kinf * Kinf.transpose() / pow(Finf, 2);
    P -= Kt * Kinf.transpose() / Finf + Kinf * Kt.transpose() / Finf;
    Pinf -= Kinf * Kinf.transpose() / Finf;
    rankp--;
  } else {  // should never happen
    Finf = 0;
    if (Ft > tol) {
      a += vt * Kt / Ft;
      P -= Kt * Kt.transpose() / Ft;
    }
  }
  if (Ft < tol)
    Ft = 0;

  // symmetrize
  Ptemp = P;
  Ptemp += P.transpose();
  P = Ptemp / 2;
  Ptemp = Pinf;
  Ptemp += Pinf.transpose();
  Pinf = Ptemp / 2;

  // Fix possible negative definiteness, should never happen
  for (int i = 0; i < k; i++) {
    if (P(i, i) < 0) {
      P.row(i).setZero();
      P.col(i).setZero();
    }
    if (Pinf(i, i) < 0) {
      Pinf.row(i).setZero();
      Pinf.col(i).setZero();
    }
  }
}
