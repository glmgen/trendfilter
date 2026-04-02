#ifndef LINEARSYSTEM_H
#define LINEARSYSTEM_H

typedef Eigen::COLAMDOrdering<int> Ord;

// [[Rcpp::depends(RcppEigen)]]

/**
 * Implementation of linear system solve using Sparse QR, tridiagonal, or
 * other updates (e.g., Kalman Filter-based).
 *
 * Encapsulating all approaches to solving the linear system in the theta
 * update allows a single ADMM routine to be written (rather than
 * several separate routines, one for each solver).
 *
 * @param solver = 0 (`trigiag`), 1 (`sparse_qr`), 2 (`kf`)
 */
class LinearSystem {
  public:
    void construct(const Eigen::VectorXd&, const Eigen::ArrayXd&, int, double, 
        const Eigen::SparseMatrix<double>&, const Eigen::MatrixXd&, const Eigen::VectorXd&, int);
    void compute(int);
    std::tuple<Eigen::VectorXd,int> solve(const Eigen::VectorXd&, const Eigen::ArrayXd&, 
        const Eigen::VectorXd&, int, Rcpp::NumericVector, double, const Eigen::MatrixXd&, 
        const Eigen::VectorXd&, int, bool);
    void kf_init(int, int, double, const Eigen::MatrixXd&, const Eigen::VectorXd&);
    void kf_iter(const Eigen::VectorXd&, const Eigen::ArrayXd&, const Eigen::VectorXd&, 
        const Eigen::MatrixXd&, const Eigen::VectorXd&, bool);
  private:
    // tridiag
    Eigen::VectorXd a, b, c, cp;
    Eigen::VectorXd wy;
    // sparse_qr
    Eigen::SparseQR<Eigen::SparseMatrix<double>, Ord> qr;
    Eigen::SparseMatrix<double> A;
    // kf: 
    int d, rankp;
    double vt_b, Ft_b, Finf_b;
    Eigen::VectorXd RQR, a1, vt, Ft, Finf, Kt_b, Kinf_b, r,  r1, rtmp, sol;
    Eigen::MatrixXd T, at, P1, Pt, P1inf, Pinf, Kt, Kinf, L0, L1, Ptemp;
    // cholesky
    Eigen::SimplicialLDLT<Eigen::SparseMatrix<double>> cholesky;
};

#endif
