#include <Rcpp.h>
#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]
using namespace Rcpp;

struct Ellipse {
    double x;
    double y;
    double a;
    double b;
    double rad;
};
Ellipse khachiyan(Eigen::MatrixXd points, double tol) {
    points.adjointInPlace();
    int N = points.cols();
    int d = points.rows();
    Eigen::MatrixXd Q(d+1, N);
    Q << points;
    Q.row(d).setOnes();
    Eigen::MatrixXd Qadj = Q.adjoint();

    double error = 1;
    double max, step;
    Eigen::MatrixXd X, A;
    Eigen::VectorXd M, u_tmp, c;
    Eigen::VectorXd::Index max_i;
    Eigen::VectorXd u(N);
    u.fill(1/double(N));

    while (error > tol) {
        X = Q * u.asDiagonal() * Qadj;
        M = (Qadj * X.inverse() * Q).diagonal();
        max = M.maxCoeff(&max_i);
        step = (max - d - 1)/((d + 1)*(max - 1));
        u_tmp = (1 - step)*u;
        u_tmp[max_i] = u_tmp[max_i] + step;
        error = (u_tmp - u).norm();
        u = u_tmp;
    }
    A = (1.0/d) * (points * u.asDiagonal() * points.adjoint() - (points * u)*(points * u).adjoint() ).inverse();
    Eigen::JacobiSVD<Eigen::MatrixXd> svd_A(A, Eigen::ComputeThinV);
    c = points * u;
    Ellipse enc;
    enc.x = c[0];
    enc.y = c[1];
    enc.a = 1.0/std::sqrt(float(svd_A.singularValues()[1]));
    enc.b = 1.0/std::sqrt(float(svd_A.singularValues()[0]));
    enc.rad = std::asin(float(svd_A.matrixV()(1,1)));
    return enc;
}

// [[Rcpp::export]]
DataFrame enclose_ellip_points(NumericVector x, NumericVector y, IntegerVector id, double tol) {
    if (x.size() != y.size() || x.size() != id.size()) {
        stop("x, y, and id must have same dimensions");
    }
    const Eigen::Map<Eigen::VectorXd> X(as< Eigen::Map<Eigen::VectorXd> >(x));
    const Eigen::Map<Eigen::VectorXd> Y(as< Eigen::Map<Eigen::VectorXd> >(y));
    std::vector< double > x0, y0, a, b, rad;
    std::vector< int > splits;
    splits.push_back(0);
    int currentId = id[0];
    int i, size;
    for (i = 0; i < id.size(); ++i) {
        if (id[i] != currentId) {
            currentId = id[i];
            splits.push_back(i);
        }
    }
    splits.push_back(id.size());
    for (i = 0; i < splits.size() - 1; ++i) {
        size = splits[i+1] - splits[i];
        Eigen::MatrixXd points(size, 2);
        points.col(0) = X.segment(splits[i], size);
        points.col(1) = Y.segment(splits[i], size);
        Ellipse center = khachiyan(points, tol);
        x0.push_back(center.x);
        y0.push_back(center.y);
        a.push_back(center.a);
        b.push_back(center.b);
        rad.push_back(center.rad);
    }
    return DataFrame::create(
        Named("x0") = wrap(x0),
        Named("y0") = wrap(y0),
        Named("a") = wrap(a),
        Named("b") = wrap(b),
        Named("angle") = wrap(rad)
    );
}
