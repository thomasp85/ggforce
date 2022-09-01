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

bool points_on_line(Eigen::MatrixXd points, Ellipse &enc) {
  double xmin, ymin, xmax, ymax;
  int n = points.rows();
  if (n == 1) {
    enc.x = points.coeff(0, 0);
    enc.y = points.coeff(0, 1);
    enc.a = 0;
    enc.b = 0;
    enc.rad = 0;
    return true;
  }
  if (n == 2) {
    xmin = points.coeff(0, 0);
    xmax = points.coeff(1, 0);
    ymin = points.coeff(0, 1);
    ymax = points.coeff(1, 1);
  } else {
    double x0 = xmin = xmax = points.coeff(0, 0);
    double y0 = ymin = ymax = points.coeff(0, 1);
    double xdiff = points.coeff(1, 0) - x0;
    bool vert = xdiff == 0;
    double slope;
    if (!vert) {
      slope = (points.coeff(1, 1) - y0) / xdiff;
    }
    for (int i = 2; i < n; i++) {
      xdiff = points(i, 0) - x0;
      if (vert && xdiff == 0) {
        ymin = std::min(ymin, points(i, 1));
        ymax = std::max(ymax, points(i, 1));
        continue;
      }
      if (slope == (points.coeff(i, 1) - y0) / xdiff) {
        if (points(i, 0) < xmin) {
          xmin = points(i, 0);
          ymin = points(i, 1);
        } else if (points(i, 0) > xmin) {
          xmax = points(i, 0);
          ymax = points(i, 1);
        }
        continue;
      }
      return false;
    }
  }
  if (xmin == xmax && ymin == ymax) {
    enc.x = xmin;
    enc.y = ymin;
    enc.a = 0;
    enc.b = 0;
    enc.rad = 0;
  } else {
    enc.x = (xmin + xmax) / 2;
    enc.y = (ymin + ymax) / 2;
    double diff_x = xmax - xmin;
    double diff_y = ymax - ymin;
    enc.a = std::sqrt(diff_x * diff_x + diff_y * diff_y) / 2;
    enc.b = enc.a * 0.1;
    enc.rad = std::atan(diff_y / diff_x);
  }
  return true;
}

Ellipse khachiyan(Eigen::MatrixXd points, double tol) {
  Ellipse enc;
  if (points_on_line(points, enc)) {
    return enc;
  }
  points.adjointInPlace();
  int N = points.cols();
  int d = points.rows();
  Eigen::MatrixXd Q;
  Q = points;
  Q.conservativeResize(d + 1, Eigen::NoChange);
  Q.row(d).setOnes();
  Eigen::MatrixXd Qadj = Q.adjoint();

  double error = 1;
  double max, step;
  Eigen::MatrixXd X, A, V;
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
  c = points * u;
  enc.x = c[0];
  enc.y = c[1];

  Eigen::JacobiSVD<Eigen::MatrixXd> svd_A(A, Eigen::ComputeThinV);
  enc.a = 1.0/std::sqrt(float(svd_A.singularValues()[1]));
  enc.b = 1.0/std::sqrt(float(svd_A.singularValues()[0]));
  V = svd_A.matrixV();
  if (V(0, 1) == V(1, 0)) {
    enc.rad = std::asin(float(V(1, 1)));
  } else if (V(0, 1) < V(1, 0)) {
    enc.rad = std::asin(-float(V(0, 0)));
  } else {
    enc.rad = std::asin(float(V(0, 0)));
  }
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
