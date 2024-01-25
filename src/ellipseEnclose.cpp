#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/matrix.hpp>
#include <cpp11/list.hpp>
#include <cpp11/data_frame.hpp>
#include <cpp11/function.hpp>

using namespace cpp11::literals;

struct Ellipse {
  double x;
  double y;
  double a;
  double b;
  double rad;
};

bool points_on_line(const cpp11::doubles_matrix<>& points, Ellipse &enc) {
  double xmin, ymin, xmax, ymax;
  R_xlen_t n = points.nrow();
  if (n == 1) {
    enc.x = points(0, 0);
    enc.y = points(0, 1);
    enc.a = 0;
    enc.b = 0;
    enc.rad = 0;
    return true;
  }
  if (n == 2) {
    xmin = points(0, 0);
    xmax = points(1, 0);
    ymin = points(0, 1);
    ymax = points(1, 1);
  } else {
    double x0 = xmin = xmax = points(0, 0);
    double y0 = ymin = ymax = points(0, 1);
    double xdiff = points(1, 0) - x0;
    bool vert = xdiff == 0;
    double slope;
    if (!vert) {
      slope = (points(1, 1) - y0) / xdiff;
    }
    for (int i = 2; i < n; i++) {
      xdiff = points(i, 0) - x0;
      if (vert && xdiff == 0) {
        ymin = std::min(ymin, points(i, 1));
        ymax = std::max(ymax, points(i, 1));
        continue;
      }
      if (slope == (points(i, 1) - y0) / xdiff) {
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

cpp11::writable::doubles_matrix<> transpose(const cpp11::doubles_matrix<>& x) {
  cpp11::writable::doubles_matrix<> trans(x.ncol(), x.nrow());
  for (R_xlen_t j = 0; j < x.ncol(); ++j) for (R_xlen_t i = 0; i < x.nrow(); ++i) {
    trans(j, i) = x(i, j);
  }
  return trans;
}

cpp11::writable::doubles_matrix<> make_q(const cpp11::doubles_matrix<>& p) {
  cpp11::writable::doubles_matrix<> Q(p.nrow() + 1, p.ncol());
  for (R_xlen_t j = 0; j < p.ncol(); ++j) for (R_xlen_t i = 0; i < p.nrow(); ++i) {
    Q(i, j) = p(i, j);
  }
  for (R_xlen_t j = 0; j < p.ncol(); ++j) Q(p.nrow(), j) = 1.0;

  return Q;
}

// Q * diagonal(u) * Qadj
cpp11::writable::doubles_matrix<> solve_x(const cpp11::doubles_matrix<>& Q, const cpp11::doubles& u, const cpp11::doubles_matrix<>& Qadj) {
  cpp11::writable::doubles_matrix<> res(Q.nrow(), Qadj.ncol());
  for (R_xlen_t i = 0; i < Q.nrow(); ++i) for (R_xlen_t j = 0; j < Qadj.ncol(); ++j) {
    double cell = 0.0;
    for (R_xlen_t k = 0; k < u.size(); ++k) {
      cell += Q(i, k) * u[k] * Qadj(k, j);
    }
    res(i, j) = cell;
  }
  return res;
}

// diagonal(Qadj * inverse(X) * Q)
cpp11::writable::doubles solve_m(const cpp11::doubles_matrix<>& Qadj, const cpp11::doubles_matrix<>& X, const cpp11::doubles_matrix<>& Q) {
  static auto solve = cpp11::package("base")["solve"];
  cpp11::doubles_matrix<> Xinv = cpp11::as_cpp< cpp11::doubles_matrix<> >(solve(X));

  cpp11::writable::doubles res;

  for (R_xlen_t i = 0; i < Qadj.nrow(); ++i) {
    double sum = 0.0;
    for (R_xlen_t k = 0; k < Xinv.ncol(); ++k) {
      double cell = 0.0;
      for (R_xlen_t j = 0; j < Qadj.ncol(); ++j) {
        cell += Qadj(i, j) * Xinv(j, k);
      }
      sum += cell * Q(k, i);
    }
    res.push_back(sum);
  }

  return res;
}

cpp11::writable::doubles_matrix<> prod_with_diag(const cpp11::doubles_matrix<>& A, const cpp11::doubles& b) {
  if (A.ncol() != b.size()) {
    cpp11::stop("A must have the same number of columns as there are elements in b");
  }
  cpp11::writable::doubles_matrix<> res(A.nrow(), A.ncol());
  for (R_xlen_t i = 0; i < A.nrow(); ++i) for (R_xlen_t j = 0; j < A.ncol(); ++j) {
    res(i, j) = A(i, j) * b[j];
  }
  return res;
}

cpp11::writable::doubles_matrix<> prod(const cpp11::doubles_matrix<>& A, const cpp11::doubles_matrix<>& B) {
  if (A.ncol() != B.nrow()) {
    cpp11::stop("A must have the same number of columns as there are rows in B");
  }
  cpp11::writable::doubles_matrix<> res(A.nrow(), B.ncol());
  for (R_xlen_t i = 0; i < A.nrow(); ++i) for (R_xlen_t j = 0; j < B.ncol(); ++j) {
    double cell = 0.0;
    for (R_xlen_t k = 0; k < A.ncol(); ++k) {
      cell += A(i, k) * B(k, j);
    }
    res(i, j) = cell;
  }
  return res;
}
cpp11::writable::doubles_matrix<> prod_with_vec(const cpp11::doubles_matrix<>& A, const cpp11::doubles& b) {
  if (A.ncol() != b.size()) {
    cpp11::stop("A must have the same number of columns as there are elements in b");
  }
  cpp11::writable::doubles_matrix<> res(A.nrow(), 1);
  for (R_xlen_t i = 0; i < A.nrow(); ++i) {
    double cell = 0.0;
    for (R_xlen_t k = 0; k < A.ncol(); ++k) {
      cell += A(i, k) * b[k];
    }
    res(i, 0) = cell;
  }
  return res;
}
//           ------------------------------------- fourth ------------------------------------------------
//                                                         ----------- third -----------------
//            --------------- first --------------------    - second -
// (1.0/d) * (points * u.asDiagonal() * points.adjoint() - (points * u)*(points * u).adjoint() ).inverse()
cpp11::writable::doubles_matrix<> solve_a(const cpp11::doubles_matrix<>& points, const cpp11::doubles& u, const cpp11::doubles_matrix<>& second) {
  static auto solve = cpp11::package("base")["solve"];
  cpp11::writable::doubles_matrix<> first = prod(prod_with_diag(points, u), transpose(points));
  cpp11::doubles_matrix<> third = prod(second, transpose(second));

  for (R_xlen_t i = 0; i < first.nrow(); ++i) for (R_xlen_t j = 0; j < first.ncol(); ++j) {
    first(i, j) -= third(i, j);
  }

  cpp11::writable::doubles_matrix<> fourth = cpp11::as_cpp< cpp11::writable::doubles_matrix<> >(solve(first));

  for (R_xlen_t i = 0; i < fourth.nrow(); ++i) for (R_xlen_t j = 0; j < fourth.ncol(); ++j) {
    fourth(i, j) *= 1.0 / points.nrow();
  }

  return fourth;
}

Ellipse solve_enc(const cpp11::doubles_matrix<>& A) {
  static auto svd = cpp11::package("base")["svd"];
  Ellipse enc;
  cpp11::list svd_a = cpp11::as_cpp<cpp11::list>(svd(A));

  cpp11::doubles d(svd_a["d"]);
  cpp11::doubles_matrix<> v(svd_a["v"]);

  enc.a = 1.0/std::sqrt(d[1]);
  enc.b = 1.0/std::sqrt(d[0]);
  if (v(0, 1) == v(1, 0)) {
    enc.rad = std::asin(v(1, 1));
  } else if (v(0, 1) < v(1, 0)) {
    enc.rad = std::asin(-v(0, 0));
  } else {
    enc.rad = std::asin(v(0, 0));
  }
  return enc;
}

Ellipse khachiyan(const cpp11::doubles_matrix<>& points, double tol) {
  Ellipse enc;
  if (points_on_line(points, enc)) {
    return enc;
  }

  cpp11::doubles_matrix<> pointsAdj = transpose(points);

  R_xlen_t N = pointsAdj.ncol();
  R_xlen_t d = pointsAdj.nrow();
  cpp11::doubles_matrix<> Q = make_q(pointsAdj);
  cpp11::doubles_matrix<> Qadj = transpose(Q);

  cpp11::writable::doubles u(N);
  std::fill(u.begin(), u.end(), 1/double(N));
  cpp11::writable::doubles u_tmp(N);

  double error = 1;
  while (error > tol) {
    cpp11::doubles_matrix<> X = solve_x(Q, u, Qadj);
    cpp11::doubles M = solve_m(Qadj, X, Q);
    R_xlen_t max_i = 0;
    double max = M[0];
    for (R_xlen_t i = 1; i < M.size(); ++i) {
      if (M[i] > max) {
        max_i = i;
        max = M[i];
      }
    }
    double step = (max - d - 1)/((d + 1)*(max - 1));
    cpp11::writable::doubles u_tmp;
    u_tmp.reserve(N);
    for (R_xlen_t i = 0; i < u.size(); ++i) {
      u_tmp.push_back(u[i] * (1 - step));
    }
    u_tmp[max_i] += step;

    error = 0.0;
    for (R_xlen_t i = 0; i < u.size(); ++i) {
      double du = u_tmp[i] - u[i];
      error += du * du;
      u[i] = double(u_tmp[i]);
    }
    error = std::sqrt(error);
  }
  cpp11::doubles_matrix<> c = prod_with_vec(pointsAdj, u);
  cpp11::doubles_matrix<> A = solve_a(pointsAdj, u, c);
  enc = solve_enc(A);
  enc.x = c(0, 0);
  enc.y = c(1, 0);

  return enc;
}

[[cpp11::register]]
cpp11::writable::data_frame enclose_ellip_points(cpp11::doubles x, cpp11::doubles y, cpp11::integers id, double tol) {
  if (x.size() != y.size() || x.size() != id.size()) {
    cpp11::stop("x, y, and id must have same dimensions");
  }
  std::vector< int > splits;
  splits.push_back(0);
  int currentId = id[0];
  for (R_xlen_t i = 0; i < id.size(); ++i) {
    if (id[i] != currentId) {
      currentId = id[i];
      splits.push_back(i);
    }
  }
  splits.push_back(id.size());

  cpp11::writable::doubles x0;
  cpp11::writable::doubles y0;
  cpp11::writable::doubles a;
  cpp11::writable::doubles b;
  cpp11::writable::doubles rad;
  for (R_xlen_t i = 0; i < splits.size() - 1; ++i) {
    int size = splits[i+1] - splits[i];
    cpp11::writable::doubles_matrix<> points(size, 2);
    for (size_t j = 0; j < points.nrow(); ++j) {
      points(j, 0) = x[splits[i] + j];
      points(j, 1) = y[splits[i] + j];
    }
    Ellipse center = khachiyan(points, tol);
    x0.push_back(center.x);
    y0.push_back(center.y);
    a.push_back(center.a);
    b.push_back(center.b);
    rad.push_back(center.rad);
  }

  return cpp11::writable::data_frame({
    "x0"_nm = x0,
    "y0"_nm = y0,
    "a"_nm = a,
    "b"_nm = b,
    "angle"_nm = rad
  });
}
