#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/matrix.hpp>
#include <cpp11/list.hpp>

using namespace cpp11::literals;

#include <vector>

double Bezier2(double t, const cpp11::doubles& w) {
  double t2 = t * t;
  double mt = 1-t;
  double mt2 = mt * mt;
  return w[0]*mt2 + w[1]*2*mt*t + w[2]*t2;
}
double Bezier3(double t, const cpp11::doubles& w) {
  double t2 = t * t;
  double t3 = t2 * t;
  double mt = 1-t;
  double mt2 = mt * mt;
  double mt3 = mt2 * mt;
  return w[0]*mt3 + 3*w[1]*mt2*t + 3*w[2]*mt*t2 + w[3]*t3;
}
[[cpp11::register]]
cpp11::writable::doubles_matrix<> bezierPath(const cpp11::doubles& x, const cpp11::doubles& y, int detail) {
  cpp11::writable::doubles_matrix<> res(detail, 2);
  detail = detail - 1;
  double step = 1.0/detail;
  double t;
  if (x.size() == 3) {
    for (int i = 0; i < detail; i++) {
      t = i * step;
      res(i, 0) = Bezier2(t, x);
      res(i, 1) = Bezier2(t, y);
    }
  } else if (x.size() == 4) {
    for (int i = 0; i < detail; i++) {
      t = i * step;
      res(i, 0) = Bezier3(t, x);
      res(i, 1) = Bezier3(t, y);
    }
  } else {
    cpp11::stop("Only support for quadratic and cubic beziers");
  }
  res(detail, 0) = x[x.size() - 1];
  res(detail, 1) = y[y.size() - 1];
  return res;
}
[[cpp11::register]]
cpp11::writable::list getBeziers(const cpp11::doubles& x, const cpp11::doubles& y,
                                 const cpp11::integers& id, int detail) {
  std::vector<int> nControls;
  std::vector<int> pathID;
  nControls.push_back(1);
  pathID.push_back(id[0]);
  for (int i = 1; i < id.size(); i++) {
    if (id[i] == pathID.back()) {
      nControls.back()++;
    } else {
      nControls.push_back(1);
      pathID.push_back(id[i]);
    }
  }
  size_t nPaths = nControls.size();
  cpp11::writable::doubles_matrix<> paths(nPaths * detail, 2);
  cpp11::writable::integers pathsID(nPaths * detail);
  int controlsStart = 0;
  R_xlen_t pathStart = 0;
  for (size_t i = 0; i < nPaths; i++) {
    cpp11::writable::doubles x_tmp(x.begin() + controlsStart, x.begin() + controlsStart + nControls[i]);
    cpp11::writable::doubles y_tmp(y.begin() + controlsStart, y.begin() + controlsStart + nControls[i]);
    cpp11::doubles_matrix<> path = bezierPath(x_tmp, y_tmp, detail);
    for (R_xlen_t j = 0; j < path.nrow(); ++j) {
      pathsID[pathStart + j] = pathID[i];
      paths(pathStart + j, 0) = path(j, 0);
      paths(pathStart + j, 1) = path(j, 1);
    }
    controlsStart += nControls[i];
    pathStart += path.nrow();
  }
  return cpp11::writable::list({
    "paths"_nm = paths,
    "pathID"_nm = pathsID
  });
}
