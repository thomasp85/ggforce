#include "deBoor.h"

#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/strings.hpp>
#include <cpp11/matrix.hpp>
#include <cpp11/list.hpp>

using namespace cpp11::literals;

cpp11::writable::doubles createKnots(int nControl, int degree) {
  int nKnots = nControl + degree + 1;

  cpp11::writable::doubles knots;
  knots.reserve(nKnots);
  for (int i = 0; i < nKnots; i++) {
    if (i < degree + 1) {
      knots.push_back(0);
    } else if (i < nKnots - degree) {
      knots.push_back(knots[i-1] + 1);
    } else {
      knots.push_back(knots[i-1]);
    }
  }
  return knots;
}
std::vector<double> createOpenKnots(int nControl, int degree) {
  int nKnots = nControl + degree + 1;

  std::vector<double> knots (nKnots, 0);
  for (int i = 0; i < nKnots; i++) {
    if (i < 1) knots[i] = 0;
    else knots[i] = knots[i-1] + 1;
  }
  return knots;
}
std::vector<Point> createControls(const cpp11::doubles& x, const cpp11::doubles& y) {
  int nControls = x.size();
  std::vector<Point> controls(nControls, Point());
  for (int i = 0; i < nControls; i++) {
    controls[i] = Point(x[i], y[i]);
  }
  return controls;
}
[[cpp11::register]]
cpp11::writable::doubles_matrix<> splinePath(cpp11::doubles x, cpp11::doubles y,
                                             int degree, cpp11::doubles knots,
                                             int detail, cpp11::strings type) {
  std::vector<Point> controls = createControls(x, y);
  std::vector<double> knots_vec(knots.begin(), knots.end());
  if (type[0] == "closed") {
    controls.push_back(controls[0]);
    controls.push_back(controls[1]);
    controls.push_back(controls[2]);
  }
  cpp11::writable::doubles_matrix<> res(detail, 2);
  double zJump = (knots_vec[knots_vec.size()-1-degree] - knots_vec[degree]);
  if (type[0] == "clamped") {
    zJump /= double(detail-1);
  } else {
    zJump /= double(detail);
  }
  for (int i = 0; i < detail; i++) {
    Point point;
    if (i == detail-1 && type[0] == "clamped") {
      point = controls[controls.size()-1];
    } else {
      double z = knots_vec[degree] + i * zJump;
      int zInt = whichInterval(z, knots_vec);
      point = deBoor(degree, degree, zInt, z, knots_vec, controls);
    }
    res(i, 0) = point.x;
    res(i, 1) = point.y;
  }
  return res;
}
[[cpp11::register]]
cpp11::writable::list getSplines(cpp11::doubles x, cpp11::doubles y,
                                 cpp11::integers id, int detail,
                                 cpp11::strings type) {
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
  int nPaths = nControls.size();
  cpp11::writable::doubles_matrix<> paths(nPaths * detail, 2);
  cpp11::writable::integers pathsID(nPaths * detail);
  int controlsStart = 0;
  R_xlen_t pathStart = 0;
  for (int i = 0; i < nPaths; i++) {
    cpp11::writable::doubles knots;
    int degree = nControls[i] <= 3 ? nControls[i] - 1 : 3;
    if (type[0] == "clamped") {
      knots = createKnots(nControls[i], degree);
    } else if (type[0] == "open") {
      knots = createOpenKnots(nControls[i], degree);
    } else if (type[0] == "closed") {
      if (nControls[i] < 3) {
        cpp11::stop("At least 3 control points must be provided for closed b-splines");
      }
      degree = 3;
      knots = createOpenKnots(nControls[i] + 3, degree);
    } else {
      cpp11::stop("type must be either \"open\", \"closed\", or \"clamped\"");
    }
    cpp11::writable::doubles x_tmp(x.begin() + controlsStart, x.begin() + controlsStart + nControls[i]);
    cpp11::writable::doubles y_tmp(y.begin() + controlsStart, y.begin() + controlsStart + nControls[i]);
    cpp11::doubles_matrix<> path = splinePath(x_tmp, y_tmp, degree, knots, detail, type);
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
