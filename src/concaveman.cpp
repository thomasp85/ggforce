#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/matrix.hpp>

#include "concaveman.h"

#include <vector>

[[cpp11::register]]
cpp11::writable::doubles_matrix<> concaveman_c(cpp11::doubles_matrix<> p, cpp11::integers h,
                                               double concavity, double threshold) {
  typedef std::array<double, 2> point_type;
  std::vector<point_type> points(p.nrow());
  for (auto i = 0; i < p.nrow(); ++i) {
    points[i] = {p(i, 0), p(i, 1)};
  }
  std::vector<int> hull(h.size());
  for (auto i = 0; i < h.size(); ++i) {
    hull[i] = h[i];
  }

  auto chull = concaveman<double, 16>(points, hull, concavity, threshold);

  cpp11::writable::doubles_matrix<> res(chull.size(), 2);

  for (size_t i = 0; i < chull.size(); ++i) {
    res(i, 0) = chull[i][0];
    res(i, 1) = chull[i][1];
  }

  return res;
}
