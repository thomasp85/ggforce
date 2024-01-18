#include <Rcpp.h>
#include "concaveman.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix concaveman_c(NumericMatrix p, IntegerVector h, double concavity, double threshold) {
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

  NumericMatrix res(chull.size(), 2);

  for (auto i = 0; i < chull.size(); ++i) {
    res(i, 0) = chull[i][0];
    res(i, 1) = chull[i][1];
  }

  return res;
}
