#include <cpp11/doubles.hpp>
#include <cpp11/matrix.hpp>
#include <cpp11/list.hpp>
#include <cpp11/list_of.hpp>

using namespace cpp11::literals;

#include <math.h>
#include <vector>

double distSquared(std::pair<double, double> p, std::pair<double, double> p1) {
  double x = p1.first - p.first;
  double y = p1.second - p.second;
  return x * x + y * y;
}
std::pair<double, double> projection(std::pair<double, double> a, std::pair<double, double> b, std::pair<double, double> p, bool clamp) {
  if (a.first == b.first && a.second == b.second) return a;
  double length2 = distSquared(a, b);
  std::pair<double, double> norm(b.first - a.first, b.second - a.second);
  std::pair<double, double> pa(p.first - a.first, p.second - a.second);
  double t = (norm.first * pa.first + norm.second * pa.second) / length2;
  if (clamp) {
    t = std::max(0.0, std::min(1.0, t));
  }
  norm.first = t * norm.first + a.first;
  norm.second = t * norm.second + a.second;
  return norm;
}
void dist_to_path(double x, double y, const cpp11::list_of< cpp11::doubles_matrix<> >& path, std::vector<double> &res, bool closed_poly) {
  double shortest_dist = -1;
  std::pair<double, double> closest;
  std::pair<double, double> point(x, y);
  for (R_xlen_t i = 0; i < path.size(); ++i) {
    for (R_xlen_t j = 0; j < path[i].nrow(); ++j) {
      if (j == path[i].nrow() && !closed_poly) break;
      std::pair<double, double> a(path[i](j, 0), path[i](j, 1));
      R_xlen_t k = j == path[i].nrow() - 1 ? 0 : j + 1;
      std::pair<double, double> b(path[i](k, 0), path[i](k, 1));
      std::pair<double, double> close = projection(a, b, point, true);
      double dist = std::sqrt(distSquared(point, close));
      if (shortest_dist < 0 || dist < shortest_dist) {
        shortest_dist = dist;
        closest = close;
      }
    }
  }
  res.clear();
  res.push_back(closest.first);
  res.push_back(closest.second);
  res.push_back(shortest_dist);
}

[[cpp11::register]]
cpp11::writable::list points_to_path(cpp11::doubles_matrix<> pos, cpp11::list_of< cpp11::doubles_matrix<> > path, bool close) {
  std::vector<double> res_container;
  cpp11::writable::doubles_matrix<> proj(pos.nrow(), 2);
  cpp11::writable::doubles dist(pos.nrow());
  for (R_xlen_t i = 0; i < pos.nrow(); ++i) {
    dist_to_path(pos(i, 0), pos(i, 1), path, res_container, close);
    proj(i, 0) = res_container[0];
    proj(i, 1) = res_container[1];
    dist[i] = res_container[2];
  }
  return cpp11::writable::list({
    "projection"_nm = proj,
    "distance"_nm = dist
  });
}
