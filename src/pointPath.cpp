#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;

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
void dist_to_path(double x, double y, ListOf<NumericMatrix> path, std::vector<double> &res, bool closed_poly) {
    int i, j, k;
    double dist, shortest_dist = -1;
    std::pair<double, double> point, a, b, close, closest;
    point.first = x;
    point.second = y;
    for (i = 0; i < path.size(); ++i) {
        for (j = 0; j < path[i].nrow(); ++j) {
            if (j == path[i].nrow() && !closed_poly) break;
            a.first = path[i](j, 0);
            a.second = path[i](j, 1);
            k = j == path[i].nrow() - 1 ? 0 : j + 1;
            b.first = path[i](k, 0);
            b.second = path[i](k, 1);
            close = projection(a, b, point, true);
            dist = std::sqrt(distSquared(point, close));
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

//[[Rcpp::export]]
List points_to_path(NumericMatrix pos, ListOf<NumericMatrix> path, bool close) {
    std::vector<double> res_container;
    NumericMatrix proj(pos.nrow(), 2);
    NumericVector dist(pos.nrow());
    for (int i = 0; i < pos.nrow(); ++i) {
        dist_to_path(pos(i, 0), pos(i, 1), path, res_container, close);
        proj(i, 0) = res_container[0];
        proj(i, 1) = res_container[1];
        dist[i] = res_container[2];
    }
    return List::create(
        _["projection"] = proj,
        _["distance"] = dist
    );
}
