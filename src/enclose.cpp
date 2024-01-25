#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/data_frame.hpp>

using namespace cpp11::literals;

struct Circle {
  double x;
  double y;
  double r;
};
struct Point {
  double x;
  double y;
};
bool equalPoints(const Point &p1, const Point &p2) {
  return std::abs(p2.x - p1.x) < 1e-9 && std::abs(p2.y - p1.y) < 1e-9;
}
bool perpendicularPoints(const Point &p1, const Point &p2) {
  return std::abs(p2.x - p1.x) < 1e-9 || std::abs(p2.y - p1.y) < 1e-9;
}
bool horizontalPoints(const Point &p1, const Point &p2) {
  return std::abs(p2.y - p1.y) < 1e-9;
}
bool verticalPoints(const Point &p1, const Point &p2) {
  return std::abs(p2.x - p1.x) < 1e-9;
}
Circle circleByPoints(const Point &p1, const Point &p2, const Point &p3) {
  Circle results;
  double X1, Y1, X2, Y2, A1, A2;
  X1 = p2.x - p1.x;
  Y1 = p2.y - p1.y;
  X2 = p3.x - p2.x;
  Y2 = p3.y - p2.y;
  A1 = Y1/X1;
  A2 = Y2/X2;
  if (std::abs(A2 - A1) < 1e-9) {
    cpp11::stop("Error in circleByPoints: The 3 points are colinear");
  }
  results.x = ( A1*A2*(p1.y - p3.y) + A2*(p1.x + p2.x) - A1*(p2.x+p3.x) )/( 2.0 * (A2 - A1) );
  results.y = -1.0*( results.x - (p1.x + p2.x)/2.0 )/A1 + (p1.y + p2.y)/2.0;
  return results;
}
Circle encloseOne(const Point &p1) {
  Circle results;
  results.x = p1.x;
  results.y = p1.y;
  results.r = 0.0;
  return results;
}
Circle encloseTwo(const Point &p1, const Point &p2) {
  if (equalPoints(p1, p2)) return encloseOne(p1);
  Circle results;
  double dx = p2.x - p1.x;
  double dy = p2.y - p1.y;
  results.x = p1.x + dx/2.0;
  results.y = p1.y + dy/2.0;
  results.r = std::sqrt(dx*dx + dy*dy)/2.0;
  return results;
}
Circle encloseThree(const Point &p1, const Point &p2, const Point &p3) {
  if (equalPoints(p1, p2)) return encloseTwo(p1, p3);
  if (equalPoints(p1, p3)) return encloseTwo(p1, p2);
  if (equalPoints(p2, p3)) return encloseTwo(p1, p2);
  bool perp12 = perpendicularPoints(p1, p2);
  bool perp23 = perpendicularPoints(p2, p3);
  bool perp13 = perpendicularPoints(p1, p3);
  Circle results;
  if (perp12 && perp23 && perp13) {
    cpp11::stop("Error in encloseThree: The 3 points are colinear");
  } else if (perp12 + perp23 + perp13 == 2) {
    if (perp12) {
      if (horizontalPoints(p1, p2)) {
        results.y = p1.y + (p2.y - p1.y)/2;
      } else {
        results.x = p1.x + (p2.x - p1.x)/2;
      }
    }
    if (perp23) {
      if (horizontalPoints(p2, p3)) {
        results.y = p2.y + (p3.y - p2.y)/2;
      } else {
        results.x = p2.x + (p3.x - p2.x)/2;
      }
    }
    if (perp13) {
      if (horizontalPoints(p1, p3)) {
        results.y = p1.y + (p3.y -p1.y)/2;
      } else {
        results.x = p1.x + (p3.x -p1.x)/2;
      }
    }
  } else {
    if (!perp12 && !perp23) {
      results = circleByPoints(p1, p2, p3);
    } else if (!perp12 && !perp13) {
      results = circleByPoints(p2, p1, p3);
    } else {
      results = circleByPoints(p1, p3, p2);
    }
  }
  double dx, dy;
  dx = p1.x - results.x;
  dy = p1.y - results.y;
  results.r = std::sqrt(float(dx*dx + dy*dy));
  return results;
}
Circle encloseDefault(std::vector<Point> points) {
  switch(points.size()) {
  case 1: return encloseOne(points[0]);
  case 2: return encloseTwo(points[0], points[1]);
  case 3: return encloseThree(points[0], points[1], points[2]);
  default: cpp11::stop("Error in encloseDefault - expecting less than 4 points");
  }
}
bool inCircle(Circle c, Point p) {
  double dx = p.x - c.x;
  double dy = p.y - c.y;
  return (dx*dx + dy*dy) - c.r*c.r <= 1e-3;
}
bool allInCircle(Circle c, std::vector<Point> points) {
  std::vector<Point>::iterator it;
  for (it = points.begin(); it != points.end(); ++it) {
    if (!inCircle(c, *it)) return false;
  }
  return true;
}
std::vector<Point> extendPerimeter(std::vector<Point> perimeter, Point p) {
  std::vector<Point>::iterator it, jt;
  for (it = perimeter.begin(); it != perimeter.end(); ++it) {
    if (equalPoints(*it, p)) return perimeter;
  }
  if (perimeter.size() < 2) {
    perimeter.push_back(p);
    return perimeter;
  }
  if (inCircle(encloseDefault(perimeter), p)) {
    return perimeter;
  }
  std::vector<Point> new_per;
  for (it = perimeter.begin(); it != perimeter.end(); ++it) {
    if (allInCircle(encloseTwo(*it, p), perimeter)) {
      new_per.push_back(p);
      new_per.push_back(*it);
      return new_per;
    }
  }
  for (it = perimeter.begin(); it != perimeter.end(); ++it) {
    for (jt = it + 1; jt != perimeter.end(); ++jt) {
      if (!inCircle(encloseTwo(*it, *jt), p) &&
          !inCircle(encloseTwo(*it, p), *jt) &&
          !inCircle(encloseTwo(*jt, p), *it) &&
          allInCircle(encloseThree(*it, *jt, p), perimeter)) {
        new_per.push_back(*it);
        new_per.push_back(*jt);
        new_per.push_back(p);
        return new_per;
      }
    }
  }
  cpp11::stop("Error in extendPerimeter: Could not enclose points");
}
Circle enclosePoints(std::vector<Point> points) {
  std::vector<Point>::iterator it = points.begin();
  Circle center = {0.0, 0.0, 0.0};
  std::vector<Point> perimeter;
  while (it != points.end()) {
    if (inCircle(center, *it)) {
      ++it;
    } else {
      perimeter = extendPerimeter(perimeter, *it);
      center = encloseDefault(perimeter);
      it = points.begin();
    }
  }
  return center;
}

[[cpp11::register]]
cpp11::writable::data_frame enclose_points(cpp11::doubles x, cpp11::doubles y, cpp11::integers id) {
  if (x.size() != y.size() || x.size() != id.size()) {
    cpp11::stop("x, y, and id must have same dimensions");
  }
  cpp11::writable::doubles x0;
  cpp11::writable::doubles y0;
  cpp11::writable::doubles  r;
  std::vector< std::vector<Point> > all_points;
  std::vector<Point> points;
  all_points.push_back(points);
  int currentId = id[0];
  int i;
  for (i = 0; i < id.size(); ++i) {
    Point p_tmp = {x[i], y[i]};
    if (id[i] != currentId) {
      currentId = id[i];
      std::vector<Point> points;
      all_points.push_back(points);
    }
    all_points.back().push_back(p_tmp);
  }
  for (i = 0; i < all_points.size(); ++i) {
    Circle center = enclosePoints(all_points[i]);
    x0.push_back(center.x);
    y0.push_back(center.y);
    r.push_back(center.r);
  }
  return cpp11::writable::data_frame({
    "x0"_nm = x0,
    "y0"_nm = y0,
    "r"_nm = r
  });
}
