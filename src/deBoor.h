// Taken from https://chi3x10.wordpress.com/2009/10/18/de-boor-algorithm-in-c/

#include <vector>

// Class for dealing with points/vectors in a 2-dimensional space
class Point {
public:
    double x;
    double y;

    Point();
    Point(double xInit, double yInit);

    // copy assignment operator
    Point operator=(const Point pt);

    // Arithmatic operators
    Point operator+(const Point pt) const;
    Point operator*(double m) const;
    Point operator/(double m) const;
};

// Find the interval in knots where x resides
int whichInterval(double x, std::vector<double> knots);

// Calculate the position along the B-spline given by x
// The spline is defined by the degree, knots and ctrlPoints. When calling k
// should equal degree but due to the recursive nature k will decrease to
// zero during recursion. i gives the interval in knots that x resides in
Point deBoor(int k, int degree, int i, double x, std::vector<double> knots,
             std::vector<Point> ctrlPoints);
