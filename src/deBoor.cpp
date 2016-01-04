// Taken from https://chi3x10.wordpress.com/2009/10/18/de-boor-algorithm-in-c/

#include "deBoor.h"

Point::Point() {
    x = 0.0;
    y = 0.0;
}
Point::Point(double xInit, double yInit) {
    x = xInit;
    y = yInit;
}
Point Point::operator=(const Point pt) {
    x = pt.x;
    y = pt.y;
    return *this;
}
Point Point::operator+(const Point pt) const {
    Point temp;
    temp.x = x + pt.x;
    temp.y = y + pt.y;
    return temp;
}
Point Point::operator*(double m) const {
    Point temp;
    temp.x = x*m;
    temp.y = y*m;
    return temp;
}
Point Point::operator/(double m) const {
    Point temp;
    temp.x = x/m;
    temp.y = y/m;
    return temp;
}

Point deBoor(int k, int degree, int i, double x, std::vector<double> knots,
             std::vector<Point> ctrlPoints) {
    // Please see wikipedia page for detail
    // note that the algorithm here kind of traverses in reverse order
    // comapred to that in the wikipedia page
    if(k == 0) {
        return ctrlPoints[i];
    } else {
        double alpha = (x - knots[i])/(knots[i+degree + 1 - k] - knots[i]);
        return (deBoor(k - 1, degree, i - 1, x, knots, ctrlPoints)*(1 - alpha) +
                deBoor(k - 1, degree, i, x, knots, ctrlPoints)*alpha);
    }
}

int whichInterval(double x, std::vector<double> knots) {
    int ti = knots.size();
    for(int i = 1; i < ti - 1; i++) {
        if(x < knots[i])
            return(i - 1);
        else if(x == knots[ti - 1])
            return(ti - 1);
    }
    return -1;
}
