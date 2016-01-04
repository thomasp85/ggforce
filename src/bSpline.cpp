#include <Rcpp.h>
#include "deBoor.h"
using namespace Rcpp;

std::vector<double> createKnots(int nControl, int degree) {
    int nKnots = nControl + degree + 1;

    std::vector<double> knots (nKnots, 0);
    for (int i = 0; i < nKnots; i++) {
        if (i < degree + 1) {
            knots[i] = 0;
        } else if (i < nKnots - degree) {
            knots[i] = knots[i-1] + 1;
        } else {
            knots[i] = knots[i-1];
        }
    }
    return knots;
}
std::vector<Point> createControls(NumericVector x, NumericVector y) {
    int nControls = x.size();
    std::vector<Point> controls(nControls, Point());
    for (int i = 0; i < nControls; i++) {
        controls[i] = Point(x[i], y[i]);
    }
    return controls;
}
// [[Rcpp::export]]
NumericMatrix splinePath(NumericVector x, NumericVector y, int degree, std::vector<double> knots, int detail) {
    std::vector<Point> controls = createControls(x, y);
    NumericMatrix res(detail, 2);
    double zJump = knots[knots.size()-1] / double(detail-1);
    double z;
    Point point;
    for (int i = 0; i < detail; i++) {
        if (i == detail-1) {
            point = controls[controls.size()-1];
        } else {
            z = i * zJump;
            int zInt = whichInterval(z, knots);
            point = deBoor(degree, degree, zInt, z, knots, controls);
        }
        res(i, 0) = point.x;
        res(i, 1) = point.y;
    }
    return res;
}
// [[Rcpp::export]]
List getSplines(NumericVector x, NumericVector y, IntegerVector id,
                         int detail) {
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
    NumericMatrix paths(nPaths * detail, 2);
    IntegerVector pathsID(nPaths * detail);
    int controlsStart = 0;
    IntegerVector controlInd;
    int pathStart = 0;
    IntegerVector pathInd;
    IntegerVector::iterator pathIter;
    int degree;
    std::vector<double> knots;
    NumericMatrix path;
    for (int i = 0; i < nPaths; i++) {
        degree = nControls[i] <= 3 ? nControls[i] - 1 : 3;
        knots = createKnots(nControls[i], degree);
        controlInd = Range(controlsStart, controlsStart + nControls[i] - 1);
        pathInd = Range(pathStart, pathStart + detail - 1);
        path = splinePath(x[controlInd], y[controlInd], degree, knots, detail);
        int j = 0;
        for (pathIter = pathInd.begin(); pathIter != pathInd.end(); pathIter++) {
            pathsID[*pathIter] = pathID[i];
            paths(*pathIter, 0) = path(j, 0);
            paths(*pathIter, 1) = path(j, 1);
            j++;
        }
        controlsStart += nControls[i];
        pathStart += detail;
    }
    return List::create(Named("paths") = paths,
                        Named("pathID") = pathsID);
}
