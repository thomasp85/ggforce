#include <Rcpp.h>

using namespace Rcpp;

double Bezier2(double t, NumericVector w) {
    double t2 = t * t;
    double mt = 1-t;
    double mt2 = mt * mt;
    return w[0]*mt2 + w[1]*2*mt*t + w[2]*t2;
}
double Bezier3(double t, NumericVector w) {
    double t2 = t * t;
    double t3 = t2 * t;
    double mt = 1-t;
    double mt2 = mt * mt;
    double mt3 = mt2 * mt;
    return w[0]*mt3 + 3*w[1]*mt2*t + 3*w[2]*mt*t2 + w[3]*t3;
}
// [[Rcpp::export]]
NumericMatrix bezierPath(NumericVector x, NumericVector y, int detail) {
    NumericMatrix res(detail, 2);
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
        stop("Only support for quadratic and cubic beziers");
    }
    res(detail, 0) = x[x.size() - 1];
    res(detail, 1) = y[y.size() - 1];
    return res;
}
// [[Rcpp::export]]
List getBeziers(NumericVector x, NumericVector y, IntegerVector id,
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
    NumericMatrix path;
    for (int i = 0; i < nPaths; i++) {
        controlInd = Range(controlsStart, controlsStart + nControls[i] - 1);
        pathInd = Range(pathStart, pathStart + detail - 1);
        path = bezierPath(x[controlInd], y[controlInd], detail);
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
