Fix for reverse dependencies that showed in latest submission. All reverse 
dependencies have been checked again and show no problems

## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 21 reverse dependencies (19 from CRAN + 2 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 3 packages

Issues with CRAN packages are summarised below.

### Failed to check

* circumplex (NA)
* RxODE      (NA)
* units      (NA)
