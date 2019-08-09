# circumplex

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/circumplex
* URL: https://github.com/jmgirard/circumplex
* BugReports: https://github.com/jmgirard/circumplex/issues
* Date/Publication: 2019-05-15 18:00:03 UTC
* Number of recursive dependencies: 94

Run `revdep_details(,"circumplex")` for more info

</details>

## In both

*   checking whether package ‘circumplex’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/thomas/Dropbox/GitHub/ggforce/revdep/checks.noindex/circumplex/new/circumplex.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘circumplex’ ...
** package ‘circumplex’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/circumplex/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘circumplex’
* removing ‘/Users/thomas/Dropbox/GitHub/ggforce/revdep/checks.noindex/circumplex/new/circumplex.Rcheck/circumplex’

```
### CRAN

```
* installing *source* package ‘circumplex’ ...
** package ‘circumplex’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/circumplex/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/circumplex/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘circumplex’
* removing ‘/Users/thomas/Dropbox/GitHub/ggforce/revdep/checks.noindex/circumplex/old/circumplex.Rcheck/circumplex’

```
# RxODE

<details>

* Version: 0.9.1-3
* Source code: https://github.com/cran/RxODE
* URL: https://nlmixrdevelopment.github.io/RxODE/
* BugReports: https://github.com/nlmixrdevelopment/RxODE/issues
* Date/Publication: 2019-08-06 15:20:05 UTC
* Number of recursive dependencies: 118

Run `revdep_details(,"RxODE")` for more info

</details>

## In both

*   checking whether package ‘RxODE’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/thomas/Dropbox/GitHub/ggforce/revdep/checks.noindex/RxODE/new/RxODE.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘installr’
    ```

## Installation

### Devel

```
* installing *source* package ‘RxODE’ ...
** package ‘RxODE’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gcc... clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ISO C89... none needed
checking for clang option to support OpenMP... unsupported
configure: creating ./config.status
config.status: creating src/Makevars
--------[begin src/Makevars]--------
# -*- mode: makefile-gmake -*-
CXX_STD     = CXX11

################################################################################
## Package library flags.
PKG_LIBS    = $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS) 

# Release options
PKG_CFLAGS  =  -D_isRxODE_
PKG_CPPFLAGS =  -D_isRxODE_

SOURCES_C = call_dvode.c dop853.c tran.c omegaChol.c init.c par_solve.c cfode.c common.c corfailure.c correction.c daxpy.c ddot.c dgefa.c dgesl.c dscal.c fnorm.c idamax.c intdy.c lsoda.c methodswitch.c orderswitch.c prja.c scaleh.c solsy.c stoda.c vmnorm.c strdup_printf.c rprintf.c lincmt.c box.c 
SOURCES_CPP = RcppExports.cpp WinDrive.cpp rxInv.cpp rxData.cpp etTran.cpp et.cpp rxStack.cpp
SOURCES_FORTAN = dlsoda.f opkda2.f

OBJECTS = $(SOURCES_C:.c=.o) $(SOURCES_FORTAN:.f=.o) $(SOURCES_CPP:.cpp=.o)

.PHONY: all 
all: $(SHLIB)
#$(SHLIB): mylibs
# mylibs:  
# 	$(CC) -I"$(R_INCLUDE_DIR)" $(CFLAGS) $(CPPFLAGS) $(SHLIB_CFLAGS) $(CPICFLAGS) -I../inst/include/ ../inst/include/RxODE_model.h || $(CC) --version


--------[end src/Makevars]--------
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c call_dvode.c -o call_dvode.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c dop853.c -o dop853.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c tran.c -o tran.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c omegaChol.c -o omegaChol.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c init.c -o init.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c par_solve.c -o par_solve.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c cfode.c -o cfode.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c common.c -o common.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c corfailure.c -o corfailure.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c correction.c -o correction.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c daxpy.c -o daxpy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c ddot.c -o ddot.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c dgefa.c -o dgefa.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c dgesl.c -o dgesl.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c dscal.c -o dscal.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c fnorm.c -o fnorm.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c idamax.c -o idamax.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c intdy.c -o intdy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c lsoda.c -o lsoda.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c methodswitch.c -o methodswitch.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c orderswitch.c -o orderswitch.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c prja.c -o prja.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c scaleh.c -o scaleh.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c solsy.c -o solsy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c stoda.c -o stoda.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c vmnorm.c -o vmnorm.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c strdup_printf.c -o strdup_printf.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c rprintf.c -o rprintf.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c lincmt.c -o lincmt.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c box.c -o box.o
gfortran  -fPIC  -Wall -g -O2  -c dlsoda.f -o dlsoda.o
dlsoda.f:705:0:

       IF (IREDO .EQ. 0) GO TO 690
 
Warning: 'iredo' may be used uninitialized in this function [-Wmaybe-uninitialized]
dlsoda.f:499:0:

      1   R, RH, RHDN, RHSM, RHUP, TOLD, DMNORM
 
Warning: 'rh' may be used uninitialized in this function [-Wmaybe-uninitialized]
dlsoda.f:2874:0:

       IF (IHIT) T = TCRIT
 
Warning: 'tcrit' may be used uninitialized in this function [-Wmaybe-uninitialized]
dlsoda.f:2874:0: Warning: 'ihit' may be used uninitialized in this function [-Wmaybe-uninitialized]
dlsoda.f:2615:0:

       IF (H0 .NE. 0.0D0 .AND. (T + H0 - TCRIT)*H0 .GT. 0.0D0)
 
Warning: 'h0' may be used uninitialized in this function [-Wmaybe-uninitialized]
dlsoda.f:2537:0:

       LEN1S = LEN1S + LENWM
 
Warning: 'lenwm' may be used uninitialized in this function [-Wmaybe-uninitialized]
dlsoda.f:2536:0:

       IF (JT .GE. 4) LENWM = (2*ML + MU + 1)*N + 2
 
Warning: 'mu' may be used uninitialized in this function [-Wmaybe-uninitialized]
dlsoda.f:2536:0: Warning: 'ml' may be used uninitialized in this function [-Wmaybe-uninitialized]
gfortran  -fPIC  -Wall -g -O2  -c opkda2.f -o opkda2.o
opkda2.f:650:0:

       INTEGER FUNCTION IXSAV (IPAR, IVALUE, ISET)
 
Warning: '__result_ixsav' may be used uninitialized in this function [-Wmaybe-uninitialized]
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c WinDrive.cpp -o WinDrive.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c rxInv.cpp -o rxInv.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c rxData.cpp -o rxData.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c etTran.cpp -o etTran.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c et.cpp -o et.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/ggforce/new/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c rxStack.cpp -o rxStack.o
clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o RxODE.so call_dvode.o dop853.o tran.o omegaChol.o init.o par_solve.o cfode.o common.o corfailure.o correction.o daxpy.o ddot.o dgefa.o dgesl.o dscal.o fnorm.o idamax.o intdy.o lsoda.o methodswitch.o orderswitch.o prja.o scaleh.o solsy.o stoda.o vmnorm.o strdup_printf.o rprintf.o lincmt.o box.o dlsoda.o opkda2.o RcppExports.o WinDrive.o rxInv.o rxData.o etTran.o et.o rxStack.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [RxODE.so] Error 1
ERROR: compilation failed for package ‘RxODE’
* removing ‘/Users/thomas/Dropbox/GitHub/ggforce/revdep/checks.noindex/RxODE/new/RxODE.Rcheck/RxODE’

```
### CRAN

```
* installing *source* package ‘RxODE’ ...
** package ‘RxODE’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gcc... clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ISO C89... none needed
checking for clang option to support OpenMP... unsupported
configure: creating ./config.status
config.status: creating src/Makevars
--------[begin src/Makevars]--------
# -*- mode: makefile-gmake -*-
CXX_STD     = CXX11

################################################################################
## Package library flags.
PKG_LIBS    = $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS) 

# Release options
PKG_CFLAGS  =  -D_isRxODE_
PKG_CPPFLAGS =  -D_isRxODE_

SOURCES_C = call_dvode.c dop853.c tran.c omegaChol.c init.c par_solve.c cfode.c common.c corfailure.c correction.c daxpy.c ddot.c dgefa.c dgesl.c dscal.c fnorm.c idamax.c intdy.c lsoda.c methodswitch.c orderswitch.c prja.c scaleh.c solsy.c stoda.c vmnorm.c strdup_printf.c rprintf.c lincmt.c box.c 
SOURCES_CPP = RcppExports.cpp WinDrive.cpp rxInv.cpp rxData.cpp etTran.cpp et.cpp rxStack.cpp
SOURCES_FORTAN = dlsoda.f opkda2.f

OBJECTS = $(SOURCES_C:.c=.o) $(SOURCES_FORTAN:.f=.o) $(SOURCES_CPP:.cpp=.o)

.PHONY: all 
all: $(SHLIB)
#$(SHLIB): mylibs
# mylibs:  
# 	$(CC) -I"$(R_INCLUDE_DIR)" $(CFLAGS) $(CPPFLAGS) $(SHLIB_CFLAGS) $(CPICFLAGS) -I../inst/include/ ../inst/include/RxODE_model.h || $(CC) --version


--------[end src/Makevars]--------
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c call_dvode.c -o call_dvode.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c dop853.c -o dop853.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c tran.c -o tran.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c omegaChol.c -o omegaChol.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c init.c -o init.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c par_solve.c -o par_solve.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c cfode.c -o cfode.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c common.c -o common.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c corfailure.c -o corfailure.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c correction.c -o correction.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c daxpy.c -o daxpy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c ddot.c -o ddot.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c dgefa.c -o dgefa.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c dgesl.c -o dgesl.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c dscal.c -o dscal.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c fnorm.c -o fnorm.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c idamax.c -o idamax.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c intdy.c -o intdy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c lsoda.c -o lsoda.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c methodswitch.c -o methodswitch.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c orderswitch.c -o orderswitch.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c prja.c -o prja.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c scaleh.c -o scaleh.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c solsy.c -o solsy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c stoda.c -o stoda.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c vmnorm.c -o vmnorm.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c strdup_printf.c -o strdup_printf.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c rprintf.c -o rprintf.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c lincmt.c -o lincmt.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -D_isRxODE_ -fPIC  -Wall -g -O2  -c box.c -o box.o
gfortran  -fPIC  -Wall -g -O2  -c dlsoda.f -o dlsoda.o
dlsoda.f:705:0:

       IF (IREDO .EQ. 0) GO TO 690
 
Warning: 'iredo' may be used uninitialized in this function [-Wmaybe-uninitialized]
dlsoda.f:499:0:

      1   R, RH, RHDN, RHSM, RHUP, TOLD, DMNORM
 
Warning: 'rh' may be used uninitialized in this function [-Wmaybe-uninitialized]
dlsoda.f:2874:0:

       IF (IHIT) T = TCRIT
 
Warning: 'tcrit' may be used uninitialized in this function [-Wmaybe-uninitialized]
dlsoda.f:2874:0: Warning: 'ihit' may be used uninitialized in this function [-Wmaybe-uninitialized]
dlsoda.f:2615:0:

       IF (H0 .NE. 0.0D0 .AND. (T + H0 - TCRIT)*H0 .GT. 0.0D0)
 
Warning: 'h0' may be used uninitialized in this function [-Wmaybe-uninitialized]
dlsoda.f:2537:0:

       LEN1S = LEN1S + LENWM
 
Warning: 'lenwm' may be used uninitialized in this function [-Wmaybe-uninitialized]
dlsoda.f:2536:0:

       IF (JT .GE. 4) LENWM = (2*ML + MU + 1)*N + 2
 
Warning: 'mu' may be used uninitialized in this function [-Wmaybe-uninitialized]
dlsoda.f:2536:0: Warning: 'ml' may be used uninitialized in this function [-Wmaybe-uninitialized]
gfortran  -fPIC  -Wall -g -O2  -c opkda2.f -o opkda2.o
opkda2.f:650:0:

       INTEGER FUNCTION IXSAV (IPAR, IVALUE, ISET)
 
Warning: '__result_ixsav' may be used uninitialized in this function [-Wmaybe-uninitialized]
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c WinDrive.cpp -o WinDrive.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c rxInv.cpp -o rxInv.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c rxData.cpp -o rxData.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c etTran.cpp -o etTran.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c et.cpp -o et.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_isRxODE_ -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/dparser/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/thomas/Dropbox/GitHub/ggforce/revdep/library.noindex/RxODE/PreciseSums/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c rxStack.cpp -o rxStack.o
clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o RxODE.so call_dvode.o dop853.o tran.o omegaChol.o init.o par_solve.o cfode.o common.o corfailure.o correction.o daxpy.o ddot.o dgefa.o dgesl.o dscal.o fnorm.o idamax.o intdy.o lsoda.o methodswitch.o orderswitch.o prja.o scaleh.o solsy.o stoda.o vmnorm.o strdup_printf.o rprintf.o lincmt.o box.o dlsoda.o opkda2.o RcppExports.o WinDrive.o rxInv.o rxData.o etTran.o et.o rxStack.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [RxODE.so] Error 1
ERROR: compilation failed for package ‘RxODE’
* removing ‘/Users/thomas/Dropbox/GitHub/ggforce/revdep/checks.noindex/RxODE/old/RxODE.Rcheck/RxODE’

```
# units

<details>

* Version: 0.6-3
* Source code: https://github.com/cran/units
* URL: https://github.com/r-quantities/units/
* BugReports: https://github.com/r-quantities/units/issues/
* Date/Publication: 2019-05-03 21:50:03 UTC
* Number of recursive dependencies: 60

Run `revdep_details(,"units")` for more info

</details>

## In both

*   checking whether package ‘units’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/thomas/Dropbox/GitHub/ggforce/revdep/checks.noindex/units/new/units.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘units’ ...
** package ‘units’ successfully unpacked and MD5 sums checked
** using staged installation
configure: units: 0.6-3
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C++ compiler... yes
checking whether clang++ -std=gnu++11 accepts -g... yes
checking how to run the C++ preprocessor... clang++ -std=gnu++11 -E
checking for grep that handles long lines and -e... /usr/bin/grep
checking for egrep... /usr/bin/grep -E
checking for ANSI C header files... rm: conftest.dSYM: is a directory
rm: conftest.dSYM: is a directory
yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking for stdbool.h that conforms to C99... no
checking for _Bool... no
checking for error_at_line... no
checking for gcc... clang
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ISO C89... none needed
checking for XML_ParserCreate in -lexpat... yes
checking udunits2.h usability... no
checking udunits2.h presence... no
checking for udunits2.h... no
checking udunits2/udunits2.h usability... no
checking udunits2/udunits2.h presence... no
checking for udunits2/udunits2.h... no
checking for ut_read_xml in -ludunits2... no
configure: error: in `/Users/thomas/Dropbox/GitHub/ggforce/revdep/checks.noindex/units/new/units.Rcheck/00_pkg_src/units':
configure: error: 
--------------------------------------------------------------------------------
  Configuration failed because libudunits2.so was not found. Try installing:
    * deb: libudunits2-dev (Debian, Ubuntu, ...)
    * rpm: udunits2-devel (Fedora, EPEL, ...)
    * brew: udunits (OSX)
  If udunits2 is already installed in a non-standard location, use:
    --configure-args='--with-udunits2-lib=/usr/local/lib'
  if the library was not found, and/or:
    --configure-args='--with-udunits2-include=/usr/include/udunits2'
  if the header was not found, replacing paths with appropriate values.
  You can alternatively set UDUNITS2_INCLUDE and UDUNITS2_LIBS manually.
--------------------------------------------------------------------------------

See `config.log' for more details
ERROR: configuration failed for package ‘units’
* removing ‘/Users/thomas/Dropbox/GitHub/ggforce/revdep/checks.noindex/units/new/units.Rcheck/units’

```
### CRAN

```
* installing *source* package ‘units’ ...
** package ‘units’ successfully unpacked and MD5 sums checked
** using staged installation
configure: units: 0.6-3
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C++ compiler... yes
checking whether clang++ -std=gnu++11 accepts -g... yes
checking how to run the C++ preprocessor... clang++ -std=gnu++11 -E
checking for grep that handles long lines and -e... /usr/bin/grep
checking for egrep... /usr/bin/grep -E
checking for ANSI C header files... rm: conftest.dSYM: is a directory
rm: conftest.dSYM: is a directory
yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking for stdbool.h that conforms to C99... no
checking for _Bool... no
checking for error_at_line... no
checking for gcc... clang
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ISO C89... none needed
checking for XML_ParserCreate in -lexpat... yes
checking udunits2.h usability... no
checking udunits2.h presence... no
checking for udunits2.h... no
checking udunits2/udunits2.h usability... no
checking udunits2/udunits2.h presence... no
checking for udunits2/udunits2.h... no
checking for ut_read_xml in -ludunits2... no
configure: error: in `/Users/thomas/Dropbox/GitHub/ggforce/revdep/checks.noindex/units/old/units.Rcheck/00_pkg_src/units':
configure: error: 
--------------------------------------------------------------------------------
  Configuration failed because libudunits2.so was not found. Try installing:
    * deb: libudunits2-dev (Debian, Ubuntu, ...)
    * rpm: udunits2-devel (Fedora, EPEL, ...)
    * brew: udunits (OSX)
  If udunits2 is already installed in a non-standard location, use:
    --configure-args='--with-udunits2-lib=/usr/local/lib'
  if the library was not found, and/or:
    --configure-args='--with-udunits2-include=/usr/include/udunits2'
  if the header was not found, replacing paths with appropriate values.
  You can alternatively set UDUNITS2_INCLUDE and UDUNITS2_LIBS manually.
--------------------------------------------------------------------------------

See `config.log' for more details
ERROR: configuration failed for package ‘units’
* removing ‘/Users/thomas/Dropbox/GitHub/ggforce/revdep/checks.noindex/units/old/units.Rcheck/units’

```
