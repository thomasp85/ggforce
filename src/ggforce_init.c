#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _ggforce_bezierPath(SEXP, SEXP, SEXP);
extern SEXP _ggforce_getBeziers(SEXP, SEXP, SEXP, SEXP);
extern SEXP _ggforce_getSplines(SEXP, SEXP, SEXP, SEXP);
extern SEXP _ggforce_splinePath(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_ggforce_bezierPath", (DL_FUNC) &_ggforce_bezierPath, 3},
    {"_ggforce_getBeziers", (DL_FUNC) &_ggforce_getBeziers, 4},
    {"_ggforce_getSplines", (DL_FUNC) &_ggforce_getSplines, 4},
    {"_ggforce_splinePath", (DL_FUNC) &_ggforce_splinePath, 5},
    {NULL, NULL, 0}
};

void R_init_ggforce(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
