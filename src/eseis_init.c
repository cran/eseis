#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _eseis_stalta_event_freeze(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _eseis_stalta_event_nofreeze(SEXP, SEXP, SEXP, SEXP);
extern SEXP _eseis_kurtosis_event(SEXP, SEXP);
extern SEXP _eseis_run_cor(SEXP, SEXP, SEXP);
extern SEXP _eseis_correlation_event(SEXP, SEXP);
extern SEXP parseMiniSEED(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_eseis_stalta_event_freeze",   (DL_FUNC) &_eseis_stalta_event_freeze,   5},
    {"_eseis_stalta_event_nofreeze", (DL_FUNC) &_eseis_stalta_event_nofreeze, 4},
    {"_eseis_kurtosis_event", (DL_FUNC) &_eseis_kurtosis_event, 2},
    {"_eseis_run_cor", (DL_FUNC) &_eseis_run_cor, 3},
    {"_eseis_correlation_event", (DL_FUNC) &_eseis_correlation_event, 2},
    {"parseMiniSEED", (DL_FUNC) &parseMiniSEED, 1},
    {NULL, NULL, 0}
};

void R_init_eseis(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
