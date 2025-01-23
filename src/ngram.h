#define TEXTRECIPES_NGRAM_H

#define R_NO_REMAP
#include <Rinternals.h>
#include <stdbool.h>

SEXP ffi_ngram(SEXP x, SEXP n, SEXP n_min, SEXP delim);
