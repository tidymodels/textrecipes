#include <Rinternals.h>
#include "ngram.h"

static const R_CallMethodDef CallEntries[] = {
    {"ffi_ngram", (DL_FUNC) &ffi_ngram, 4},

    {NULL, NULL, 0}
};

void R_init_textrecipes(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
