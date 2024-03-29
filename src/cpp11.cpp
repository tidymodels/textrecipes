// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// ngram.cpp
cpp11::writable::list_of<cpp11::writable::strings> cpp11_ngram(cpp11::list_of<cpp11::strings> x, int n, int n_min, std::string delim);
extern "C" SEXP _textrecipes_cpp11_ngram(SEXP x, SEXP n, SEXP n_min, SEXP delim) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp11_ngram(cpp11::as_cpp<cpp11::decay_t<cpp11::list_of<cpp11::strings>>>(x), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<int>>(n_min), cpp11::as_cpp<cpp11::decay_t<std::string>>(delim)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_textrecipes_cpp11_ngram", (DL_FUNC) &_textrecipes_cpp11_ngram, 4},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_textrecipes(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
