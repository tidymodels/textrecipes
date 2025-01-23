#include "ngram.h"
#include <string.h>
#include "Rinternals.h"

int round_from_zero(int x) {
  if (x > 0) {
    return x;
  }

  return 0;
}

void fill_one_ngram(
    const SEXP x,
    int n,
    const char* delim,
    SEXP out,
    R_xlen_t* loc
) {
  const R_xlen_t x_size = Rf_xlength(x);
  const R_xlen_t range = round_from_zero(x_size - n + 1);

  R_xlen_t out_char_size = 1;  // 1 for NULL

  for (R_xlen_t i = 0; i < range; ++i) {
    out_char_size = out_char_size + strlen(CHAR(STRING_ELT(x, i)));

    for (R_xlen_t j = 1; j < n; ++j) {
      out_char_size = out_char_size + strlen(CHAR(STRING_ELT(x, i + j)));
      out_char_size = out_char_size + strlen(delim);
    }
  }

  // TODO define the length correctly
  char* out_elt = R_alloc(out_char_size, sizeof(char));

  for (R_xlen_t i = 0; i < range; ++i) {
    const char* elt = CHAR(STRING_ELT(x, i));
    strcpy(out_elt, elt);

    for (R_xlen_t j = 1; j < n; ++j) {
      const char* piece = CHAR(STRING_ELT(x, i + j));

      strcat(out_elt, delim);
      strcat(out_elt, piece);
    }

    SET_STRING_ELT(out, (*loc), Rf_mkCharCE(out_elt, CE_UTF8));
    ++(*loc);
  }
}

SEXP ngram(SEXP x, int n, int n_min, const char* delim) {
  R_xlen_t out_size = 0;
  const R_xlen_t x_size = Rf_xlength(x);

  for (int i = n_min; i <= n; ++i) {
    out_size += round_from_zero(x_size - i + 1);
  }

  SEXP out = PROTECT(Rf_allocVector(STRSXP, out_size));

  R_xlen_t loc = 0;
  R_xlen_t* loc_ptr;

  loc_ptr = &loc;

  for (int i = n_min; i <= n; ++i) {
    fill_one_ngram(x, i, delim, out, loc_ptr);
  }

  UNPROTECT(1);
  return out;
}

SEXP ffi_ngram(SEXP x, SEXP n, SEXP n_min, SEXP delim) {
  int n_val = INTEGER_ELT(n, 0);
  int n_min_val = INTEGER_ELT(n_min, 0);
  const char* delim_val = R_CHAR(STRING_ELT(delim, 0));

  if (n_val <= 0) {
    Rf_error("n must be a positive integer.");
  }
  if (n_min_val <= 0) {
    Rf_error("n_min must be a positive integer.");
  }
  if (n_min_val > n_val) {
    Rf_error("n_min must be less then n.");
  }

  const R_xlen_t x_size = Rf_xlength(x);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, x_size));

  for (R_xlen_t i = 0; i < x_size; ++i) {
    SEXP value = ngram(VECTOR_ELT(x, i), n_val, n_min_val, delim_val);
    SET_VECTOR_ELT(out, i, value);
  }

  UNPROTECT(1);
  return out;
}
