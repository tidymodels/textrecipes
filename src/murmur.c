// MurmurHash3_x86_32 algorithm by Austin Appleby (public domain).
// This is an independent implementation using unsigned arithmetic only, so
// there is no signed overflow or shift. The seeds below match the ones used by
// text2vec::hash_vectorizer() so that the output is identical.

#include "murmur.h"
#include <string.h>

#define HASH_SEED 3120602769U
#define SIGN_SEED 79193439U

static inline uint32_t rotl32(uint32_t x, int r) {
  return (x << r) | (x >> (32 - r));
}

uint32_t murmur3_x86_32(const char* data, size_t len, uint32_t seed) {
  const unsigned char* p = (const unsigned char*) data;
  const uint32_t c1 = 0xcc9e2d51U;
  const uint32_t c2 = 0x1b873593U;

  uint32_t h = seed;
  const size_t n_blocks = len / 4;

  for (size_t i = 0; i < n_blocks; ++i) {
    // Assemble little-endian from bytes: no alignment or endianness issues
    uint32_t k = (uint32_t) p[0] | ((uint32_t) p[1] << 8) |
                 ((uint32_t) p[2] << 16) | ((uint32_t) p[3] << 24);
    p += 4;

    k *= c1;
    k = rotl32(k, 15);
    k *= c2;

    h ^= k;
    h = rotl32(h, 13);
    h = h * 5U + 0xe6546b64U;
  }

  uint32_t k = 0;
  switch (len & 3) {
  case 3:
    k ^= (uint32_t) p[2] << 16;
    // fall through
  case 2:
    k ^= (uint32_t) p[1] << 8;
    // fall through
  case 1:
    k ^= (uint32_t) p[0];
    k *= c1;
    k = rotl32(k, 15);
    k *= c2;
    h ^= k;
  }

  h ^= (uint32_t) len;
  h ^= h >> 16;
  h *= 0x85ebca6bU;
  h ^= h >> 13;
  h *= 0xc2b2ae35U;
  h ^= h >> 16;

  return h;
}

// Hashes every token of every document. Returns list(i, j, x) with 1-based
// document (i) and column (j) indices and the +1/-1 value (x), one entry per
// token occurrence. Duplicates are summed on the R side.
SEXP ffi_hash_tokens(SEXP tokens, SEXP n, SEXP signed_hash) {
  const R_xlen_t n_docs = Rf_xlength(tokens);
  const uint32_t n_buckets = (uint32_t) Rf_asInteger(n);
  const int use_sign = Rf_asLogical(signed_hash);

  R_xlen_t total = 0;
  for (R_xlen_t d = 0; d < n_docs; ++d) {
    total += Rf_xlength(VECTOR_ELT(tokens, d));
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP out_i = SET_VECTOR_ELT(out, 0, Rf_allocVector(INTSXP, total));
  SEXP out_j = SET_VECTOR_ELT(out, 1, Rf_allocVector(INTSXP, total));
  SEXP out_x = SET_VECTOR_ELT(out, 2, Rf_allocVector(INTSXP, total));
  int* p_i = INTEGER(out_i);
  int* p_j = INTEGER(out_j);
  int* p_x = INTEGER(out_x);

  R_xlen_t loc = 0;
  for (R_xlen_t d = 0; d < n_docs; ++d) {
    SEXP doc = VECTOR_ELT(tokens, d);
    const R_xlen_t doc_size = Rf_xlength(doc);

    for (R_xlen_t t = 0; t < doc_size; ++t) {
      const char* str = Rf_translateCharUTF8(STRING_ELT(doc, t));
      const size_t len = strlen(str);

      p_i[loc] = (int) (d + 1);
      p_j[loc] = (int) (murmur3_x86_32(str, len, HASH_SEED) % n_buckets) + 1;

      int value = 1;
      if (use_sign == 1 && murmur3_x86_32(str, len, SIGN_SEED) >= 0x80000000U) {
        value = -1;
      }
      p_x[loc] = value;
      ++loc;
    }
  }

  UNPROTECT(1);
  return out;
}
