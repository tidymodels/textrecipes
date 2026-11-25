#ifndef TEXTRECIPES_MURMUR_H
#define TEXTRECIPES_MURMUR_H

#include <Rinternals.h>
#include <stdint.h>
#include <stddef.h>

uint32_t murmur3_x86_32(const char* data, size_t len, uint32_t seed);

SEXP ffi_hash_tokens(SEXP tokens, SEXP n, SEXP signed_hash);

#endif
