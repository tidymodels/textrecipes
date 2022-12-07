#include <cpp11.hpp>
#include <string>

static
void
fill_one_ngram(const cpp11::strings& x,
               int n,
               const std::string& delim,
               cpp11::writable::strings& out,
               R_xlen_t& loc) {
  const R_xlen_t x_size = x.size();
  const R_xlen_t range = std::max(x_size - n + 1, static_cast<R_xlen_t>(0));
  
  // Not strictly necessary to call `unwind_protect()` here because we also
  // call it in `cpp11_ngram()`, but it seems like it would be good practice
  // to call it here too because this is where we leave cpp11 and use the less
  // safe but faster R API directly.
  cpp11::unwind_protect([&] {
    for (R_xlen_t i = 0; i < range; ++i) {
      // `x[i]` goes through `r_string`, and that is too expensive because
      // generating each `r_string` involves protecting each CHARSXP.
      std::string elt = CHAR(STRING_ELT(x, i));
      
      for (R_xlen_t j = 1; j < n; ++j) {
        const std::string piece = CHAR(STRING_ELT(x, i + j));
        elt = elt + delim + piece;
      }
      
      // `out[i] = elt` would approximately do the same thing, but it goes
      // through `std::string elt` -> `r_string` before assigning, and that
      // again involves protecting each `r_string`, which is expensive and not
      // necessary.
      SET_STRING_ELT(out, loc, Rf_mkCharLenCE(elt.data(), elt.size(), CE_UTF8));
      ++loc;
    }
  });
}

static
cpp11::writable::strings
ngram(const cpp11::strings& x,
      int n,
      int n_min,
      const std::string& delim) {
  R_xlen_t out_size = 0;
  const R_xlen_t x_size = x.size();
  
  for (int i = n_min; i <= n; ++i) {
    out_size += std::max(x_size - i + 1, static_cast<R_xlen_t>(0));
  }
  
  cpp11::writable::strings out(out_size);
  R_xlen_t loc = 0;
  
  for (int i = n_min; i <= n; ++i) {
    fill_one_ngram(x, i, delim, out, loc);
  }
  
  return(out);
}

[[cpp11::register]]
cpp11::writable::list_of<cpp11::writable::strings>
cpp11_ngram(cpp11::list_of<cpp11::strings> x,
            int n,
            int n_min,
            std::string delim) {
  if (n <= 0) {
    cpp11::stop("n must be a positive integer.");
  }
  if (n_min <= 0) {
    cpp11::stop("n_min must be a positive integer.");
  }
  if (n_min > n) {
    cpp11::stop("n_min must be larger then n.");
  }
  
  const R_xlen_t x_size = x.size();
  cpp11::writable::list_of<cpp11::writable::strings> out(x_size);
  
  // Calling `unwind_protect()` here because each call to `ngram()` allocates
  // a `cpp11::writable::strings` vector, and that allocation calls
  // `unwind_protect()` too, so `unwind_protect()` would be run `x_size` times.
  // Calling it here "turns off" the nested inner calls to it.
  cpp11::unwind_protect([&] {
    for (R_xlen_t i = 0; i < x_size; ++i) {
      out[i] = ngram(x[i], n, n_min, delim);
    }
  });
  
  return(out);
}