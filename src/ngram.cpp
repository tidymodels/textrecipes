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
  
  std::string out_elt;
  
  // Using `unwind_protect()` manually because character vector extraction and
  // insertion is currently quite slow in cpp11 due to how protection of each
  // CHARSXP is handled. We are very careful to:
  // - Not use any of the cpp11 API here
  // - Avoid exceptions that could be thrown by `std::string` operations
  // - Avoid `std::string` creation inside `unwind_protect()`, because its
  //   destructor would not run if an R error occurred.
  cpp11::unwind_protect([&] {
    for (R_xlen_t i = 0; i < range; ++i) {
      // `x[i]` goes through `r_string`, and that is too expensive because
      // generating each `r_string` involves protecting each CHARSXP.
      const char* elt = CHAR(STRING_ELT(x, i));
      
      try {
        out_elt.clear();
        out_elt.assign(elt);
      } catch (...) {
        Rf_errorcall(R_NilValue, "C++ error while assigning.");
      }
      
      for (R_xlen_t j = 1; j < n; ++j) {
        const char* piece = CHAR(STRING_ELT(x, i + j));
        
        try {
          out_elt = out_elt + delim + piece;
        } catch (...) {
          Rf_errorcall(R_NilValue, "C++ error while concatenating.");
        }
      }
      
      // `out[i] = elt` would approximately do the same thing, but it goes
      // through `std::string elt` -> `r_string` before assigning, and that
      // again involves protecting each `r_string`, which is expensive and not
      // necessary.
      // We don't expect `.data()` or `.size()` to ever throw exceptions.
      SET_STRING_ELT(out, loc, Rf_mkCharLenCE(out_elt.data(), out_elt.size(), CE_UTF8));
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
  
  for (R_xlen_t i = 0; i < x_size; ++i) {
    out[i] = ngram(x[i], n, n_min, delim);
  }
  
  return(out);
}
