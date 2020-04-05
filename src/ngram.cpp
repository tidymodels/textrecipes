#include <Rcpp.h>
using namespace Rcpp;

CharacterVector ngram(CharacterVector x, int n, String delim) {
  
  if (n <= 0) {
    stop("'n' must be a positive integer.");
  }
  
  int len = x.length();
  int range = std::max(len - n + 1, 0);
  
  CharacterVector res (range);
  
  if (range != 0) {
    for (int i = 0; i < range; ++i) {
     res[i] = x[i];
      for(int j = 1; j < n; ++j) {
        res[i] += delim;
        res[i] += x[i + j];
      }
    }
  }
  return(res);
}

//' ngram generator
//'
//' @param x list of character vectors
//' @param n number of grams
//' @param delim delimiter
//'
// [[Rcpp::export]]
List rcpp_ngram(List x, int n, String delim) {
  int len = x.length();
  List res (len);
  
  for (int i = 0; i < len; ++i) {
    res[i] = ngram(x[i], n, delim);
  }
  return(res);
}
