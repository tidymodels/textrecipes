#include <Rcpp.h>
using namespace Rcpp;

CharacterVector ngram_single(CharacterVector x, int n, String delim) {
  
  if (n == 1) {
    return(x);
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

CharacterVector ngram(CharacterVector x, int n, int n_min, String delim) {

  int res_len = 0;
  int x_len = x.length();
  
  for (int i = n_min; i <= n; ++i) {
    res_len += std::max(x_len - i + 1, 0);
  }
  
  CharacterVector res (res_len);
  CharacterVector temp_res;
  int index = 0;
  
  for (int i = n_min; i <= n; ++i) {
    
    temp_res = ngram_single(x, i, delim);
    int temp_res_len = temp_res.size();
      
    for(int j = 0; j < temp_res_len; ++j) {
      res[index] = temp_res[j];
      ++index;
    }
  }
  
  return(res);
}
  
//' ngram generator
//'
//' @param x list of character vectors
//' @param n number of grams
//' @param n_min minimum number of grams
//' @param delim delimiter
//'
// [[Rcpp::export]]
List rcpp_ngram(List x, int n, int n_min, String delim) {
  
  if (n <= 0) {
    stop("'n' must be a positive integer.");
  }
  
  if (n_min <= 0) {
    stop("'n_min' must be a positive integer.");
  }
  
  if (n_min > n) {
    stop("'n_min' must be larger then 'n'.");
  }
  
  
  int len = x.length();
  List res (len);
  
  for (int i = 0; i < len; ++i) {
    res[i] = ngram(x[i], n, n_min, delim);
  }
  return(res);
}
