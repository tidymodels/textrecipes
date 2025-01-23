#' Nram generator
#' 
#' @keywords internal
#' @export
ngram <- function(x, n, n_min, delim) {
  n <- as.integer(n)
  n_min <- as.integer(n_min)
  
  .Call(ffi_ngram, x, n, n_min, delim)
}


