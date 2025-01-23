#' Nram generator
#' 
#' @keywords internal
#' @export
ngram <- function(x, n, min_n, delim) {
  .Call(ffi_ngram, x)
}
