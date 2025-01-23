#' Nram generator
#' 
#' @keywords internal
#' @export
ngram <- function(x) {
  .Call(ffi_ngram, x)
}
