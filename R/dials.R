#' Parameter to determine number of tokens in ngram
#'
#' Used in `step_ngram()`.
#'
#' @inheritParams dials::Laplace
#' @examples
#' n_tokens()
#' @export
n_tokens <- function(range = c(1, 3), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(n_tokens = "Number of tokens"),
    finalize = NULL
  )
}
