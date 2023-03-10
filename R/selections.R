#' Role Selection
#'
#' `all_tokenized()` selects all [`token`][tokenlist()] variables,
#' `all_tokenized_predictors()` selects all predictor [`token`][tokenlist()]
#' variables.
#'
#' @seealso [recipes::has_role()]
#' @export
all_tokenized <- function() {
  recipes::has_type("tokenlist")
}

#' @export
#' @rdname all_tokenized
all_tokenized_predictors <- function() {
  intersect(recipes::has_role("predictor"), recipes::has_type("tokenlist"))
}
