#' Role Selection
#' 
#' @description
#' `all_tokenized()` selects all tokenlist columns, `all_tokenized_predictors()`
#' selects all predictor tokenlist columns.
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