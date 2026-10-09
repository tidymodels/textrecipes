#' @param skip A logical. Should the step be skipped when the recipe is baked
#'   by [recipes::bake()]? While all operations are baked when
#'   [recipes::prep()] is run, some operations may not be able to be conducted
#'   on new data (e.g. processing the outcome variable(s)). Care should be
#'   taken when using `skip = TRUE`, as it may affect the computations for
#'   subsequent operations.
