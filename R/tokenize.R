#' Tokenization of character variables
#'
#' `step_tokenize` creates a *specification* of a recipe step that
#'  will convert a character predictor into a list of its tokenized parts.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_tokenize`, this indicates the variables to be encoded
#'  into a list column. See [recipes::selections()] for more details. For
#'  the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake.recipe()]? While all operations are baked
#'  when [recipes::prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param trained A logical to indicate if the recipe has been baked.
#' @return An updated version of `recipe` with the new step added
#'  to the sequence of existing steps (if any).
#' @keywords datagen 
#' @concept preprocessing encoding
#' @export
#' @importFrom recipes add_step step terms_select sel2char ellipse_check
step_tokenize <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           skip = FALSE
  ) {
    add_step(
      recipe,
      step_tokenize_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        skip = skip
      )
    )
  }

step_tokenize_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           columns = NULL,
           skip = FALSE) {
    step(
      subclass = "tokenize",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip
    )
  }

#' @export
prep.step_tokenize <- function(x, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  step_tokenize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip
  )
}

#' @export
#' @importFrom tibble as_tibble
#' @importFrom recipes bake prep
bake.step_tokenize <- function(object, newdata, ...) {
  col_names <- object$columns
  # for backward compat
  
  for (i in seq_along(col_names)) {
    newdata[, col_names[i]] <- token_fun(newdata[, col_names[i]], col_names[i])
  }
  as_tibble(newdata)
}


token_fun <- function(data, name) {
  out <- tibble::tibble(tokenizers::tokenize_words(dplyr::pull(data, 1)))
  names(out) <- name
  out
}
