#'  Generate the basic set of text features
#'
#' `step_tokenmerge` creates a *specification* of a recipe step that
#'  will take multiple [tokenlist]s and combine them into one
#'  [tokenlist].
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_tokenmerge`, this indicates the variables to be encoded
#'  into a [tokenlist]. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new columns created by the original variables will be
#'  used as predictors in a model.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param prefix A prefix for generated column names, default to "tokenmerge".
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake.recipe()]? While all
#'  operations are baked when [recipes::prep.recipe()] is run, some
#'  operations may not be able to be conducted on new data (e.g.
#'  processing the outcome variable(s)). Care should be taken when
#'  using `skip = TRUE` as it may affect the computations for
#'  subsequent operations.
#' @param id A character string that is unique to this step to identify it
#' @param trained A logical to indicate if the recipe has been
#'  baked.
#' @return An updated version of `recipe` with the new step added
#'  to the sequence of existing steps (if any).
#' @examples
#' library(recipes)
#' library(modeldata)
#' data(okc_text)
#'
#' okc_rec <- recipe(~., data = okc_text) %>%
#'   step_tokenize(essay0, essay1) %>%
#'   step_tokenmerge(essay0, essay1)
#'
#' okc_obj <- okc_rec %>%
#'   prep()
#'
#' bake(okc_obj, new_data = NULL)
#'
#' tidy(okc_rec, number = 1)
#' tidy(okc_obj, number = 1)
#' @export
#'
#' @seealso [step_tokenize()] to turn character into tokenlist.
#' @family tokenlist to tokenlist steps
step_tokenmerge <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           columns = NULL,
           prefix = "tokenmerge",
           skip = FALSE,
           id = rand_id("tokenmerge")) {
    add_step(
      recipe,
      step_tokenmerge_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

step_tokenmerge_new <-
  function(terms, role, trained, columns, prefix,
           skip, id) {
    step(
      subclass = "tokenmerge",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_tokenmerge <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  check_list(training[, col_names])

  step_tokenmerge_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_tokenmerge <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  new_col <- as.list(unname(new_data[, col_names, drop = FALSE])) %>%
    map(get_tokens) %>%
    pmap(c)
  new_col <- tibble(tokenlist(new_col))
  names(new_col) <- object$prefix

  new_data <-
    new_data[, !(colnames(new_data) %in% col_names), drop = FALSE]

  new_data <- vctrs::vec_cbind(new_data, new_col)

  as_tibble(new_data)
}

#' @export
print.step_tokenmerge <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Merging tokens for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_tokenmerge
#' @param x A `step_tokenmerge` object.
#' @export
tidy.step_tokenmerge <- function(x, ...) {
  if (is_trained(x)) {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}
