#' Combine Multiple Token Variables Into One
#'
#' `step_tokenmerge()` creates a *specification* of a recipe step that will take
#' multiple [`token`][tokenlist()] variables and combine them into one
#' [`token`][tokenlist()] variable.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param prefix A prefix for generated column names, defaults to "tokenmerge".
#' @template args-keep_original_cols
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @seealso [step_tokenize()] to turn characters into [`tokens`][tokenlist()]
#' @family Steps for Token Modification
#'
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~., data = tate_text) %>%
#'   step_tokenize(medium, artist) %>%
#'   step_tokenmerge(medium, artist)
#'
#' tate_obj <- tate_rec %>%
#'   prep()
#'
#' bake(tate_obj, new_data = NULL)
#'
#' tidy(tate_rec, number = 2)
#' tidy(tate_obj, number = 2)
#' @export
step_tokenmerge <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    columns = NULL,
    prefix = "tokenmerge",
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("tokenmerge")
  ) {
    add_step(
      recipe,
      step_tokenmerge_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_tokenmerge_new <-
  function(
    terms,
    role,
    trained,
    columns,
    prefix,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "tokenmerge",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_tokenmerge <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_string(x$prefix, arg = "prefix")

  check_type(training[, col_names], types = "tokenlist")

  step_tokenmerge_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_tokenmerge <- function(object, new_data, ...) {
  if (length(object$column) == 0L) {
    # Empty selection
    return(new_data)
  }

  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  new_col <- as.list(
    unname(as.data.frame(new_data[, col_names, drop = FALSE]))
  ) %>%
    map(get_tokens) %>%
    pmap(c)
  new_col <- tibble(tokenlist(new_col))
  names(new_col) <- object$prefix

  new_data <- remove_original_cols(new_data, object, col_names)

  new_col <- recipes::check_name(new_col, new_data, object, names(new_col))

  new_data <- vec_cbind(new_data, new_col)

  new_data
}

#' @export
print.step_tokenmerge <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Merging tokens for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_tokenmerge
#' @usage NULL
#' @export
tidy.step_tokenmerge <- function(x, ...) {
  if (is_trained(x)) {
    term_names <- unname(x$columns)
    res <- tibble(terms = term_names)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_tokenmerge <- function(x, ...) {
  "textrecipes"
}
