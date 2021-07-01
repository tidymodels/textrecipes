#'  Generate the basic set of text features
#'
#' `step_tokenmerge` creates a *specification* of a recipe step that
#'  will take multiple [tokenlist]s and combine them into one
#'  [tokenlist].
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param prefix A prefix for generated column names, default to "tokenmerge".
#' @template args-skip
#' @template args-id
#' 
#' @template returns
#' 
#' @seealso [step_tokenize()] to turn character into tokenlist.
#' @family tokenlist to tokenlist steps
#' 
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
#' 
#' @export
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
