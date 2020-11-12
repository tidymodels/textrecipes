#' Untokenization of [tokenlist] variables
#'
#' `step_untokenize` creates a *specification* of a recipe step that
#'  will convert a [tokenlist] into a character predictor.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_untokenize`, this indicates the variables to be encoded
#'  into a [tokenlist]. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param sep a character to determine how the tokens should be separated
#'  when pasted together. Defaults to `" "`.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake.recipe()]? While all
#'  operations are baked when [recipes::prep.recipe()] is run, some
#'  operations may not be able to be conducted on new data (e.g.
#'  processing the outcome variable(s)). Care should be taken when
#'  using `skip = TRUE` as it may affect the computations for
#'  subsequent operations.
#' @param id A character string that is unique to this step to identify it.
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
#'   step_tokenize(essay0) %>%
#'   step_untokenize(essay0)
#'
#' okc_obj <- okc_rec %>%
#'   prep()
#'
#' bake(okc_obj, new_data = NULL, essay0) %>%
#'   slice(1:2)
#'
#' bake(okc_obj, new_data = NULL) %>%
#'   slice(2) %>%
#'   pull(essay0)
#'
#' tidy(okc_rec, number = 2)
#' tidy(okc_obj, number = 2)
#' @export
#' @details
#' This steps will turn a [tokenlist] back into a character vector. This step
#' is calling `paste` internally to put the tokens back together to a character.
#'
#' @seealso [step_tokenize()] to turn character into tokenlist.
#' @family tokenlist to character steps
step_untokenize <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           sep = " ",
           skip = FALSE,
           id = rand_id("untokenize")) {
    add_step(
      recipe,
      step_untokenize_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        sep = sep,
        skip = skip,
        id = id
      )
    )
  }

step_untokenize_new <-
  function(terms, role, trained, columns, sep, skip, id) {
    step(
      subclass = "untokenize",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      sep = sep,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_untokenize <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  check_list(training[, col_names])

  step_untokenize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    sep = x$sep,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_untokenize <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (i in seq_along(col_names)) {
    tokens <- get_tokens(new_data[, col_names[i], drop = TRUE])
    new_data[, col_names[i]] <- map_chr(
      .x = tokens,
      .f = paste,
      collapse = object$sep
    )
  }

  new_data <- factor_to_text(new_data, col_names)

  as_tibble(new_data)
}

#' @export
print.step_untokenize <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Untokenization for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_untokenize
#' @param x A `step_untokenize` object.
#' @export
tidy.step_untokenize <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = x$terms,
      value = x$sep
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_chr
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_untokenize <- function(x, ...) {
  c("textrecipes")
}
