#' Untokenization of Token Variables
#'
#' `step_untokenize` creates a *specification* of a recipe step that will
#' convert a [`token`][tokenlist()] variable into a character predictor.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param sep a character to determine how the tokens should be separated when
#'   pasted together. Defaults to `" "`.
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' This steps will turn a [`token`][tokenlist()] vector back into a character
#' vector. This step is calling `paste` internally to put the tokens back
#' together to a character.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the selectors or variables selected) and `value` (seperator used for
#' collapsing).
#'
#' @template case-weights-not-supported
#'
#' @seealso [step_tokenize()] to turn characters into [`tokens`][tokenlist()]
#' @family Steps for Un-Tokenization
#'
#' @examples
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~., data = tate_text) %>%
#'   step_tokenize(medium) %>%
#'   step_untokenize(medium)
#'
#' tate_obj <- tate_rec %>%
#'   prep()
#'
#' bake(tate_obj, new_data = NULL, medium) %>%
#'   slice(1:2)
#'
#' bake(tate_obj, new_data = NULL) %>%
#'   slice(2) %>%
#'   pull(medium)
#'
#' tidy(tate_rec, number = 2)
#' tidy(tate_obj, number = 2)
#' @export
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
        terms = enquos(...),
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
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names], types = "tokenlist")

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
  check_new_data(col_names, object, new_data)

  for (i in seq_along(col_names)) {
    tokens <- get_tokens(new_data[, col_names[i], drop = TRUE])
    new_data[, col_names[i]] <- map_chr(
      .x = tokens,
      .f = paste,
      collapse = object$sep
    )
  }

  new_data <- factor_to_text(new_data, col_names)

  new_data
}

#' @export
print.step_untokenize <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Untokenization for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_untokenize` object.
#' @export
tidy.step_untokenize <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = unname(x$columns),
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
