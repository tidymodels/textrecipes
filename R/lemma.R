#' Lemmatization of Token Variables
#'
#' `step_lemma()` creates a *specification* of a recipe step that will extract
#' the lemmatization of a [`token`][tokenlist()] variable.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' This stem doesn't perform lemmatization by itself, but rather lets you
#' extract the lemma attribute of the [`token`][tokenlist()] variable. To be
#' able to use `step_lemma` you need to use a tokenization method that includes
#' lemmatization. Currently using the `"spacyr"` engine in [step_tokenize()]
#' provides lemmatization and works well with `step_lemma`.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the selectors or variables selected).
#'
#' @template case-weights-not-supported
#'
#' @seealso [step_tokenize()] to turn characters into [`tokens`][tokenlist()]
#' @family Steps for Token Modification
#'
#' @examples
#' \dontrun{
#' library(recipes)
#'
#' short_data <- data.frame(text = c(
#'   "This is a short tale,",
#'   "With many cats and ladies."
#' ))
#'
#' rec_spec <- recipe(~text, data = short_data) %>%
#'   step_tokenize(text, engine = "spacyr") %>%
#'   step_lemma(text) %>%
#'   step_tf(text)
#'
#' rec_prepped <- prep(rec_spec)
#'
#' bake(rec_prepped, new_data = NULL)
#' }
#'
#' @export
step_lemma <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           skip = FALSE,
           id = rand_id("lemma")) {
    add_step(
      recipe,
      step_lemma_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_lemma_new <-
  function(terms, role, trained, columns, skip, id) {
    step(
      subclass = "lemma",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_lemma <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names], types = "tokenlist")

  step_lemma_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_lemma <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    variable <- new_data[[col_name]]

    if (is.null(maybe_get_lemma(variable))) {
      rlang::abort(
        glue(
          "`{col_name}` doesn't have a lemma attribute. ",
          "Make sure the tokenization step includes lemmatization."
        )
      )
    } else {
      lemma_variable <- tokenlist_lemma(variable)
    }

    new_data[[col_name]] <- lemma_variable
  }
  new_data <- factor_to_text(new_data, col_names)
  new_data
}

#' @export
print.step_lemma <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Lemmatization for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_lemma` object.
#' @export
tidy.step_lemma <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$columns))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_lemma <- function(x, ...) {
  c("textrecipes")
}
