#' Part of Speech Filtering of Token Variables
#'
#' `step_pos_filter()` creates a *specification* of a recipe step that will
#' filter a [`token`][tokenlist()] variable based on part of speech tags.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param keep_tags Character variable of part of speech tags to keep. See
#'   details for complete list of tags. Defaults to "NOUN".
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' Possible part of speech tags for `spacyr` engine are: "ADJ", "ADP", "ADV",
#' "AUX", "CONJ", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON",
#' "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X" and "SPACE". For more
#' information look here
#' \url{https://github.com/explosion/spaCy/blob/master/spacy/glossary.py}.
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
#' @examples
#' \dontrun{
#' library(recipes)
#'
#' short_data <- data.frame(text = c(
#'   "This is a short tale,",
#'   "With many cats and ladies."
#' ))
#'
#' rec_spec <- recipe(~text, data = short_data) |>
#'   step_tokenize(text, engine = "spacyr") |>
#'   step_pos_filter(text, keep_tags = "NOUN") |>
#'   step_tf(text)
#'
#' rec_prepped <- prep(rec_spec)
#'
#' bake(rec_prepped, new_data = NULL)
#' }
#'
#' @export
step_pos_filter <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    columns = NULL,
    keep_tags = "NOUN",
    skip = FALSE,
    id = rand_id("pos_filter")
  ) {
    add_step(
      recipe,
      step_pos_filter_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        keep_tags = keep_tags,
        skip = skip,
        id = id
      )
    )
  }

step_pos_filter_new <-
  function(terms, role, trained, columns, keep_tags, skip, id) {
    step(
      subclass = "pos_filter",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      keep_tags = keep_tags,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_pos_filter <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_character(x$keep_tags, arg = "keep_tags")

  check_type(training[, col_names], types = "tokenlist")

  step_pos_filter_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    keep_tags = x$keep_tags,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_pos_filter <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    variable <- new_data[[col_name]]

    if (is.null(maybe_get_pos(variable))) {
      cli::cli_abort(
        c(
          "{.arg {col_name}} doesn't have a pos attribute.",
          "i" = "Make sure the tokenization step includes part of speech tagging."
        )
      )
    }

    new_data[[col_name]] <- tokenlist_pos_filter(variable, object$keep_tags)
  }
  new_data <- factor_to_text(new_data, col_names)
  new_data
}

#' @export
print.step_pos_filter <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Part of speech filtering for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_pos_filter
#' @usage NULL
#' @export
tidy.step_pos_filter <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$columns))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_pos_filter <- function(x, ...) {
  c("textrecipes")
}
