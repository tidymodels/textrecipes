#' Filtering of Stop Words for Tokens Variables
#'
#' `step_stopwords` creates a *specification* of a recipe step that will filter
#' a [`token`][tokenlist()] variable for stop words.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param language A character to indicate the language of stop words by ISO
#'   639-1 coding scheme.
#' @param keep A logical. Specifies whether to keep the stop words or discard
#'   them.
#' @param stopword_source A character to indicate the stop words source as
#'   listed in `stopwords::stopwords_getsources`.
#' @param custom_stopword_source A character vector to indicate a custom list of
#'   words that cater to the users specific problem.
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' Stop words are words which sometimes are remove before natural language
#' processing tasks. While stop words usually refers to the most common words in
#' the language there is no universal stop word list.
#'
#' The argument `custom_stopword_source` allows you to pass a character vector
#' to filter against. With the `keep` argument one can specify to keep the words
#' instead of removing thus allowing you to select words with a combination of
#' these two arguments.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the selectors or variables selected), `value` (name of stop word list), and
#' `keep` (whether stop words are removed or kept).
#'
#' @seealso [step_tokenize()] to turn characters into [`tokens`][tokenlist()]
#' @family Steps for Token Modification
#'   
#' @examples
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' if (requireNamespace("stopwords", quietly = TRUE)) {
#'   tate_rec <- recipe(~., data = tate_text) %>%
#'     step_tokenize(medium) %>%
#'     step_stopwords(medium)
#'
#'   tate_obj <- tate_rec %>%
#'     prep()
#'
#'   bake(tate_obj, new_data = NULL, medium) %>%
#'     slice(1:2)
#'
#'   bake(tate_obj, new_data = NULL) %>%
#'     slice(2) %>%
#'     pull(medium)
#'
#'   tidy(tate_rec, number = 2)
#'   tidy(tate_obj, number = 2)
#' }
#'
#' # With a custom stop words list
#'
#' tate_rec <- recipe(~., data = tate_text) %>%
#'   step_tokenize(medium) %>%
#'   step_stopwords(medium, custom_stopword_source = c("twice", "upon"))
#' tate_obj <- tate_rec %>%
#'   prep(traimomg = tate_text)
#'
#' bake(tate_obj, new_data = NULL) %>%
#'   slice(2) %>%
#'   pull(medium)
#' @export
step_stopwords <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           language = "en",
           keep = FALSE,
           stopword_source = "snowball",
           custom_stopword_source = NULL,
           skip = FALSE,
           id = rand_id("stopwords")) {
    add_step(
      recipe,
      step_stopwords_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        language = language,
        keep = keep,
        stopword_source = stopword_source,
        custom_stopword_source = custom_stopword_source,
        skip = skip,
        id = id
      )
    )
  }

step_stopwords_new <-
  function(terms, role, trained, columns, language, keep,
           stopword_source, custom_stopword_source, skip, id) {
    step(
      subclass = "stopwords",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      language = language,
      keep = keep,
      stopword_source = stopword_source,
      custom_stopword_source = custom_stopword_source,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_stopwords <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_list(training[, col_names])

  step_stopwords_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    language = x$language,
    keep = x$keep,
    stopword_source = x$stopword_source,
    custom_stopword_source = x$custom_stopword_source,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_stopwords <- function(object, new_data, ...) {
  col_names <- object$columns

  stopword_list <- object$custom_stopword_source %||%
    stopwords::stopwords(
      language = object$language,
      source = object$stopword_source
    )

  for (i in seq_along(col_names)) {
    filtered_text <- tokenlist_filter(
      new_data[, col_names[i], drop = TRUE],
      stopword_list,
      object$keep
    )

    new_data[, col_names[i]] <- tibble(filtered_text)
  }
  new_data <- factor_to_text(new_data, col_names)

  as_tibble(new_data)
}

#' @export
print.step_stopwords <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Stop word removal for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_stopwords` object.
#' @export
tidy.step_stopwords <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = unname(x$columns),
      value = x$stopword_source,
      keep = x$keep
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_chr,
      keep = na_lgl
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_stopwords <- function(x, ...) {
  c("textrecipes", "stopwords")
}
