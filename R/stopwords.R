#' Filtering of stopwords from a [tokenlist] variable
#'
#' `step_stopwords` creates a *specification* of a recipe step that
#'  will filter a [tokenlist] for stopwords(keep or remove).
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_stopwords`, this indicates the variables to be encoded
#'  into a [tokenlist]. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param language A character to indicate the language of stopwords
#'  by ISO 639-1 coding scheme.
#' @param keep A logical. Specifies whether to keep the stopwords or discard
#'  them.
#' @param stopword_source A character to indicate the stopwords source as
#'  listed in `stopwords::stopwords_getsources`.
#' @param custom_stopword_source A character vector to indicate a custom
#'  list of words that cater to the users specific problem.
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
#' if (requireNamespace("stopwords", quietly = TRUE)) {
#'   okc_rec <- recipe(~., data = okc_text) %>%
#'     step_tokenize(essay0) %>%
#'     step_stopwords(essay0)
#'
#'   okc_obj <- okc_rec %>%
#'     prep()
#'
#'   bake(okc_obj, new_data = NULL, essay0) %>%
#'     slice(1:2)
#'
#'   bake(okc_obj, new_data = NULL) %>%
#'     slice(2) %>%
#'     pull(essay0)
#'
#'   tidy(okc_rec, number = 2)
#'   tidy(okc_obj, number = 2)
#' }
#'
#' # With a custom stopwords list
#'
#' okc_rec <- recipe(~., data = okc_text) %>%
#'   step_tokenize(essay0) %>%
#'   step_stopwords(essay0, custom_stopword_source = c("twice", "upon"))
#' okc_obj <- okc_rec %>%
#'   prep(traimomg = okc_text)
#'
#' bake(okc_obj, new_data = NULL) %>%
#'   slice(2) %>%
#'   pull(essay0)
#' @export
#' @details
#' Stop words are words which sometimes are remove before natural language
#' processing tasks. While stop words usually refers to the most common
#' words in the language there is no universal stop word list.
#'
#' The argument `custom_stopword_source` allows you to pass a character vector
#' to filter against. With the `keep` argument one can specify to keep the
#' words instead of removing thus allowing you to select words with a
#' combination of these two arguments.
#'
#' @seealso [step_tokenize()] to turn character into tokenlist.
#' @family tokenlist to tokenlist steps
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
        terms = ellipse_check(...),
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
  col_names <- terms_select(x$terms, info = info)

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
    cat("Stop word removal for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_stopwords
#' @param x A `step_stopwords` object.
#' @export
tidy.step_stopwords <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = x$terms,
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
