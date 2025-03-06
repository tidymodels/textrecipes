#' Pretrained Word Embeddings of Tokens
#'
#' `step_word_embeddings()` creates a *specification* of a recipe step that will
#' convert a [`token`][tokenlist()] variable into word-embedding dimensions by
#' aggregating the vectors of each token from a pre-trained embedding.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param embeddings A tibble of pre-trained word embeddings, such as those
#'   returned by the embedding_glove function from the textdata package. The
#'   first column should contain tokens, and additional columns should contain
#'   embeddings vectors.
#' @param aggregation A character giving the name of the aggregation function to
#'   use. Must be one of "sum", "mean", "min", and "max". Defaults to "sum".
#' @param aggregation_default A numeric denoting the default value for case with
#'   no words are matched in embedding. Defaults to 0.
#' @template args-prefix
#' @template args-keep_original_cols
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' Word embeddings map words (or other tokens) into a high-dimensional feature
#' space. This function maps pre-trained word embeddings onto the tokens in your
#' data.
#'
#' The argument `embeddings` provides the pre-trained vectors. Each dimension
#' present in this tibble becomes a new feature column, with each column
#' aggregated across each row of your text using the function supplied in the
#' `aggregation` argument.
#'
#' The new components will have names that begin with `prefix`, then the name of
#' the aggregation function, then the name of the variable from the embeddings
#' tibble (usually something like "d7"). For example, using the default
#' "wordembedding" prefix, and the GloVe embeddings from the textdata package
#' (where the column names are `d1`, `d2`, etc), new columns would be
#' `wordembedding_d1`, `wordembedding_d1`, etc.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `embedding_rows`, `aggregation`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{embedding_rows}{integer, number of rows in embedding}
#'   \item{aggregation}{character,aggregation}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @seealso [step_tokenize()] to turn characters into [`tokens`][tokenlist()]
#' @family Steps for Numeric Variables From Tokens
#'
#' @examples
#' library(recipes)
#'
#' embeddings <- tibble(
#'   tokens = c("the", "cat", "ran"),
#'   d1 = c(1, 0, 0),
#'   d2 = c(0, 1, 0),
#'   d3 = c(0, 0, 1)
#' )
#'
#' sample_data <- tibble(
#'   text = c(
#'     "The.",
#'     "The cat.",
#'     "The cat ran."
#'   ),
#'   text_label = c("fragment", "fragment", "sentence")
#' )
#'
#' rec <- recipe(text_label ~ ., data = sample_data) %>%
#'   step_tokenize(text) %>%
#'   step_word_embeddings(text, embeddings = embeddings)
#'
#' obj <- rec %>%
#'   prep()
#'
#' bake(obj, sample_data)
#'
#' tidy(rec, number = 2)
#' tidy(obj, number = 2)
#' @export
step_word_embeddings <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  embeddings,
  aggregation = c("sum", "mean", "min", "max"),
  aggregation_default = 0,
  prefix = "wordembed",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("word_embeddings")
) {
  # Validate the special inputs here to make sure nothing goes haywire further
  # downstream.
  if (
    !tibble::is_tibble(embeddings) ||
      !any(inherits(embeddings[[1]], c("character", "factor"), which = TRUE)) ||
      ncol(embeddings) == 1 ||
      !all(map_lgl(embeddings[, 2:ncol(embeddings)], is.numeric))
  ) {
    cli::cli_abort(
      "embeddings should be a tibble with {.code 1} character or factor column 
      and additional numeric columns.",
      class = "bad_embeddings"
    )
  }

  aggregation <- rlang::arg_match(aggregation)

  add_step(
    recipe,
    step_word_embeddings_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      columns = columns,
      embeddings = embeddings,
      aggregation = aggregation,
      aggregation_default = aggregation_default,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_word_embeddings_new <- function(
  terms,
  role,
  trained,
  columns,
  embeddings,
  aggregation,
  aggregation_default,
  prefix,
  keep_original_cols,
  skip,
  id
) {
  recipes::step(
    subclass = "word_embeddings",
    terms = terms,
    role = role,
    trained = trained,
    columns = columns,
    embeddings = embeddings,
    aggregation = aggregation,
    aggregation_default = aggregation_default,
    prefix = prefix,
    keep_original_cols = keep_original_cols,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_word_embeddings <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_number_decimal(x$aggregation_default, arg = "aggregation_default")
  check_string(x$prefix, arg = "prefix")

  check_type(training[, col_names], types = "tokenlist")

  step_word_embeddings_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    embeddings = x$embeddings,
    aggregation = x$aggregation,
    aggregation_default = x$aggregation_default,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_word_embeddings <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  aggregation_fun <- get_aggregation_fun(object)

  for (col_name in col_names) {
    emb_columns <- tokenlist_embedding(
      new_data[[col_name]],
      object$embeddings,
      aggregation_fun
    )

    colnames(emb_columns) <- paste(
      object$prefix,
      col_name,
      colnames(emb_columns),
      sep = "_"
    )

    emb_columns <- recipes::check_name(
      emb_columns,
      new_data,
      object,
      names(emb_columns)
    )

    new_data <- vec_cbind(new_data, emb_columns)
  }

  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

get_aggregation_fun <- function(object) {
  fun <- switch(
    EXPR = object$aggregation,
    sum = sum,
    mean = mean,
    min = min,
    max = max
  )

  function(x, ...) {
    if (length(x) == 0) {
      return(object$aggregation_default)
    }
    fun(x, ...)
  }
}

#' @export
print.step_word_embeddings <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Word embeddings aggregated from "
  print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @rdname step_word_embeddings
#' @usage NULL
#' @export
tidy.step_word_embeddings <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = unname(x$columns %||% character()),
      embeddings_rows = nrow(x$embeddings),
      aggregation = x$aggregation
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      embeddings_rows = nrow(x$embeddings),
      aggregation = x$aggregation
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_word_embeddings <- function(x, ...) {
  "textrecipes"
}
