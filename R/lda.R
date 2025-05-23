#' Calculate LDA Dimension Estimates of Tokens
#'
#' `step_lda()` creates a *specification* of a recipe step that will return the
#' lda dimension estimates of a text variable.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param lda_models A WarpLDA model object from the text2vec package. If left
#'   to NULL, the default, it will train its model based on the training data.
#'   Look at the examples for how to fit a WarpLDA model.
#' @param num_topics integer desired number of latent topics.
#' @param prefix A prefix for generated column names, defaults to "lda".
#' @template args-keep_original_cols
#' @template args-skip
#' @template args-id
#'
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `num_topics`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{num_topics}{integer, number of topics}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @source \url{https://arxiv.org/abs/1301.3781}
#'
#' @template returns
#'
#' @family Steps for Numeric Variables From Tokens
#'
#' @examplesIf all(c("modeldata", "text2vec", "data.table") %in% rownames(installed.packages()))
#' \dontshow{library(data.table)}
#' \dontshow{data.table::setDTthreads(2)}
#' \dontshow{Sys.setenv("OMP_THREAD_LIMIT" = 2)}
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~., data = tate_text) |>
#'   step_tokenize(medium) |>
#'   step_lda(medium)
#'
#' tate_obj <- tate_rec |>
#'   prep()
#'
#' bake(tate_obj, new_data = NULL) |>
#'   slice(1:2)
#' tidy(tate_rec, number = 2)
#' tidy(tate_obj, number = 2)
#'
#' # Changing the number of topics.
#' recipe(~., data = tate_text) |>
#'   step_tokenize(medium, artist) |>
#'   step_lda(medium, artist, num_topics = 20) |>
#'   prep() |>
#'   bake(new_data = NULL) |>
#'   slice(1:2)
#'
#' # Supplying A pre-trained LDA model trained using text2vec
#' library(text2vec)
#' tokens <- word_tokenizer(tolower(tate_text$medium))
#' it <- itoken(tokens, ids = seq_along(tate_text$medium))
#' v <- create_vocabulary(it)
#' dtm <- create_dtm(it, vocab_vectorizer(v))
#' lda_model <- LDA$new(n_topics = 15)
#'
#' recipe(~., data = tate_text) |>
#'   step_tokenize(medium, artist) |>
#'   step_lda(medium, artist, lda_models = lda_model) |>
#'   prep() |>
#'   bake(new_data = NULL) |>
#'   slice(1:2)
#' @export
step_lda <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    columns = NULL,
    lda_models = NULL,
    num_topics = 10L,
    prefix = "lda",
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("lda")
  ) {
    recipes::recipes_pkg_check(required_pkgs.step_lda())

    add_step(
      recipe,
      step_lda_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        lda_models = lda_models,
        num_topics = num_topics,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_lda_new <-
  function(
    terms,
    role,
    trained,
    columns,
    lda_models,
    num_topics,
    prefix,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "lda",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      lda_models = lda_models,
      num_topics = num_topics,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_lda <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_number_whole(x$num_topics, min = 0, arg = "num_topics")
  check_string(x$prefix, arg = "prefix")

  check_lda_character(training[, col_names])

  check_type(training[, col_names], types = "tokenlist")

  model_list <- list()

  for (col_name in col_names) {
    tokens <- get_tokens(training[[col_name]])

    ddd <- utils::capture.output(
      model_list[[col_name]] <- x$lda_models %||%
        attr(word_dims(tokens, n = x$num_topics), "dict")
    )
  }

  step_lda_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    lda_models = model_list,
    num_topics = x$num_topics,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_lda <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  if (is.null(names(object$lda_models))) {
    # Backwards compatibility with 1.0.3 (#230)
    names(object$lda_models) <- col_names
  }

  for (col_name in col_names) {
    tokens <- get_tokens(new_data[[col_name]])

    ddd <- utils::capture.output(
      tf_text <- word_dims_newtext(object$lda_models[[col_name]], tokens)
    )

    attr(tf_text, "dict") <- NULL
    colnames(tf_text) <- paste(
      object$prefix,
      col_name,
      colnames(tf_text),
      sep = "_"
    )

    tf_text <- recipes::check_name(tf_text, new_data, object, names(tf_text))

    new_data <- vec_cbind(new_data, tf_text)
  }

  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

#' @export
print.step_lda <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Text feature extraction for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_lda
#' @usage NULL
#' @export
tidy.step_lda <- function(x, ...) {
  if (is_trained(x)) {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      num_topics = x$num_topics
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      num_topics = x$num_topics
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_lda <- function(x, ...) {
  "textrecipes"
}

word_dims <- function(tokens, n = 10, n_iter = 20) {
  it <- text2vec::itoken(tokens, ids = seq_along(tokens))
  v <- text2vec::create_vocabulary(it)
  v <- text2vec::prune_vocabulary(
    v,
    term_count_min = 2,
    vocab_term_max = n * 50
  )
  dtm <- text2vec::create_dtm(it, text2vec::vocab_vectorizer(v))
  lda_model <- text2vec::LDA$new(n_topics = n)
  d <- lda_model$fit_transform(dtm, n_iter = n_iter)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  names(d) <- seq_len(ncol(d))
  row.names(d) <- NULL
  attr(d, "dict") <- lda_model
  d
}

word_dims_newtext <- function(lda_model, tokens, n_iter = 20) {
  it <- text2vec::itoken(tokens, ids = seq_along(tokens))
  v <- text2vec::create_vocabulary(it)
  v <- text2vec::prune_vocabulary(
    v,
    term_count_min = 5,
    doc_proportion_max = 0.2
  )
  dtm <- text2vec::create_dtm(it, text2vec::vocab_vectorizer(v))
  d <- lda_model$fit_transform(dtm, n_iter = n_iter)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  names(d) <- seq_len(ncol(d))
  row.names(d) <- NULL
  d
}

check_lda_character <- function(dat) {
  character_ind <- vapply(dat, is.character, logical(1))
  factor_ind <- vapply(dat, is.factor, logical(1))

  all_good <- character_ind | factor_ind

  if (any(all_good)) {
    cli::cli_abort(
      c(
        "All columns selected for this step should be tokenlists.",
        "i" = "See {.url https://github.com/tidymodels/textrecipes#breaking-changes}
           for more information."
      )
    )
  }

  invisible(all_good)
}
