#' Sentence Embeddings of Text
#'
#' `step_sentence_embeddings()` creates a *specification* of a recipe step that
#' will convert a character variable into dense embedding dimensions by running
#' each string through an embedding model.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param engine An engine object created by one of the embedding engine
#'   functions, such as [embedding_custom()]. The engine defines how strings are
#'   turned into vectors and holds the settings needed to do so.
#' @param prefix A single string used as the prefix of the new columns. See
#'   Details for how the column names are built. Defaults to `"sentembed"`.
#' @template args-keep_original_cols
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' Unlike [step_word_embeddings()], which looks up and aggregates the vectors
#' of tokens, this step takes a character (or factor) column directly and
#' returns one dense
#' vector per row, as produced by the embedding model. It doesn't use
#' [step_tokenize()].
#'
#' The new columns have names that begin with `prefix`, then the name of the
#' variable being embedded, then the position of the dimension. For example,
#' using the default prefix and a variable named `text`, the new columns are
#' `sentembed_text_1`, `sentembed_text_2`, etc.
#'
#' # What is stored in the recipe
#'
#' Embedding models are large, so the step never stores the model. It stores the
#' engine, which is a reference to the model, together with the number of
#' dimensions seen when the recipe was prepped. This keeps prepped recipes
#' small, but it also means a prepped recipe can only be baked where the engine
#' can run. A recipe that is moved to another machine needs whatever the engine
#' depends on to be available there.
#'
#' If the engine returns a different number of dimensions when baking than it
#' did when prepping, `bake()` errors instead of returning columns the
#' downstream model wasn't trained on.
#'
#' # Missing values and caching
#'
#' Missing values stay missing: a row with `NA` text gets `NA` in all of its
#' embedding columns, and is never sent to the engine. Empty strings are sent to
#' the engine as they are.
#'
#' Embedding is usually the slowest part of a recipe, and `bake()` embeds new
#' data every time it is called. Within an R session, embeddings are cached per
#' engine and string, so repeated strings, and the training data that is
#' embedded both by `prep()` and by `bake()`, are only embedded once. The cache
#' lives outside the recipe and is cleared when the R session ends.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned
#' with columns `terms`, `engine`, `model`, `dimension`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{engine}{character, the name of the engine}
#'   \item{model}{character, the name of the model, `NA` if the engine doesn't
#'     have one}
#'   \item{dimension}{integer, the number of embedding dimensions, `NA` until
#'     the recipe is prepped}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @seealso [embedding_custom()] for the available engines, and
#'   [step_word_embeddings()] for embeddings that are aggregated from tokens.
#' @family Steps for Numeric Variables From Characters
#'
#' @examples
#' library(recipes)
#'
#' # A toy embedder, replace it with a call to a real embedding model
#' toy_embedder <- function(x) {
#'   cbind(
#'     nchar = nchar(x),
#'     vowels = nchar(gsub("[^aeiou]", "", x))
#'   )
#' }
#'
#' sample_data <- tibble(
#'   text = c("The cat.", "The cat ran.", "A longer sentence here."),
#'   text_label = c("short", "short", "long")
#' )
#'
#' rec <- recipe(text_label ~ ., data = sample_data) |>
#'   step_sentence_embeddings(text, engine = embedding_custom(toy_embedder))
#'
#' obj <- rec |>
#'   prep()
#'
#' bake(obj, new_data = NULL)
#'
#' tidy(rec, number = 1)
#' tidy(obj, number = 1)
#' @export
step_sentence_embeddings <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  engine,
  prefix = "sentembed",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("sentence_embeddings")
) {
  if (missing(engine) || !inherits(engine, "sentence_embedding_engine")) {
    cli::cli_abort(
      c(
        "{.arg engine} must be an embedding engine.",
        "i" = "Create one with a function such as {.fn embedding_custom}."
      ),
      class = "bad_embedding_engine"
    )
  }

  add_step(
    recipe,
    step_sentence_embeddings_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      columns = columns,
      engine = engine,
      dimension = NA_integer_,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_sentence_embeddings_new <- function(
  terms,
  role,
  trained,
  columns,
  engine,
  dimension,
  prefix,
  keep_original_cols,
  skip,
  id
) {
  step(
    subclass = "sentence_embeddings",
    terms = terms,
    role = role,
    trained = trained,
    columns = columns,
    engine = engine,
    dimension = dimension,
    prefix = prefix,
    keep_original_cols = keep_original_cols,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_sentence_embeddings <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_string(x$prefix, arg = "prefix")

  check_type(training[, col_names], types = c("string", "factor", "ordered"))

  dimension <- NA_integer_

  # The dimension is learned from the embeddings of the training data. The
  # training data has to be embedded here anyway, since recipes bake each step
  # on the training data to prep the next one.
  if (length(col_names) > 0) {
    dimensions <- integer()
    for (col_name in col_names) {
      embeddings <- embed_column(
        x$engine,
        training[[col_name]],
        col_name = col_name
      )
      dimensions <- c(dimensions, ncol(embeddings))
    }

    if (all(dimensions == 0L)) {
      cli::cli_abort(
        c(
          "Can't determine the embedding dimension.",
          "i" = "All of the selected columns only contain missing values."
        ),
        class = "no_embedding_text"
      )
    }

    dimension <- as.integer(max(dimensions))
  }

  step_sentence_embeddings_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    engine = x$engine,
    dimension = dimension,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_sentence_embeddings <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    emb_columns <- embed_column(
      object$engine,
      new_data[[col_name]],
      dimension = object$dimension,
      col_name = col_name
    )

    colnames(emb_columns) <- paste(
      object$prefix,
      col_name,
      seq_len(ncol(emb_columns)),
      sep = "_"
    )

    emb_columns <- as_tibble(emb_columns)

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

#' @export
print.step_sentence_embeddings <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("Sentence embeddings (", engine_label(x$engine), ") for ")
  print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @rdname step_sentence_embeddings
#' @usage NULL
#' @export
tidy.step_sentence_embeddings <- function(x, ...) {
  if (is_trained(x)) {
    terms <- unname(x$columns %||% character())
  } else {
    terms <- sel2char(x$terms)
  }

  res <- tibble(
    terms = terms,
    engine = rep(engine_label(x$engine), length(terms)),
    model = rep(engine_model(x$engine), length(terms)),
    dimension = rep(as.integer(x$dimension), length(terms))
  )
  res$id <- rep(x$id, length(terms))
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_sentence_embeddings <- function(x, ...) {
  unique(c("textrecipes", engine_required_pkgs(x$engine)))
}

# Embedding --------------------------------------------------------------------

# Session-level cache of embeddings. It lives outside the step because steps are
# immutable once prepped, and it is never serialized with a recipe.
sentence_embedding_cache <- new.env(parent = emptyenv())

clear_sentence_embedding_cache <- function() {
  rm(
    list = ls(sentence_embedding_cache, all.names = TRUE),
    envir = sentence_embedding_cache
  )
  invisible()
}

# Returns a numeric matrix with one row per element of `x` and one column per
# embedding dimension. Missing values give rows of `NA`. If there is nothing to
# embed the matrix has `dimension` columns, or 0 if it isn't known.
embed_column <- function(
  engine,
  x,
  dimension = NULL,
  col_name = "x",
  call = caller_env()
) {
  check_embedding_input(x, col_name, call = call)

  # recipes turns strings into factors by default, the embeddings are of the
  # labels.
  if (is.factor(x)) {
    x <- as.character(x)
  }

  present <- !is.na(x)
  texts <- unique(x[present])

  if (length(texts) == 0L) {
    return(matrix(NA_real_, nrow = length(x), ncol = dimension %||% 0L))
  }

  keys <- paste(
    engine_cache_key(engine),
    map_chr(texts, rlang::hash),
    sep = "_"
  )

  missing_keys <- !map_lgl(
    keys,
    exists,
    envir = sentence_embedding_cache,
    inherits = FALSE
  )

  if (any(missing_keys)) {
    check_embedding_backend(engine, call = call)

    new_texts <- texts[missing_keys]

    res <- tryCatch(
      engine_embed(engine, new_texts),
      error = function(cnd) {
        cli::cli_abort(
          "Embedding failed in the {.field {engine_label(engine)}} engine.",
          parent = cnd,
          call = call
        )
      }
    )
    check_embedding_result(res, length(new_texts), engine, call = call)
    storage.mode(res) <- "double"

    new_keys <- keys[missing_keys]
    for (i in seq_along(new_texts)) {
      assign(new_keys[[i]], unname(res[i, ]), envir = sentence_embedding_cache)
    }
  }

  unique_embeddings <- do.call(
    rbind,
    unname(mget(keys, envir = sentence_embedding_cache))
  )

  if (!is.null(dimension)) {
    check_embedding_dimension(unique_embeddings, dimension, engine, call = call)
  }

  out <- matrix(NA_real_, nrow = length(x), ncol = ncol(unique_embeddings))
  out[present, ] <- unique_embeddings[match(x[present], texts), , drop = FALSE]
  out
}
