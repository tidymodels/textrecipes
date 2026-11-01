#' Model2Vec Embedding Engine
#'
#' `embedding_model2vec()` creates an engine for [step_sentence_embeddings()]
#' that embeds text with a [Model2Vec](https://github.com/MinishLab/model2vec)
#' model. The model is a table of token vectors that is read from disk and used
#' in R, so it needs neither Python nor a server, and text never leaves your
#' machine.
#'
#' @param path A single string giving the path to a folder holding a Model2Vec
#'   model, which must contain the files `model.safetensors` and `vocab.txt`.
#'   [download_embeddings_model2vec()] downloads such a folder. The path is
#'   expanded and stored in the engine.
#' @param normalize A single logical. If `TRUE`, each embedding is scaled to
#'   have unit length, as the Model2Vec Python package does by default. Defaults
#'   to `TRUE`.
#' @param max_tokens A single positive whole number giving the maximum number of
#'   tokens of a text that are used. Later tokens are ignored. Defaults to
#'   `512`, as in the Model2Vec Python package.
#'
#' @details
#'
#' The embedding of a text is the mean of the vectors of its tokens, scaled to
#' unit length if `normalize = TRUE`. This is what Model2Vec does. The text is
#' tokenized with the WordPiece vocabulary of the model using
#' [wordpiece::wordpiece_tokenize()], tokens that are not in the vocabulary
#' (`"[UNK]"`) are ignored, and no special tokens are added. A text without any
#' known tokens gets a vector of zeros.
#'
#' This follows the way Model2Vec builds an embedding, but it is not guaranteed
#' to match the Python `model2vec` package exactly. Tokenizer details such as
#' the handling of accents or unusual characters may differ, so compare a few
#' texts with the Python package if you need identical values. Only models that
#' use a WordPiece vocabulary, such as the English potion models, are
#' supported. Models with a different tokenizer, with quantized weights, or with
#' extra weights for the tokens can't be used.
#'
#' The engine stores the path to the model folder, not the model. A prepped
#' recipe therefore needs the model files to be at the same path where it is
#' baked. The model is read the first time it is used and is then kept for the
#' rest of the R session. Each parallel worker reads its own copy.
#'
#' Requires the wordpiece, safetensors and torch packages. The torch package is
#' only used to read the file, and the model is not run with it.
#'
#' @return An object of class `sentence_embedding_engine`.
#'
#' @seealso [step_sentence_embeddings()], [download_embeddings_model2vec()], and
#'   `vignette("static-embeddings", package = "textrecipes")` for using the same
#'   model with [step_word_embeddings()].
#' @family Embedding Engines
#'
#' @examplesIf FALSE
#' # Downloads the model files
#' library(recipes)
#'
#' url <- paste0(
#'   "https://huggingface.co/minishlab/potion-base-8M/resolve/",
#'   "bf8b056651a2c21b8d2565580b8569da283cab23"
#' )
#' model_dir <- download_embeddings_model2vec(url)
#'
#' sample_data <- tibble(
#'   text = c("The cat sat.", "A dog ran.", "Stocks fell sharply today.")
#' )
#'
#' rec <- recipe(~text, data = sample_data) |>
#'   step_sentence_embeddings(text, engine = embedding_model2vec(model_dir))
#'
#' rec |>
#'   prep() |>
#'   bake(new_data = NULL)
#' @export
embedding_model2vec <- function(path, normalize = TRUE, max_tokens = 512) {
  check_string(path, arg = "path")
  check_bool(normalize, arg = "normalize")
  check_number_whole(max_tokens, min = 1, arg = "max_tokens")

  if (!nzchar(path)) {
    cli::cli_abort("{.arg path} must not be an empty string.")
  }

  new_embedding_engine(
    "embedding_model2vec",
    path = normalizePath(path, mustWork = FALSE),
    normalize = normalize,
    max_tokens = as.integer(max_tokens)
  )
}

#' @export
engine_label.embedding_model2vec <- function(engine) {
  "model2vec"
}

#' @export
engine_model.embedding_model2vec <- function(engine) {
  basename(engine$path)
}

# The model is a folder of files, with no revision to pin.
#' @export
engine_revision.embedding_model2vec <- function(engine) {
  NA_character_
}

#' @export
engine_required_pkgs.embedding_model2vec <- function(engine) {
  c("wordpiece", "safetensors", "torch")
}

# The files identify the model, so the key changes if they are replaced.
#' @export
engine_cache_key.embedding_model2vec <- function(engine) {
  rlang::hash(list(
    "model2vec",
    model2vec_files_id(engine$path),
    engine$normalize,
    engine$max_tokens
  ))
}

#' @export
engine_check.embedding_model2vec <- function(engine) {
  rlang::check_installed(
    c("wordpiece", "safetensors", "torch"),
    reason = "to use `embedding_model2vec()`."
  )

  # Reading is the check that the folder holds a usable model. The model is
  # cached, so `engine_embed()` doesn't read it again.
  model2vec_model(engine)

  invisible(engine)
}

#' @export
engine_embed.embedding_model2vec <- function(engine, x) {
  if (anyNA(x)) {
    cli::cli_abort("{.arg x} can't contain missing values.")
  }

  if (length(x) == 0L) {
    return(matrix(NA_real_, nrow = 0L, ncol = 0L))
  }

  model <- model2vec_model(engine)

  tokens <- wordpiece::wordpiece_tokenize(text = x, vocab = model$tokens)

  res <- matrix(0, nrow = length(x), ncol = ncol(model$vectors))
  for (i in seq_along(x)) {
    # Ids are 0-indexed. Unknown tokens are dropped, as Model2Vec does.
    ids <- tokens[[i]][names(tokens[[i]]) != "[UNK]"]
    ids <- utils::head(ids, engine$max_tokens)
    if (length(ids) > 0L) {
      res[i, ] <- colMeans(model$vectors[ids + 1L, , drop = FALSE])
    }
  }

  if (engine$normalize) {
    norms <- sqrt(rowSums(res^2))
    # Rows of zeros stay zero.
    res <- res / pmax(norms, 1e-32)
  }

  res
}

# Model files --------------------------------------------------------------------

# Session-level cache of the models that have been read. It lives outside the
# step so that a prepped recipe only holds the path, and is never serialized.
model2vec_model_cache <- new.env(parent = emptyenv())

clear_model2vec_model_cache <- function() {
  rm(
    list = ls(model2vec_model_cache, all.names = TRUE),
    envir = model2vec_model_cache
  )
  invisible()
}

# Cheap identity of the files, so that replacing them gives a new cache entry.
model2vec_files_id <- function(path) {
  files <- file.path(path, c("model.safetensors", "vocab.txt"))
  info <- file.info(files)
  list(path, info$size, as.numeric(info$mtime))
}

model2vec_model <- function(engine) {
  key <- rlang::hash(model2vec_files_id(engine$path))

  if (!exists(key, envir = model2vec_model_cache, inherits = FALSE)) {
    assign(
      key,
      read_model2vec_files(engine$path),
      envir = model2vec_model_cache
    )
  }

  get(key, envir = model2vec_model_cache, inherits = FALSE)
}
