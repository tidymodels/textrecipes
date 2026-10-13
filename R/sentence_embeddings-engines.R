#' Embedding Engines for `step_sentence_embeddings()`
#'
#' These functions create the `engine` object passed to
#' [step_sentence_embeddings()]. An engine describes how character vectors are
#' turned into numeric embeddings. Engines only hold a reference to the model
#' (a function, or later a model name and server settings), never the model
#' itself, so a prepped recipe stays small.
#'
#' @param fun A function that takes a character vector and returns a numeric
#'   matrix with one row per element and one column per embedding dimension. A
#'   formula such as `~ my_embedder(.x)` is converted to a function with
#'   [rlang::as_function()].
#'
#' @details
#'
#' `embedding_custom()` is the escape hatch for any backend that doesn't have a
#' dedicated engine. The function is called with the unique, non-missing
#' strings that haven't been embedded yet in the current R session, and must
#' return a numeric matrix with exactly one row per string.
#'
#' The function is stored in the recipe. If it captures a live external object,
#' such as a Python model through reticulate, that object will not survive
#' [saveRDS()]. Have the function load its model lazily, from a path or an
#' identifier, instead of capturing it.
#'
#' @return An object of class `sentence_embedding_engine`.
#'
#' @seealso [step_sentence_embeddings()]
#' @family Embedding Engines
#'
#' @examples
#' # A toy embedder that returns the length and number of vowels of each string
#' toy_embedder <- function(x) {
#'   cbind(
#'     nchar = nchar(x),
#'     vowels = nchar(gsub("[^aeiou]", "", x))
#'   )
#' }
#'
#' embedding_custom(toy_embedder)
#' @export
embedding_custom <- function(fun) {
  if (is_formula(fun)) {
    fun <- as_function(fun)
  }
  check_function(fun, arg = "fun")

  new_embedding_engine(
    "embedding_custom",
    fun = fun,
    # A closure can capture arbitrary state, so identical-looking functions are
    # not interchangeable. Each engine object gets its own cache identity.
    cache_key = rand_id("custom")
  )
}

new_embedding_engine <- function(subclass, ...) {
  structure(
    list(...),
    class = c(subclass, "sentence_embedding_engine")
  )
}

#' @export
print.sentence_embedding_engine <- function(x, ...) {
  cat("<", engine_label(x), ">\n", sep = "")
  invisible(x)
}

# Engine generics --------------------------------------------------------------
# One method per engine. Adding an engine means adding methods here, nothing
# else in the step needs to change.

# Human readable name, used in printing and error messages.
engine_label <- function(engine) {
  UseMethod("engine_label")
}

# Name of the model, or NA if the engine doesn't have one.
engine_model <- function(engine) {
  UseMethod("engine_model")
}

# Packages needed to run the engine, in addition to textrecipes.
engine_required_pkgs <- function(engine) {
  UseMethod("engine_required_pkgs")
}

# String identifying everything that affects the embeddings an engine returns.
# Used as part of the session cache key.
engine_cache_key <- function(engine) {
  UseMethod("engine_cache_key")
}

# Run once before anything is embedded. Should abort with an informative error
# if the engine can't run, for example a missing package or an unreachable
# server.
engine_check <- function(engine) {
  UseMethod("engine_check")
}

# Embed a character vector of unique, non-missing strings. Returns a numeric
# matrix with one row per string.
engine_embed <- function(engine, x) {
  UseMethod("engine_embed")
}

#' @export
engine_label.embedding_custom <- function(engine) {
  "custom function"
}

#' @export
engine_model.embedding_custom <- function(engine) {
  NA_character_
}

#' @export
engine_required_pkgs.embedding_custom <- function(engine) {
  character()
}

#' @export
engine_cache_key.embedding_custom <- function(engine) {
  engine$cache_key
}

#' @export
engine_check.embedding_custom <- function(engine) {
  invisible(engine)
}

#' @export
engine_embed.embedding_custom <- function(engine, x) {
  engine$fun(x)
}
