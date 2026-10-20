#' Read Pretrained Static Embeddings From Files
#'
#' These functions read downloaded static embeddings into the tibble that the
#' `embeddings` argument of [step_word_embeddings()] expects: the first column
#' holds the tokens and the other columns hold the vectors. They read files
#' that are already on disk and do not download anything. Use
#' [download_embeddings_model2vec()] to download a Model2Vec model first.
#'
#' @param path For `read_embeddings_model2vec()`, a string giving the path to a
#'   folder holding a Model2Vec model, which must contain the files
#'   `model.safetensors` and `vocab.txt`. For `read_embeddings_bpemb()`, a string
#'   giving the path to an unpacked BPEmb vectors file, which ends in
#'   `.w2v.txt`.
#'
#' @details
#'
#' The vectors belong to the tokens of one specific tokenizer, and tokens that
#' are not found in the table are dropped without a warning. The text therefore
#' has to be tokenized in the same way the model was trained. See
#' `vignette("static-embeddings", package = "textrecipes")` for the recipes to
#' use with each model, and for where to download the files.
#'
#' ## Model2Vec
#'
#' `read_embeddings_model2vec()` reads the `embeddings` tensor of
#' `model.safetensors` and uses the lines of `vocab.txt` as the tokens. This
#' needs the safetensors and torch packages. Models that store quantized weights
#' are not supported. The columns of the vectors are named `d1`, `d2`, and so
#' on.
#'
#' ## BPEmb
#'
#' `read_embeddings_bpemb()` reads the word2vec text format, where the first
#' line gives the number of rows and columns, and each other line is a token
#' followed by its numbers. The columns of the vectors are named `d1`, `d2`, and
#' so on.
#'
#' @return A [tibble][tibble::tibble()] with a character column `token`
#'   followed by one numeric column for each dimension of the embeddings.
#'
#' @seealso [step_word_embeddings()] to use the embeddings in a recipe.
#'
#' @examples
#' # A small file in the BPEmb format
#' file <- tempfile(fileext = ".w2v.txt")
#' writeLines(
#'   c("3 2", "<unk> 0.1 0.2", "▁the 0.3 0.4", "▁cat 0.5 0.6"),
#'   file
#' )
#'
#' read_embeddings_bpemb(file)
#' @export
read_embeddings_model2vec <- function(path) {
  check_string(path, arg = "path")
  rlang::check_installed(
    c("safetensors", "torch"),
    reason = "to use `read_embeddings_model2vec()`."
  )

  model_file <- file.path(path, "model.safetensors")
  vocab_file <- file.path(path, "vocab.txt")
  for (file in c(model_file, vocab_file)) {
    if (!file.exists(file)) {
      cli::cli_abort(
        "Can't find {.file {basename(file)}} in {.path {path}}."
      )
    }
  }

  tensors <- safetensors::safe_load_file(model_file, framework = "torch")
  if (!"embeddings" %in% names(tensors)) {
    cli::cli_abort(
      c(
        "{.file model.safetensors} must contain a tensor named
        {.val embeddings}.",
        i = "It contains {.val {names(tensors)}}.",
        i = "Models with quantized weights are not supported."
      )
    )
  }
  vectors <- as.matrix(tensors$embeddings)

  tokens <- readLines(vocab_file, warn = FALSE, encoding = "UTF-8")
  if (length(tokens) != nrow(vectors)) {
    cli::cli_abort(
      "{.file vocab.txt} has {length(tokens)} tokens but
      {.file model.safetensors} has {nrow(vectors)} vectors."
    )
  }

  embeddings_tibble(tokens, vectors)
}

#' @rdname read_embeddings_model2vec
#' @export
read_embeddings_bpemb <- function(path) {
  check_string(path, arg = "path")
  if (!file.exists(path)) {
    cli::cli_abort("Can't find the file {.path {path}}.")
  }

  header <- scan(path, nlines = 1L, quiet = TRUE)
  if (length(header) != 2L) {
    cli::cli_abort(
      "The first line of {.path {path}} must give the number of rows and
      columns."
    )
  }

  # `quote`, `comment.char`, and `na.strings` are turned off because tokens can
  # be quote characters, `#`, or `NA`.
  table <- utils::read.table(
    path,
    skip = 1L,
    quote = "",
    comment.char = "",
    na.strings = character(),
    stringsAsFactors = FALSE,
    encoding = "UTF-8"
  )

  if (nrow(table) != header[1] || ncol(table) != header[2] + 1) {
    cli::cli_abort(
      "The first line of {.path {path}} gives {header[1]} row{?s} and
      {header[2]} vector column{?s}, but the file has {nrow(table)} row{?s}
      and {ncol(table) - 1} vector column{?s}."
    )
  }

  embeddings_tibble(as.character(table[[1]]), as.matrix(table[, -1]))
}

embeddings_tibble <- function(tokens, vectors) {
  storage.mode(vectors) <- "double"
  dimnames(vectors) <- list(NULL, paste0("d", seq_len(ncol(vectors))))
  tibble::add_column(tibble::as_tibble(vectors), token = tokens, .before = 1)
}
