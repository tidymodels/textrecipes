tokenizers_sentencepiece_tokens <- function(text, options = list()) {
  temp_dir0 <- tempfile()
  dir.create(temp_dir0)
  on.exit(unlink(temp_dir0, recursive = TRUE))

  temp_file0 <- tempfile(tmpdir = temp_dir0)

  # Delete existing model
  if (file.exists(file.path(tempdir(), "sentencepiece.model"))) {
    file.remove(file.path(tempdir(), "sentencepiece.model"))
  }

  writeLines(text = text, con = temp_file0)

  token_expr <- expr(
    sentencepiece::sentencepiece(
      x = temp_file0,
      threads = 1,
      model_dir = temp_dir0
    )
  )

  if (length(options) > 0) {
    token_expr <- rlang::call_modify(token_expr, !!!options)
  }

  model <- eval(token_expr)

  sentencepiece_model_tokenizer(model$model_path)
}

# Builds the tokenizer function from a sentencepiece `.model` file. The model
# is read into memory so the result does not depend on the file afterwards.
sentencepiece_model_tokenizer <- function(model_path) {
  binary_file <- readBin(model_path, "raw", file.size(model_path))

  # `...` absorbs any training-only `options` that get re-passed at bake
  # time (#319). Those options only apply when the model is trained via
  # `sentencepiece::sentencepiece()` above and have no meaning for
  # `sentencepiece_encode()`, so they are intentionally not forwarded.
  function(x, ...) {
    temp_file <- tempfile()
    on.exit(unlink(temp_file))

    writeBin(binary_file, temp_file)

    model <- sentencepiece::sentencepiece_load_model(temp_file)
    tokens <- sentencepiece::sentencepiece_encode(model, x)
    new_tokenlist(tokens = tokens, unique_tokens = model$vocabulary$subword)
  }
}

check_sentencepiece_model_file <- function(path, call = caller_env()) {
  loaded <- tryCatch(
    sentencepiece::sentencepiece_load_model(path),
    error = function(e) NULL
  )
  if (is.null(loaded)) {
    cli::cli_abort(
      "The file {.file {path}} passed to {.arg model} is not a valid
      sentencepiece model.",
      call = call
    )
  }
  invisible(path)
}
