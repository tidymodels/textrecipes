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

  file_con <- file(model$model_path, "rb")
  binary_file <- readBin(
    file_con, "raw",
    file.info(model$model_path)$size
  )
  close(file_con)

  function(x) {
    temp_file <- tempfile()

    writeBin(binary_file, temp_file)

    model <- sentencepiece::sentencepiece_load_model(temp_file)
    tokens <- sentencepiece::sentencepiece_encode(model, x)
    new_tokenlist(tokens = tokens, unique_tokens = model$vocabulary$subword)
  }
}
