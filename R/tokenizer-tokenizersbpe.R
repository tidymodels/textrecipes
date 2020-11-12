tokenizers_bpe_words <- function(text, options = list()) {
  temp_file0 <- tempfile()

  token_expr <- expr(
    tokenizers.bpe::bpe(
      x = text,
      threads = 1,
      model_path = temp_file0
    )
  )

  if (length(options) > 0) {
    token_expr <- mod_call_args(token_expr, args = options)
  }

  ddd <- utils::capture.output(type = "message", {
    model <- eval(token_expr)
  })

  model_code <- readLines(temp_file0)

  function(x) {
    temp_file <- tempfile()

    writeLines(model_code, temp_file)

    model <- tokenizers.bpe::bpe_load_model(temp_file)
    tokens <- tokenizers.bpe::bpe_encode(model, x)
    new_tokenlist(tokens = tokens, unique_tokens = model$vocabulary$subword)
  }
}
