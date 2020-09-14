tokenizers_bpe_words <- function(text, ...) {
  temp_file0 <- tempfile()
  
  ddd <- utils::capture.output(type = "message", {
    model <- tokenizers.bpe::bpe(text, 
                                 threads = 1, 
                                 model_path = temp_file0, 
                                 ...)
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

