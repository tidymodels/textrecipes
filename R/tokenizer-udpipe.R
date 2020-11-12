udpipe_words <- function(model) {
  file_con <- file(model$file_model, "rb")
  binary_file <- readBin(
    file_con, "raw",
    file.info(model$file_model)$size
  )
  close(file_con)
  function(x, ...) {
    temp_file <- tempfile()

    writeBin(binary_file, temp_file)

    model$file_model <- temp_file

    tokens <- udpipe::udpipe(x = x, object = model, parser = "none", ...)

    doc_id <- factor(tokens$doc_id, paste0("doc", seq_along(x)))

    token_list <- split(tokens$token, doc_id)
    names(token_list) <- NULL

    lemma_list <- split(tokens$lemma, doc_id)
    names(lemma_list) <- NULL

    pos_list <- split(tokens$upos, doc_id)
    names(pos_list) <- NULL

    textrecipes::tokenlist(token_list, lemma = lemma_list, pos = pos_list)
  }
}
