spacyr_tokenizer_words <- function(x) {
  tokens <- spacyr::spacy_parse(x, multithread = FALSE)
  doc_id <- factor(tokens$doc_id, paste0("text", seq_along(x)))
  token_list <- split(tokens$token, doc_id)
  token_list <- unname(token_list)
  
  tokenlist(token_list)
}