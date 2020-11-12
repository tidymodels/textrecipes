spacyr_tokenizer_words <- function(x) {
  tokens <- spacyr::spacy_parse(x, multithread = FALSE)
  doc_id <- factor(tokens$doc_id, paste0("text", seq_along(x)))
  token_list <- split(tokens$token, doc_id)
  names(token_list) <- NULL

  lemma_list <- split(tokens$lemma, doc_id)
  names(lemma_list) <- NULL

  pos_list <- split(tokens$pos, doc_id)
  names(pos_list) <- NULL

  tokenlist(token_list, lemma = lemma_list, pos = pos_list)
}
