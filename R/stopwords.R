#' @keywords datagen 
#' @concept preprocessing encoding
#' @export
#' @importFrom recipes add_step step terms_select sel2char ellipse_check 
#' @importFrom recipes check_type
step_stopwords <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           options = list(),
           language = "en",
           keep = F,
           stopword_list = "snowball",
           custom_stopword_list = NULL,
           skip = FALSE
  ) {
    add_step(
      recipe,
      step_stopwords_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        options = options,
        language = language,
        keep = keep,
        stopword_list = stopword_list,
        custome_stopword_list = custom_stopword_list,
        skip = skip
      )
    )
  }

step_tokenize_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           columns = NULL,
           options = NULL,
           token = NULL,
           custom.token = NULL,
           skip = FALSE) {
    step(
      subclass = "stopwords",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      options = options,
      language = language,
      keep = keep,
      stopword_list = stopword_list,
      custome_stopword_list = custom_stopword_list,
      skip = skip
    )
  }

#' @export
prep.step_stopwords <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  training <- factor_to_text(training, col_names)
  
  check_type(training[, col_names], quant = FALSE)
  
  step_stopwords_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    options = x$options,
    language = x$language,
    keep = x$keep,
    stopword_list = x$stopword_list,
    custom_stopword_list = x$custom_stopword_list,
    skip = x$skip
  )
}

#' @export
#' @importFrom tibble as_tibble
#' @importFrom recipes bake prep
bake.step_stopwords <- function(object, newdata, ...) {
  col_names <- object$columns
  # for backward compat
  
  if(!is.null(object$custom_stopword_list)) {
    tokenizer <- object$custom.token
  } else {
    tokenizer <- tokenizers_switch(object$token)
  }
  
  for (i in seq_along(col_names)) {
    newdata[, col_names[i]] <- tokenizer_fun(newdata[, col_names[i]], 
                                             col_names[i],
                                             options = object$options,
                                             token = tokenizer)
  }
  as_tibble(newdata)
}

#' @importFrom rlang expr
tokenizer_fun <- function(data, name, options, token, ...) {
  check_type(data[, name], quant = FALSE)
  
  data <- factor_to_text(data, name)
  
  token_expr <- expr(
    token(
      x = data[, 1, drop = TRUE]
    )
  )
  
  if (length(options) > 0)
    token_expr <- mod_call_args(token_expr, args = options)
  
  out <- tibble::tibble(eval(token_expr))
  names(out) <- name
  out
}

tokenizers_switch <- function(name) {
  possible_tokenizers <- 
    c("characters", "character_shingle", "lines", "ngrams",
      "paragraphs", "ptb", "regex", "sentences", "skip_ngrams",
      "tweets", "words", "word_stems")
  
  if(!(name %in% possible_tokenizers)) 
    stop("token should be one of the supported ",
         paste0("'", possible_tokenizers, "'", collapse = ", "),
         call. = FALSE)
  
  switch(name,
         characters = tokenizers::tokenize_characters,
         character_shingle = tokenizers::tokenize_character_shingles,
         lines = tokenizers::tokenize_lines,
         ngrams = tokenizers::tokenize_ngrams,
         paragraphs = tokenizers::tokenize_paragraphs,
         ptb = tokenizers::tokenize_ptb,
         regex = tokenizers::tokenize_regex,
         sentences = tokenizers::tokenize_sentences,
         skip_ngrams = tokenizers::tokenize_skip_ngrams,
         tweets = tokenizers::tokenize_tweets,
         words = tokenizers::tokenize_words,
         word_stems = tokenizers::tokenize_word_stems
  )
}