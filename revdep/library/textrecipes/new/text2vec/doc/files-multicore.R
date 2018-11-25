## ----global_options, include=FALSE, echo=FALSE---------------------------
knitr::opts_chunk$set(echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
library(text2vec)
library(magrittr)
data("movie_review")

# remove all internal EOL to simplify reading
movie_review$review = gsub(pattern = '\n', replacement = ' ', 
                            x = movie_review$review, fixed = TRUE)
N_FILES = 10
CHUNK_LEN = nrow(movie_review) / N_FILES
files = sapply(1:N_FILES, function(x) tempfile())
chunks = split(movie_review, rep(1:N_FILES, 
                                  each = nrow(movie_review) / N_FILES ))
for (i in 1:N_FILES ) {
  write.table(chunks[[i]], files[[i]], quote = T, row.names = F,
              col.names = T, sep = '|')
}

# Note what the moview review data looks like
str(movie_review, strict.width = 'cut')

## ------------------------------------------------------------------------
library(data.table)
reader = function(x, ...) {
  # read
  chunk = data.table::fread(x, header = T, sep = '|')
  # select column with review
  res = chunk$review
  # assign ids to reviews
  names(res) = chunk$id
  res
}
# create iterator over files
it_files  = ifiles(files, reader = reader)
# create iterator over tokens from files iterator
it_tokens = itoken(it_files, preprocessor = tolower, tokenizer = word_tokenizer, progressbar = FALSE)

vocab = create_vocabulary(it_tokens)

## ------------------------------------------------------------------------
dtm = create_dtm(it_tokens, vectorizer = vocab_vectorizer(vocab))
str(dtm, list.len = 5)

## ------------------------------------------------------------------------
for (i in 1:N_FILES ) {
  write.table(chunks[[i]][["review"]], files[[i]], quote = T, row.names = F,
              col.names = T, sep = '|')
}
# read with default reader - readLines
it_files  = ifiles(files)
# create iterator over tokens from files iterator
it_tokens = itoken(it_files, preprocessor = tolower,  tokenizer = word_tokenizer, progressbar = FALSE)
dtm = create_dtm(it_tokens, vectorizer = hash_vectorizer())
str(dtm, list.len = 5)

## ---- eval=FALSE---------------------------------------------------------
#  N_WORKERS = 2
#  library(doParallel)
#  # register parallel backend
#  registerDoParallel(N_WORKERS)
#  
#  # note that we can control level of granularity with `n_chunks` argument
#  it_token_par = itoken_parallel(movie_review$review, preprocessor = tolower,
#                                 tokenizer = word_tokenizer, ids = movie_review$id,
#                                 n_chunks = 8)
#  
#  vocab = create_vocabulary(it_token_par)
#  v_vectorizer = vocab_vectorizer(vocab)
#  dtm = create_dtm(it_token_par, vectorizer = v_vectorizer)
#  

## ---- warning=FALSE, message=FALSE, eval=FALSE---------------------------
#  N_WORKERS = 2
#  library(doParallel)
#  # register parallel backend
#  registerDoParallel(N_WORKERS)
#  
#  it_files_par = ifiles_parallel(file_paths = files)
#  it_token_par = itoken_parallel(it_files_par, preprocessor = tolower, tokenizer = word_tokenizer)
#  
#  vocab = create_vocabulary(it_token_par)
#  # DTM vocabulary vectorization
#  v_vectorizer = vocab_vectorizer(vocab)
#  dtm_v = create_dtm(it_token_par, vectorizer = v_vectorizer)
#  
#  # DTM hash vectorization
#  h_vectorizer = hash_vectorizer()
#  dtm_h = create_dtm(it_token_par, vectorizer = h_vectorizer)
#  
#  # co-ocurence statistics
#  tcm = create_tcm(it_token_par, vectorizer = v_vectorizer, skip_grams_window = 5)

