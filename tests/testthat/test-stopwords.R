context("test-stopwords")

library(recipes)
library(textrecipes)

data(okc_text)
okc_rec <- recipe(~ ., data = okc_text)

test_that("stopwords are removed correctly", {
  okc_rec <- okc_rec %>%
    step_tokenize(essay0) %>%
    step_stopwords(essay0) %>%
    prep(training = okc_text, retain = TRUE)
  
  token_words <- tokenizers::tokenize_words(okc_text$essay0[1])[[1]]
  
  expect_equal(
    token_words[!is.element(token_words, stopwords::stopwords())],
    juice(okc_rec) %>% 
      slice(1) %>% 
      pull(essay0) %>%
      unlist()
  )
})

test_that("stopwords are kept correctly", {
  okc_rec <- okc_rec %>%
    step_tokenize(essay0) %>%
    step_stopwords(essay0, keep = TRUE) %>%
    prep(training = okc_text, retain = TRUE)
  
  token_words <- tokenizers::tokenize_words(okc_text$essay0[1])[[1]]
  
  expect_equal(
    token_words[is.element(token_words, stopwords::stopwords())],
    juice(okc_rec) %>% 
      slice(1) %>% 
      pull(essay0) %>%
      unlist()
  )
})

test_that("custom stopwords are supported", {
  custom_stopwords <- c("dead", "babies", "candy")
  
  okc_rec <- okc_rec %>%
    step_tokenize(essay0) %>%
    step_stopwords(essay0, custom_stopword_source = custom_stopwords) %>%
    prep(training = okc_text, retain = TRUE)
  
  token_words <- tokenizers::tokenize_words(okc_text$essay0[1])[[1]]
  
  expect_equal(
    token_words[!is.element(token_words, custom_stopwords)],
    juice(okc_rec) %>% 
      slice(1) %>% 
      pull(essay0) %>%
      unlist()
  )
})