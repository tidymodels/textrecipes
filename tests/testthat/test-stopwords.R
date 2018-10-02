context("test-stopwords")

library(recipes)
library(textrecipes)

data(okc_text)
rec <- recipe(~ ., data = okc_text)

test_that("stopwords are removed correctly", {
  rec <- rec %>%
    step_tokenize(essay0) %>%
    step_stopwords(essay0) 
  
  obj <- rec %>%
    prep(training = okc_text, retain = TRUE)
  
  token_words <- tokenizers::tokenize_words(okc_text$essay0[1])[[1]]
  
  expect_equal(
    token_words[!is.element(token_words, stopwords::stopwords())],
    juice(obj) %>% 
      slice(1) %>% 
      pull(essay0) %>%
      unlist()
  )
  
  expect_equal(dim(tidy(rec)), c(2, 5))
  expect_equal(dim(tidy(obj)), c(2, 5))
})

test_that("stopwords are kept correctly", {
  rec <- rec %>%
    step_tokenize(essay0) %>%
    step_stopwords(essay0, keep = TRUE) %>%
    prep(training = okc_text, retain = TRUE)
  
  token_words <- tokenizers::tokenize_words(okc_text$essay0[1])[[1]]
  
  expect_equal(
    token_words[is.element(token_words, stopwords::stopwords())],
    juice(rec) %>% 
      slice(1) %>% 
      pull(essay0) %>%
      unlist()
  )
})

test_that("custom stopwords are supported", {
  custom_stopwords <- c("dead", "babies", "candy")
  
  rec <- rec %>%
    step_tokenize(essay0) %>%
    step_stopwords(essay0, custom_stopword_source = custom_stopwords) %>%
    prep(training = okc_text, retain = TRUE)
  
  token_words <- tokenizers::tokenize_words(okc_text$essay0[1])[[1]]
  
  expect_equal(
    token_words[!is.element(token_words, custom_stopwords)],
    juice(rec) %>% 
      slice(1) %>% 
      pull(essay0) %>%
      unlist()
  )
})

test_that('printing', {
  rec <- rec %>%
    step_tokenize(essay0) %>%
    step_stopwords(essay0)
  expect_output(print(rec))
  expect_output(prep(rec, training = okc_text, verbose = TRUE))
})
