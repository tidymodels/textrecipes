context("test-tokenize")

library(recipes)
library(textrecipes)

data(okc_text)
rec <- recipe(~ ., data = okc_text)

test_that("output is list when length is 1 or 0", {
  data <- tibble(a = rep(c("a", ""), 20))
  
  data_rec <- recipe(~ ., data = data) %>%
    step_tokenize(a) %>%
    prep(training = data, retain = TRUE)
  
  expect_true(is.list(juice(data_rec, a)[, 1, drop = TRUE]))
})

test_that("tokenization is done correctly", {
  rec <- rec %>%
    step_tokenize(essay0) 
  
  obj <- rec %>%
    prep(training = okc_text, retain = TRUE)
  
  expect_equal(
    tokenizers::tokenize_words(okc_text$essay0[1]),
    juice(obj) %>% slice(1) %>% pull(essay0)
  )
  
  expect_equal(dim(tidy(rec, 1)), c(1, 2))
  expect_equal(dim(tidy(obj, 1)), c(1, 2))
})

test_that("step throws an error if unavaliable tokenizer is picked", {
  expect_error(
    rec %>%
      step_tokenize(essay0, token = "wrong") %>%
      prep(training = okc_text, retain = TRUE)
  )
})

test_that("tokenization works with other built-in tokenizers", {
  rec <- rec %>%
    step_tokenize(essay0, token = "characters") %>%
    prep(training = okc_text, retain = TRUE)
  
  expect_equal(
    tokenizers::tokenize_characters(okc_text$essay0[1]),
    juice(rec) %>% slice(1) %>% pull(essay0)
  )
})

test_that("tokenization works with custom tokenizer", {
  rec <- rec %>%
    step_tokenize(essay0, custom.token = tokenizers::tokenize_characters) %>%
    prep(training = okc_text, retain = TRUE)
  
  expect_equal(
    tokenizers::tokenize_characters(okc_text$essay0[1]),
    juice(rec) %>% 
      slice(1) %>% 
      pull(essay0)
  )
})

test_that("arguments are passed using options argument", {
  rec <- rec %>%
    step_tokenize(essay0, options = list(lowercase = FALSE)) %>%
    prep(training = okc_text, retain = TRUE)
  
  expect_equal(
    tokenizers::tokenize_words(okc_text$essay0[1], lowercase = FALSE),
    juice(rec) %>% slice(1) %>% pull(essay0)
  )
})

test_that('printing', {
  rec <- rec %>%
    step_tokenize(essay0)
  expect_output(print(rec))
  expect_output(prep(rec, training = okc_text, verbose = TRUE))
})
