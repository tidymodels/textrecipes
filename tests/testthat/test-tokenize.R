context("test-tokenize")

library(textrecipes)
library(recipes)

test_data <- tibble(text = c("I would not eat them here or there.",
                             "I would not eat them anywhere.",
                             "I would not eat green eggs and ham.",
                             "I do not like them, Sam-I-am.")
                    )

rec <- recipe(~ ., data = test_data)

test_that("output is list when length is 1 or 0", {
  data <- tibble(a = rep(c("a", ""), 20))
  
  data_rec <- recipe(~ ., data = data) %>%
    step_tokenize(a) %>%
    prep(training = data, retain = TRUE)
  
  expect_true(is.list(juice(data_rec, a)[, 1, drop = TRUE]))
})

test_that("tokenization is done correctly", {
  rec <- rec %>%
    step_tokenize(text) 
  
  obj <- rec %>%
    prep(training = test_data, retain = TRUE)
  
  expect_equal(
    list(c("i", "would", "not", "eat", "them", "here", "or", "there"),
         c("i", "would", "not", "eat", "them", "anywhere"),
         c("i", "would", "not", "eat", "green", "eggs", "and", "ham"),
         c("i", "do", "not", "like", "them", "sam", "i", "am")),
    juice(obj) %>% pull(text)
  )
  
  expect_equal(dim(recipes:::tidy.recipe(rec, 1)), c(1, 3))
  expect_equal(dim(recipes:::tidy.recipe(obj, 1)), c(1, 3))
})

test_that("step throws an error if unavaliable tokenizer is picked", {
  expect_error(
    rec %>%
      step_tokenize(text, token = "wrong") %>%
      prep(training = test_data, retain = TRUE)
  )
})

test_that("tokenization works with other built-in tokenizers", {
  rec <- rec %>%
    step_tokenize(text, token = "characters") %>%
    prep(training = test_data, retain = TRUE)
  
  expect_equal(
    tokenizers::tokenize_characters(test_data$text[1]),
    juice(rec) %>% slice(1) %>% pull(text)
  )
})

test_that("tokenization works with custom tokenizer", {
  rec <- rec %>%
    step_tokenize(text, custom_token = tokenizers::tokenize_characters) %>%
    prep(training = test_data, retain = TRUE)
  
  expect_equal(
    tokenizers::tokenize_characters(test_data$text[1]),
    juice(rec) %>% 
      slice(1) %>% 
      pull(text)
  )
})

test_that("arguments are passed using options argument", {
  rec <- rec %>%
    step_tokenize(text, options = list(lowercase = FALSE)) %>%
    prep(training = test_data, retain = TRUE)
  
  expect_equal(
    list(c("I", "would", "not", "eat", "them", "here", "or", "there"),
         c("I", "would", "not", "eat", "them", "anywhere"),
         c("I", "would", "not", "eat", "green", "eggs", "and", "ham"),
         c("I", "do", "not", "like", "them", "Sam", "I", "am")),
    juice(rec) %>% pull(text)
  )
})

test_that("printing", {
  rec <- rec %>%
    step_tokenize(text)
  expect_output(print(rec))
  expect_output(prep(rec, training = test_data, verbose = TRUE))
})
