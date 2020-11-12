context("test-stopwords")

library(recipes)
library(textrecipes)

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec <- recipe(~., data = test_data)

test_that("stopwords are removed correctly", {
  skip_if_not_installed("stopwords")
  rec <- rec %>%
    step_tokenize(text) %>%
    step_stopwords(text)

  obj <- rec %>%
    prep()

  token_words <- tokenizers::tokenize_words(test_data$text[1])[[1]]

  expect_equal(
    token_words[!is.element(token_words, stopwords::stopwords())],
    bake(obj, new_data = NULL) %>%
      slice(1) %>%
      pull(text) %>%
      vctrs::field("tokens") %>%
      unlist()
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 4))
  expect_equal(dim(tidy(obj, 2)), c(1, 4))
})

test_that("stopwords are kept correctly", {
  skip_if_not_installed("stopwords")
  rec <- rec %>%
    step_tokenize(text) %>%
    step_stopwords(text, keep = TRUE) %>%
    prep()

  token_words <- tokenizers::tokenize_words(test_data$text[1])[[1]]

  expect_equal(
    token_words[is.element(token_words, stopwords::stopwords())],
    bake(rec, new_data = NULL) %>%
      slice(1) %>%
      pull(text) %>%
      vctrs::field("tokens") %>%
      unlist()
  )
})

test_that("custom stopwords are supported", {
  custom_stopwords <- c("i", "not")

  rec <- rec %>%
    step_tokenize(text) %>%
    step_stopwords(text, custom_stopword_source = custom_stopwords) %>%
    prep()

  token_words <- tokenizers::tokenize_words(test_data$text[1])[[1]]

  expect_equal(
    list(
      c("would", "eat", "them", "here", "or", "there"),
      c("would", "eat", "them", "anywhere"),
      c("would", "eat", "green", "eggs", "and", "ham"),
      c("do", "like", "them", "sam", "am")
    ),
    bake(rec, new_data = NULL) %>%
      pull(text) %>%
      vctrs::field("tokens")
  )
})

test_that("printing", {
  skip_if_not_installed("stopwords")
  rec <- rec %>%
    step_tokenize(text) %>%
    step_stopwords(text)
  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})
