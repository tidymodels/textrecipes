context("test-stem")

library(recipes)
library(textrecipes)

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec <- recipe(~., data = test_data)

test_that("stemming is done correctly", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_stem(text)

  obj <- rec %>%
    prep()

  expect_equal(
    tokenizers::tokenize_words(test_data$text[1])[[1]] %>%
      SnowballC::wordStem(),
    bake(obj, new_data = NULL) %>%
      slice(1) %>%
      pull(text) %>%
      vctrs::field("tokens") %>%
      unlist()
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 3))
})

test_that("custom stemmer works", {
  custom_stem_fun <- function(x) substr(x, 1, 2)

  rec <- rec %>%
    step_tokenize(text) %>%
    step_stem(text, custom_stemmer = custom_stem_fun)

  obj <- rec %>%
    prep()

  expect_equal(
    tokenizers::tokenize_words(test_data$text[1])[[1]] %>%
      custom_stem_fun(),
    bake(obj, new_data = NULL) %>%
      slice(1) %>%
      pull(text) %>%
      vctrs::field("tokens") %>%
      unlist()
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 3))
})

test_that("arguments are passed by options", {
  data <- tibble(y = 0, text = "коты")

  expect_equal(
    recipe(y ~ ., data = data) %>%
      step_tokenize(text) %>%
      step_stem(text, options = list(language = "russian")) %>%
      prep(data) %>%
      bake(new_data = NULL) %>%
      pull(text) %>%
      vctrs::field("tokens"),
    list("кот")
  )
})


test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_stem(text)
  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})
