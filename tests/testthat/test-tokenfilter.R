context("test-tokenfilter")

library(textrecipes)
library(recipes)

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec <- recipe(~., data = test_data)

test_that("tokenfilter removes words correctly using min_times and max_times", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tokenfilter(text, max_times = 3, min_times = 2)

  expect_warning(
    obj <- rec %>%
      prep(),
    "only 3 was available and selected."
  )

  expect_equal(
    bake(obj, new_data = NULL) %>% pull(text) %>% vctrs::field("tokens"),
    list(
      c("would", "eat", "them"),
      c("would", "eat", "them"),
      c("would", "eat"),
      c("them")
    )
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 3))
})

test_that("removes words correctly with min_times, max_times and procentage", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tokenfilter(text, max_times = 0.04, min_times = 0, percentage = TRUE)

  expect_warning(
    obj <- rec %>%
      prep(),
    "only 12 was available and selected."
  )

  expect_equal(
    bake(obj, new_data = NULL) %>% pull(text) %>% vctrs::field("tokens"),
    list(
      c("here", "or", "there"),
      c("anywhere"),
      c("green", "eggs", "and", "ham"),
      c("do", "like", "sam", "am")
    )
  )
})

test_that("tokenfilter removes words correctly using max_tokens", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tokenfilter(text, max_tokens = 10)

  obj <- rec %>%
    prep()

  expect_equal(
    bake(obj, new_data = NULL) %>% pull(text) %>% vctrs::field("tokens"),
    list(
      c("i", "would", "not", "eat", "them"),
      c("i", "would", "not", "eat", "them", "anywhere"),
      c("i", "would", "not", "eat", "eggs", "and"),
      c("i", "do", "not", "them", "i", "am")
    )
  )
})

test_that("tokenfilter throws warning when max_tokens > words", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tokenfilter(text, max_tokens = 10000)

  expect_warning(
    rec %>%
      prep()
  )
})

test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tokenfilter(text)
  expect_output(print(rec))
  expect_warning(
    expect_output(prep(rec, verbose = TRUE))
  )
})
