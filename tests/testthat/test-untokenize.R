context("test-untokenize")

library(recipes)
library(textrecipes)

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec <- recipe(~., data = test_data)

test_that("output is not a list", {
  data <- tibble(a = rep("a", 20))

  rec <- recipe(~., data = data) %>%
    step_tokenize(a) %>%
    step_untokenize(a)

  obj <- rec %>%
    prep()

  expect_true(is.factor(juice(obj, a)[, 1, drop = TRUE]))

  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 3))
})

test_that("working as intended", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_untokenize(text)

  obj <- rec %>%
    prep()

  expect_equal(
    bake(obj, new_data = NULL) %>% pull(text) %>% as.character(),
    c(
      "i would not eat them here or there",
      "i would not eat them anywhere",
      "i would not eat green eggs and ham",
      "i do not like them sam i am"
    )
  )
})

test_that("working as intended", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_untokenize(text, sep = "-")

  obj <- rec %>%
    prep()

  expect_equal(
    bake(obj, new_data = NULL) %>% pull(text) %>% as.character(),
    c(
      "i-would-not-eat-them-here-or-there",
      "i-would-not-eat-them-anywhere",
      "i-would-not-eat-green-eggs-and-ham",
      "i-do-not-like-them-sam-i-am"
    )
  )
})

test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_untokenize(text)
  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})
