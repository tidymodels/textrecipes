context("test-tfidf")

library(recipes)
library(textrecipes)

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec <- recipe(~., data = test_data)

test_that("step_tfidf works as intended", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tfidf(text)

  obj <- rec %>%
    prep()

  rec_answer <- unname(bake(obj, new_data = NULL))

  manual_answer <- unname(
    tibble(
      am = c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(1 + 4 / 1),
      and = c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(1 + 4 / 1),
      anywhere = c(0 / 8, 1 / 6, 0 / 8, 0 / 8) * log(1 + 4 / 1),
      do = c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(1 + 4 / 1),
      eat = c(1 / 8, 1 / 6, 1 / 8, 0 / 8) * log(1 + 4 / 3),
      eggs = c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(1 + 4 / 1),
      green = c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(1 + 4 / 1),
      ham = c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(1 + 4 / 1),
      here = c(1 / 8, 0 / 6, 0 / 8, 0 / 8) * log(1 + 4 / 1),
      i = c(1 / 8, 1 / 6, 1 / 8, 2 / 8) * log(1 + 4 / 4),
      like = c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(1 + 4 / 1),
      not = c(1 / 8, 1 / 6, 1 / 8, 1 / 8) * log(1 + 4 / 4),
      or = c(1 / 8, 0 / 6, 0 / 8, 0 / 8) * log(1 + 4 / 1),
      sam = c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(1 + 4 / 1),
      them = c(1 / 8, 1 / 6, 0 / 8, 1 / 8) * log(1 + 4 / 3),
      there = c(1 / 8, 0 / 6, 0 / 8, 0 / 8) * log(1 + 4 / 1),
      would = c(1 / 8, 1 / 6, 1 / 8, 0 / 8) * log(1 + 4 / 3)
    )
  )

  expect_equal(
    as.matrix(rec_answer),
    as.matrix(manual_answer)
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 2))
  expect_equal(dim(tidy(obj, 2)), c(1, 2))
})

test_that("step_tfidf works with vocabulary argument", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tfidf(text, vocabulary = letters)

  obj <- rec %>%
    prep()

  expect_length(
    bake(obj, new_data = NULL),
    26
  )
})

test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tfidf(text)
  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})
