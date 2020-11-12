context("test-tf")

library(recipes)
library(textrecipes)

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec <- recipe(~., data = test_data)

test_that("step_tf works as intended", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text)

  obj <- rec %>%
    prep()

  rec_answer <- unname(bake(obj, new_data = NULL))
  manual_answer <- unname(tibble(
    am = c(0, 0, 0, 1),
    and = c(0, 0, 1, 0),
    anywhere = c(0, 1, 0, 0),
    do = c(0, 0, 0, 1),
    eat = c(1, 1, 1, 0),
    eggs = c(0, 0, 1, 0),
    green = c(0, 0, 1, 0),
    ham = c(0, 0, 1, 0),
    here = c(1, 0, 0, 0),
    i = c(1, 1, 1, 2),
    like = c(0, 0, 0, 1),
    not = c(1, 1, 1, 1),
    or = c(1, 0, 0, 0),
    sam = c(0, 0, 0, 1),
    them = c(1, 1, 0, 1),
    there = c(1, 0, 0, 0),
    would = c(1, 1, 1, 0)
  ))

  expect_equal(
    as.matrix(rec_answer),
    as.matrix(manual_answer)
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 3))
})

test_that("step_tf works with vocabulary argument", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, vocabulary = letters)

  obj <- rec %>%
    prep()

  expect_length(
    bake(obj, new_data = NULL),
    26
  )
})


test_that("step_tf works with other weighting schemes", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, weight_scheme = "term frequency")

  obj <- rec %>%
    prep()

  rec_answer <- unname(bake(obj, new_data = NULL))
  manual_answer <- unname(tibble(
    am = c(0 / 8, 0 / 6, 0 / 8, 1 / 8),
    and = c(0 / 8, 0 / 6, 1 / 8, 0 / 8),
    anywhere = c(0 / 8, 1 / 6, 0 / 8, 0 / 8),
    do = c(0 / 8, 0 / 6, 0 / 8, 1 / 8),
    eat = c(1 / 8, 1 / 6, 1 / 8, 0 / 8),
    eggs = c(0 / 8, 0 / 6, 1 / 8, 0 / 8),
    green = c(0 / 8, 0 / 6, 1 / 8, 0 / 8),
    ham = c(0 / 8, 0 / 6, 1 / 8, 0 / 8),
    here = c(1 / 8, 0 / 6, 0 / 8, 0 / 8),
    i = c(1 / 8, 1 / 6, 1 / 8, 2 / 8),
    like = c(0 / 8, 0 / 6, 0 / 8, 1 / 8),
    not = c(1 / 8, 1 / 6, 1 / 8, 1 / 8),
    or = c(1 / 8, 0 / 6, 0 / 8, 0 / 8),
    sam = c(0 / 8, 0 / 6, 0 / 8, 1 / 8),
    them = c(1 / 8, 1 / 6, 0 / 8, 1 / 8),
    there = c(1 / 8, 0 / 6, 0 / 8, 0 / 8),
    would = c(1 / 8, 1 / 6, 1 / 8, 0 / 8)
  ))

  expect_equal(
    as.matrix(rec_answer),
    as.matrix(manual_answer)
  )
})

test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text)
  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})
