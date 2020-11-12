context("test-lda")

set.seed(1234)
library(recipes)
library(textrecipes)
library(modeldata)
data(okc_text)

n_rows <- 100
rec <- recipe(~ essay0 + essay1, data = okc_text[seq_len(n_rows), ])

test_that("step_lda works as intended", {
  skip_if_not_installed("text2vec")
  n_top <- 10
  rec1 <- rec %>%
    step_tokenize(essay0) %>%
    step_lda(essay0, num_topics = n_top)

  obj <- rec1 %>%
    prep()

  expect_equal(dim(bake(obj, new_data = NULL)), c(n_rows, n_top + 1))

  expect_equal(dim(tidy(rec1, 1)), c(1, 3))
  expect_equal(dim(tidy(obj, 1)), c(1, 3))
})

test_that("step_lda works with num_topics argument", {
  skip_if_not_installed("text2vec")
  n_top <- 100
  rec1 <- rec %>%
    step_tokenize(essay0) %>%
    step_lda(essay0, num_topics = n_top)

  obj <- rec1 %>%
    prep()

  expect_equal(dim(bake(obj, new_data = NULL)), c(n_rows, n_top + 1))
})

test_that("printing", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_tokenize(essay0) %>%
    step_lda(essay0)

  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})
