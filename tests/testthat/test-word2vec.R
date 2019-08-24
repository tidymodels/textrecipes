context("test-word2vec")

set.seed(1234)
library(recipes)
library(textrecipes)

n_rows <- 100
rec <- recipe(~ essay0 + essay1, data = okc_text[seq_len(n_rows), ])

test_that("step_word2vec works as intended", {
  n_top <- 10
  rec1 <- rec %>%
    step_word2vec(essay0, num_topics = n_top)
  
  obj <- rec1 %>%
    prep()
  
  expect_equal(dim(juice(obj)), c(n_rows, n_top + 1))
  
  expect_equal(dim(tidy(rec1, 1)), c(1, 3))
  expect_equal(dim(tidy(obj, 1)), c(1, 3))
})

test_that("step_word2vec works with num_topics argument", {
  n_top <- 100
  rec1 <- rec %>%
    step_word2vec(essay0, num_topics = n_top)
  
  obj <- rec1 %>%
    prep()
  
  expect_equal(dim(juice(obj)), c(n_rows, n_top + 1))
})

test_that("printing", {
  rec <- rec %>%
    step_word2vec(essay0)
  
  expect_output(print(rec))
  expect_output(prep(rec, training = okc_text, verbose = TRUE))
})
