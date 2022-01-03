set.seed(1234)
library(recipes)
library(textrecipes)
library(modeldata)
data(tate_text)

n_rows <- 100
rec <- recipe(~ medium + artist, data = tate_text[seq_len(n_rows), ])

test_that("step_lda works as intended", {
  skip_if_not_installed("text2vec")
  n_top <- 10
  rec1 <- rec %>%
    step_tokenize(medium) %>%
    step_lda(medium, num_topics = n_top)

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
    step_tokenize(medium) %>%
    step_lda(medium, num_topics = n_top)

  obj <- rec1 %>%
    prep()

  expect_equal(dim(bake(obj, new_data = NULL)), c(n_rows, n_top + 1))
})

test_that("printing", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_tokenize(medium) %>%
    step_lda(medium)

  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_lda(rec1)
  
  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)
  
  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)
  
  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_lda(rec)
  
  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), num_topics = integer(), id = character())
  )
  
  rec <- prep(rec, mtcars)
  
  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), num_topics = integer(), id = character())
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_lda(rec)
  
  expect_snapshot(rec)
  
  rec <- prep(rec, mtcars)
  
  expect_snapshot(rec)
})