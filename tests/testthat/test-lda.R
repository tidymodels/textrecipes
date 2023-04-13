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

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("text2vec")
  tokenized_test_data <- rec %>%
    step_tokenize(medium) %>%
    prep() %>%
    bake(new_data = NULL)

  rec <- recipe(tokenized_test_data) %>%
    update_role(medium, new_role = "predictor") %>%
    step_lda(medium, num_topics = 10) %>%
    update_role(medium, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = tokenized_test_data, verbose = FALSE)

  expect_error(
    bake(trained, new_data = tokenized_test_data[, -1]),
    class = "new_data_missing_column"
  )
})

test_that("check_name() is used", {
  skip_if_not_installed("text2vec")
  dat <- tate_text[seq_len(100), ]
  dat$text <- dat$medium
  dat$lda_text_1 <- dat$text
  
  rec <- recipe(~., data = dat) %>%
    step_tokenize(text) %>%
    step_lda(text)
  
  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("printing", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_tokenize(medium) %>%
    step_lda(medium)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("keep_original_cols works", {
  koc_rec <- rec %>%
    step_tokenize(medium) %>%
    step_lda(medium, num_topics = 5, keep_original_cols = TRUE)

  koc_trained <- prep(koc_rec, training = tate_text, verbose = FALSE)

  koc_pred <- bake(koc_trained, new_data = tate_text, all_predictors())

  expect_equal(
    colnames(koc_pred),
    c(
      "medium", "artist", "lda_medium_1", "lda_medium_2", "lda_medium_3",
      "lda_medium_4", "lda_medium_5"
    )
  )
})

test_that("can prep recipes with no keep_original_cols", {
  koc_rec <- rec %>%
    step_tokenize(medium) %>%
    step_lda(medium, num_topics = 5, keep_original_cols = TRUE)

  koc_rec$steps[[2]]$keep_original_cols <- NULL

  expect_snapshot(
    koc_trained <- prep(koc_rec, training = tate_text, verbose = FALSE)
  )

  expect_error(
    pca_pred <- bake(koc_trained, new_data = tate_text, all_predictors()),
    NA
  )
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
