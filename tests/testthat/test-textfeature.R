library(recipes)
library(textrecipes)

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec <- recipe(~., data = test_data)

test_that("textfeature extraction is done correctly", {
  skip_if_not_installed("textfeatures")
  library(textfeatures)
  rec <- rec %>%
    step_textfeature(text)

  obj <- rec %>%
    prep()

  baked_data <- bake(obj, new_data = NULL)

  expect_equal(dim(baked_data), c(nrow(test_data), length(count_functions)))

  expect_true(all(vapply(baked_data, function(x) all(is.numeric(x)),
    FUN.VALUE = logical(1)
  )))

  expect_equal(dim(tidy(rec, 1)), c(1, 3))
  expect_equal(dim(tidy(obj, 1)), c(length(count_functions), 3))
})

test_that("custom extraction functions work works", {
  skip_if_not_installed("textfeatures")
  nchar1 <- function(x) nchar(x) + 1
  nchar2 <- function(x) nchar(x) + 2
  nchar3 <- function(x) nchar(x) + 3

  rec <- rec %>%
    step_textfeature(text, extract_functions = list(
      nchar1 = nchar1,
      nchar2 = nchar2,
      nchar3 = nchar3
    ))

  obj <- rec %>%
    prep()

  expect_equal(dim(bake(obj, new_data = NULL)), c(nrow(test_data), 3))

  expect_snapshot(
    error = TRUE,
    rec %>%
      step_textfeature(text, extract_functions = list(as.character)) %>%
      prep()
  )

  expect_snapshot(
    error = TRUE,
    rec %>%
      step_textfeature(
        text,
        extract_functions = list(function(x) 1)
      ) %>%
      prep()
  )
})

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~text, data = test_data) %>%
    step_textfeature(text) %>%
    update_role(text, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = test_data, verbose = FALSE)

  expect_error(
    bake(trained, new_data = test_data[, -1]),
    class = "new_data_missing_column"
  )
})

test_that("check_name() is used", {
  dat <- test_data
  dat$textfeature_text_n_words <- dat$text
  
  rec <- recipe(~., data = dat) %>%
    step_textfeature(text)
  
  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("printing", {
  skip_if_not_installed("textfeatures")
  rec <- rec %>%
    step_textfeature(text)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("keep_original_cols works", {
  koc_rec <- rec %>%
    step_textfeature(text,
      extract_functions = list(nchar = nchar),
      keep_original_cols = TRUE
    )

  koc_trained <- prep(koc_rec, training = test_data, verbose = FALSE)

  koc_pred <- bake(koc_trained, new_data = test_data, all_predictors())

  expect_equal(
    colnames(koc_pred),
    c(
      "text", "textfeature_text_nchar"
    )
  )
})

test_that("can prep recipes with no keep_original_cols", {
  koc_rec <- rec %>%
    step_textfeature(text, keep_original_cols = TRUE)

  koc_rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    koc_trained <- prep(koc_rec, training = test_data, verbose = FALSE)
  )

  expect_error(
    pca_pred <- bake(koc_trained, new_data = test_data, all_predictors()),
    NA
  )
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_textfeature(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_textfeature(rec)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), functions = character(), id = character())
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), functions = character(), id = character())
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_textfeature(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
