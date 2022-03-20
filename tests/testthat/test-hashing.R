library(textrecipes)
library(recipes)

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec <- recipe(~., data = test_data)

test_that("hashing gives double outputs", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text)

  obj <- rec %>%
    prep()

  expect_true(
    bake(obj, new_data = NULL) %>%
      select(contains("hash")) %>%
      lapply(is.double) %>%
      unlist() %>%
      all()
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 4))
  expect_equal(dim(tidy(obj, 2)), c(1, 4))
})

test_that("hashing output width changes accordingly with num_terms", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text, num_terms = 256) %>%
    prep()

  expect_equal(
    bake(rec, new_data = NULL) %>%
      select(contains("hash")) %>%
      ncol(),
    256
  )
})

test_that("hashing output width changes accordingly with num_terms", {
  skip_if_not_installed("text2vec")

  signed <- recipe(~., data = test_data) %>%
    step_tokenize(all_predictors()) %>%
    step_texthash(all_predictors(), num_terms = 2) %>%
    prep() %>%
    bake(new_data = NULL)

  unsigned <- recipe(~., data = test_data) %>%
    step_tokenize(all_predictors()) %>%
    step_texthash(all_predictors(), num_terms = 2, signed = FALSE) %>%
    prep() %>%
    bake(new_data = NULL)

  all(unsigned$texthash_text_1 == signed$texthash_text_1)
  all(unsigned$texthash_text_2 == signed$texthash_text_2)
  expect_false(all(unsigned$texthash_text_1 == signed$texthash_text_1))
  expect_false(all(unsigned$texthash_text_2 == signed$texthash_text_2))
})

test_that("printing", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text)
  expect_snapshot(print(rec))
})

test_that("keep_original_cols works", {
  koc_rec <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text, keep_original_cols = TRUE, num_terms = 4)
  
  koc_trained <- prep(koc_rec, training = test_data, verbose = FALSE)
  
  koc_pred <- bake(koc_trained, new_data = test_data, all_predictors())
  
  expect_equal(
    colnames(koc_pred),
    c(
      "texthash_text_1", "texthash_text_2", "texthash_text_3",
      "texthash_text_4", "text"
    )
  )
})

test_that("can prep recipes with no keep_original_cols", {
  koc_rec <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text, keep_original_cols = TRUE)
  
  koc_rec$steps[[2]]$keep_original_cols <- NULL
  
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
  rec2 <- step_texthash(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_texthash(rec)

  expect_identical(
    tidy(rec, number = 1),
    tibble(
      terms = character(),
      value = logical(),
      length = integer(),
      id = character()
    )
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(
      terms = character(),
      value = logical(),
      length = integer(),
      id = character()
    )
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_texthash(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
