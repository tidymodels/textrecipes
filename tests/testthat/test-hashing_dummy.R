library(textrecipes)
library(recipes)
data(grants, package = "modeldata")

test_data <- grants_test[1:20, c("contract_value_band", "sponsor_code")]
test_data <- tibble::as_tibble(test_data)

rec <- recipe(~., data = test_data)

test_that("hashing gives double outputs", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_dummy_hash(sponsor_code)

  obj <- rec %>%
    prep()

  expect_true(
    bake(obj, new_data = NULL) %>%
      select(contains("hash")) %>%
      lapply(is.double) %>%
      unlist() %>%
      all()
  )

  expect_equal(dim(tidy(rec, 1)), c(1, 5))
  expect_equal(dim(tidy(obj, 1)), c(1, 5))
})

test_that("hashing multiple factors", {
  res <- rec %>%
    step_dummy_hash(all_nominal_predictors(), num_terms = 12) %>%
    prep() %>%
    juice()

  expect_equal(ncol(res), 24)
  expect_equal(sum(grepl("contract", names(res))), 12)
  expect_equal(sum(grepl("sponsor", names(res))), 12)
})

test_that("hashing collapsed multiple factors", {
  res <- rec %>%
    step_dummy_hash(all_nominal_predictors(), num_terms = 4, collapse = TRUE) %>%
    prep() %>%
    juice()

  expect_equal(ncol(res), 4)
  expect_equal(mean(grepl("contract_value_band_sponsor", names(res))), 1)
})

test_that("hashing output width changes accordingly with num_terms", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_dummy_hash(sponsor_code, num_terms = 256) %>%
    prep()

  expect_equal(
    bake(rec, new_data = NULL) %>%
      select(contains("dummyhash")) %>%
      ncol(),
    256
  )
})

test_that("hashing output width changes accordingly with num_terms", {
  skip_if_not_installed("text2vec")

  signed <- recipe(~., data = test_data) %>%
    step_dummy_hash(all_predictors(), num_terms = 2) %>%
    prep() %>%
    bake(new_data = NULL)

  unsigned <- recipe(~., data = test_data) %>%
    step_dummy_hash(all_predictors(), num_terms = 2, signed = FALSE) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_true(all(unsigned$dummyhash_sponsor_code_1 == abs(signed$dummyhash_sponsor_code_1)))
  expect_true(all(unsigned$dummyhash_sponsor_code_2 == abs(signed$dummyhash_sponsor_code_2)))
  expect_false(all(unsigned$dummyhash_sponsor_code_1 == signed$dummyhash_sponsor_code_1))
  expect_false(all(unsigned$dummyhash_sponsor_code_2 == signed$dummyhash_sponsor_code_2))
})

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~sponsor_code, data = test_data) %>%
    step_dummy_hash(sponsor_code) %>%
    update_role(sponsor_code, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)
  
  trained <- prep(rec, training = test_data, verbose = FALSE)
  
  expect_error(bake(trained, new_data = test_data[, -2]),
               class = "new_data_missing_column")
})

test_that("printing", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_dummy_hash(sponsor_code)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("keep_original_cols works", {
  koc_rec <- rec %>%
    step_dummy_hash(sponsor_code, num_terms = 4, keep_original_cols = TRUE)
  
  koc_trained <- prep(koc_rec, training = test_data, verbose = FALSE)
  
  koc_pred <- bake(koc_trained, new_data = test_data, all_predictors())
  
  expect_equal(
    colnames(koc_pred),
    c(
      "dummyhash_sponsor_code_1", "dummyhash_sponsor_code_2", "dummyhash_sponsor_code_3", 
      "dummyhash_sponsor_code_4", "contract_value_band", "sponsor_code"
    )
  )
})

test_that("can prep recipes with no keep_original_cols", {
  koc_rec <- rec %>%
    step_dummy_hash(sponsor_code, keep_original_cols = TRUE)
  
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
  rec2 <- step_dummy_hash(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_dummy_hash(rec)

  expect_identical(
    tidy(rec, number = 1),
    tibble(
      terms = character(),
      value = logical(),
      num_terms = integer(),
      collapse = logical(),
      id = character()
    )
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(
      terms = character(),
      value = logical(),
      num_terms = integer(),
      collapse = logical(),
      id = character()
    )
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_dummy_hash(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
