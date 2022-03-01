library(textrecipes)
library(recipes)
data(grants, package = "modeldata")

test_data <- grants_test[1:20, c("contract_value_band", "sponsor_code")]


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
  expect_equal(sum(grepl("^contract", names(res))), 12)
  expect_equal(sum(grepl("^sponsor", names(res))), 12)
})


test_that("hashing collapsed multiple factors", {
  res <- rec %>%
    step_dummy_hash(all_nominal_predictors(), num_terms = 4, collapse = TRUE) %>%
    prep() %>%
    juice()

  expect_equal(ncol(res), 4)
  expect_equal(mean(grepl("^contract_value_band_sponsor", names(res))), 1)
})

test_that("hashing output width changes accordingly with num_terms", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_dummy_hash(sponsor_code, num_terms = 256) %>%
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
    step_dummy_hash(all_predictors(), num_terms = 2) %>%
    prep() %>%
    bake(new_data = NULL)

  unsigned <- recipe(~., data = test_data) %>%
    step_dummy_hash(all_predictors(), num_terms = 2, signed = FALSE) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_true(all(unsigned$sponsor_code_hash1 == abs(signed$sponsor_code_hash1)))
  expect_true(all(unsigned$sponsor_code_hash2 == abs(signed$sponsor_code_hash2)))
  expect_false(all(unsigned$sponsor_code_hash1 == signed$sponsor_code_hash1))
  expect_false(all(unsigned$sponsor_code_hash2 == signed$sponsor_code_hash2))
})

test_that("printing", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_dummy_hash(sponsor_code)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec, verbose = TRUE))
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
