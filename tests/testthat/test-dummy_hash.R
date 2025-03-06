test_that("hashing gives double outputs", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("grants", package = "modeldata")

  test_data <- grants_test[1:20, c("contract_value_band", "sponsor_code")]
  test_data <- tibble::as_tibble(test_data)

  rec <- recipe(~., data = test_data) %>%
    step_dummy_hash(sponsor_code)

  obj <- rec %>%
    prep()

  expect_true(
    bake(obj, new_data = NULL) %>%
      select(contains("hash")) %>%
      lapply(is.integer) %>%
      unlist() %>%
      all()
  )

  expect_equal(dim(tidy(rec, 1)), c(1, 5))
  expect_equal(dim(tidy(obj, 1)), c(1, 5))
})

test_that("hashing multiple factors", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("text2vec")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("grants", package = "modeldata")

  test_data <- grants_test[1:20, c("contract_value_band", "sponsor_code")]
  test_data <- tibble::as_tibble(test_data)

  res <- recipe(~., data = test_data) %>%
    step_dummy_hash(all_nominal_predictors(), num_terms = 12) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(ncol(res), 24)
  expect_equal(sum(grepl("contract", names(res))), 12)
  expect_equal(sum(grepl("sponsor", names(res))), 12)
})

test_that("hashing collapsed multiple factors", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("text2vec")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("grants", package = "modeldata")

  test_data <- grants_test[1:20, c("contract_value_band", "sponsor_code")]
  test_data <- tibble::as_tibble(test_data)

  res <- recipe(~., data = test_data) %>%
    step_dummy_hash(
      all_nominal_predictors(),
      num_terms = 4,
      collapse = TRUE
    ) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(ncol(res), 4)
  expect_equal(mean(grepl("contract_value_band_sponsor", names(res))), 1)
})

test_that("hashing output width changes accordingly with num_terms", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("grants", package = "modeldata")

  test_data <- grants_test[1:20, c("contract_value_band", "sponsor_code")]
  test_data <- tibble::as_tibble(test_data)

  rec <- recipe(~., data = test_data) %>%
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
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("grants", package = "modeldata")

  test_data <- grants_test[1:20, c("contract_value_band", "sponsor_code")]
  test_data <- tibble::as_tibble(test_data)

  signed <- recipe(~., data = test_data) %>%
    step_dummy_hash(all_predictors(), num_terms = 2) %>%
    prep() %>%
    bake(new_data = NULL)

  unsigned <- recipe(~., data = test_data) %>%
    step_dummy_hash(all_predictors(), num_terms = 2, signed = FALSE) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_true(
    all(
      unsigned$dummyhash_sponsor_code_1 == abs(signed$dummyhash_sponsor_code_1)
    )
  )
  expect_true(
    all(
      unsigned$dummyhash_sponsor_code_2 == abs(signed$dummyhash_sponsor_code_2)
    )
  )
  expect_false(
    all(unsigned$dummyhash_sponsor_code_1 == signed$dummyhash_sponsor_code_1)
  )
  expect_false(
    all(unsigned$dummyhash_sponsor_code_2 == signed$dummyhash_sponsor_code_2)
  )
})

test_that("check_name() is used", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("grants", package = "modeldata")

  test_data <- grants_test[1:20, c("contract_value_band", "sponsor_code")]
  test_data <- tibble::as_tibble(test_data)

  dat <- test_data
  dat$text <- dat$sponsor_code
  dat$dummyhash_text_01 <- dat$sponsor_code

  rec <- recipe(~., data = dat) %>%
    step_dummy_hash(text)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) %>%
    step_dummy_hash(all_predictors())
  rec_param <- tunable.step_dummy_hash(rec$steps[[1]])
  expect_equal(rec_param$name, c("signed", "num_terms"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("bad args", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("text2vec")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_dummy_hash(signed = "yes") %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_dummy_hash(num_terms = -4) %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_dummy_hash(collapse = "yes") %>%
      prep()
  )
})

test_that("sparse = 'yes' works", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("text2vec")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("grants", package = "modeldata")

  test_data <- grants_test[1:20, c("contract_value_band", "sponsor_code")]
  test_data <- tibble::as_tibble(test_data)

  rec <- recipe(~sponsor_code, data = test_data)

  dense <- rec %>%
    step_dummy_hash(sponsor_code, sparse = "no") %>%
    prep() %>%
    bake(NULL)
  sparse <- rec %>%
    step_dummy_hash(sponsor_code, sparse = "yes") %>%
    prep() %>%
    bake(NULL)

  expect_identical(dense, sparse)

  expect_false(any(vapply(dense, sparsevctrs::is_sparse_vector, logical(1))))
  expect_true(all(vapply(sparse, sparsevctrs::is_sparse_vector, logical(1))))
})

test_that("sparse argument is backwards compatible", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("text2vec")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("grants", package = "modeldata")

  test_data <- grants_test[1:20, c("contract_value_band", "sponsor_code")]
  test_data <- tibble::as_tibble(test_data)

  rec <- recipe(~., data = test_data) %>%
    step_dummy_hash(sponsor_code, sparse = "no") %>%
    prep()

  exp <- bake(rec, test_data)

  # Simulate old recipe
  rec$steps[[1]]$sparse <- NULL

  expect_identical(
    bake(rec, test_data),
    exp
  )
})

test_that(".recipes_toggle_sparse_args works", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("text2vec")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("grants", package = "modeldata")

  test_data <- grants_test[1:20, c("contract_value_band", "sponsor_code")]
  test_data <- tibble::as_tibble(test_data)

  rec <- recipe(~., data = test_data) %>%
    step_dummy_hash(sponsor_code, sparse = "auto")

  exp <- rec %>% prep() %>% bake(NULL) %>% sparsevctrs::sparsity()

  expect_true(.recipes_estimate_sparsity(rec) >= exp)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("text2vec")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("grants", package = "modeldata")

  test_data <- grants_test[1:20, c("contract_value_band", "sponsor_code")]
  test_data <- tibble::as_tibble(test_data)

  rec <- recipe(~sponsor_code, data = test_data) %>%
    step_dummy_hash(sponsor_code) %>%
    update_role(sponsor_code, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = test_data, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = test_data[, -2])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_dummy_hash(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
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

  expect <- tibble(
    terms = character(),
    value = logical(),
    num_terms = integer(),
    collapse = logical(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("grants", package = "modeldata")

  test_data <- grants_test[1:20, c("contract_value_band", "sponsor_code")]
  test_data <- tibble::as_tibble(test_data)

  new_names <- paste0("dummyhash_sponsor_code_", 1:5)

  rec <- recipe(~sponsor_code, data = test_data) %>%
    step_dummy_hash(sponsor_code, num_terms = 5, keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~sponsor_code, data = test_data) %>%
    step_dummy_hash(sponsor_code, num_terms = 5, keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("sponsor_code", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("grants", package = "modeldata")

  test_data <- grants_test[1:20, c("contract_value_band", "sponsor_code")]
  test_data <- tibble::as_tibble(test_data)

  rec <- recipe(~sponsor_code, data = test_data) %>%
    step_dummy_hash(sponsor_code)

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = test_data)
  )
})

test_that("printing", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  rec <- recipe(~., data = iris) %>%
    step_dummy_hash(Species)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) %>%
    step_dummy_hash(
      all_predictors(),
      signed = hardhat::tune(),
      num_terms = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})
