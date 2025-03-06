test_data <- tibble(
  text = c(
    "I would not eat them here or there.",
    "I would not eat them anywhere.",
    "I would not eat green eggs and ham.",
    "I do not like them, Sam-I-am."
  )
)

rec <- recipe(~., data = test_data)

test_that("hashing gives double outputs", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  rec <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text)

  obj <- rec %>%
    prep()

  expect_true(
    bake(obj, new_data = NULL) %>%
      select(contains("hash")) %>%
      lapply(is.integer) %>%
      unlist() %>%
      all()
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 4))
  expect_equal(dim(tidy(obj, 2)), c(1, 4))
})

test_that("hashing output width changes accordingly with num_terms", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  data.table::setDTthreads(2) # because data.table uses all cores by default

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
  skip_if_not_installed("data.table")
  data.table::setDTthreads(2) # because data.table uses all cores by default

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

test_that("check_name() is used", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  dat <- test_data
  dat$texthash_text_0001 <- dat$text

  rec <- recipe(~., data = dat) %>%
    step_tokenize(text) %>%
    step_texthash(text)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) %>%
    step_texthash(all_predictors())
  rec_param <- tunable.step_texthash(rec$steps[[1]])
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
  skip_if_not_installed("text2vec")

  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_texthash(signed = "yes") %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_texthash(num_terms = -4) %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_texthash(prefix = NULL) %>%
      prep()
  )
})

test_that("sparse = 'yes' works", {
  skip_if_not_installed("text2vec")

  rec <- recipe(~., data = test_data)

  dense <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text, sparse = "no") %>%
    prep() %>%
    bake(NULL)
  sparse <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text, sparse = "yes") %>%
    prep() %>%
    bake(NULL)

  expect_identical(dense, sparse)

  expect_false(any(vapply(dense, sparsevctrs::is_sparse_vector, logical(1))))
  expect_true(all(vapply(sparse, sparsevctrs::is_sparse_vector, logical(1))))
})

test_that("sparse argument is backwards compatible", {
  skip_if_not_installed("text2vec")

  rec <- recipe(~., data = test_data) %>%
    step_tokenize(text) %>%
    step_texthash(text, sparse = "no") %>%
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
  skip_if_not_installed("text2vec")

  rec <- recipe(~., data = test_data) %>%
    step_tokenize(text) %>%
    step_texthash(text, sparse = "auto")

  exp <- rec %>% prep() %>% bake(NULL) %>% sparsevctrs::sparsity()

  expect_true(.recipes_estimate_sparsity(rec) > exp)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("text2vec")

  tokenized_test_data <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    prep() %>%
    bake(new_data = NULL)

  rec <- recipe(tokenized_test_data) %>%
    update_role(text, new_role = "predictor") %>%
    step_texthash(text) %>%
    update_role(text, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = tokenized_test_data, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = tokenized_test_data[, -1])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_texthash(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
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

  expect <- tibble(
    terms = character(),
    value = logical(),
    length = integer(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  new_names <- paste0("texthash_text_", 1:5)

  rec <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    step_texthash(text, num_terms = 5, keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    step_texthash(text, num_terms = 5, keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("text", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_if_not_installed("text2vec")

  rec <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    step_texthash(text)

  rec$steps[[2]]$keep_original_cols <- NULL

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

  rec <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) %>%
    step_texthash(
      all_predictors(),
      signed = hardhat::tune(),
      num_terms = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})
