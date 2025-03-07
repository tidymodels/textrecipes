test_data <- tibble(
  text = c(
    "I would not eat them here or there.",
    "I would not eat them anywhere.",
    "I would not eat green eggs and ham.",
    "I do not like them, Sam-I-am."
  )
)

rec <- recipe(~., data = test_data)

test_that("step_tf works as intended", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text)

  obj <- rec %>%
    prep()

  rec_answer <- unname(as.data.frame(bake(obj, new_data = NULL)))
  manual_answer <- unname(
    data.frame(
      am = c(0L, 0L, 0L, 1L),
      and = c(0L, 0L, 1L, 0L),
      anywhere = c(0L, 1L, 0L, 0L),
      do = c(0L, 0L, 0L, 1L),
      eat = c(1L, 1L, 1L, 0L),
      eggs = c(0L, 0L, 1L, 0L),
      green = c(0L, 0L, 1L, 0L),
      ham = c(0L, 0L, 1L, 0L),
      here = c(1L, 0L, 0L, 0L),
      i = c(1L, 1L, 1L, 2L),
      like = c(0L, 0L, 0L, 1L),
      not = c(1L, 1L, 1L, 1L),
      or = c(1L, 0L, 0L, 0L),
      sam = c(0L, 0L, 0L, 1L),
      them = c(1L, 1L, 0L, 1L),
      there = c(1L, 0L, 0L, 0L),
      would = c(1L, 1L, 1L, 0L)
    )
  )

  expect_identical(
    as.matrix(rec_answer),
    as.matrix(manual_answer)
  )

  expect_identical(dim(tidy(rec, 2)), c(1L, 3L))
  expect_identical(dim(tidy(obj, 2)), c(1L, 3L))
})

test_that("step_tf works with vocabulary argument", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, vocabulary = letters)

  obj <- rec %>%
    prep()

  expect_length(
    bake(obj, new_data = NULL),
    26
  )
})

test_that("step_tf works with other weighting schemes", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, weight_scheme = "term frequency")

  obj <- rec %>%
    prep()

  rec_answer <- unname(as.data.frame(bake(obj, new_data = NULL)))
  manual_answer <- unname(
    data.frame(
      am = c(0 / 8, 0 / 6, 0 / 8, 1 / 8),
      and = c(0 / 8, 0 / 6, 1 / 8, 0 / 8),
      anywhere = c(0 / 8, 1 / 6, 0 / 8, 0 / 8),
      do = c(0 / 8, 0 / 6, 0 / 8, 1 / 8),
      eat = c(1 / 8, 1 / 6, 1 / 8, 0 / 8),
      eggs = c(0 / 8, 0 / 6, 1 / 8, 0 / 8),
      green = c(0 / 8, 0 / 6, 1 / 8, 0 / 8),
      ham = c(0 / 8, 0 / 6, 1 / 8, 0 / 8),
      here = c(1 / 8, 0 / 6, 0 / 8, 0 / 8),
      i = c(1 / 8, 1 / 6, 1 / 8, 2 / 8),
      like = c(0 / 8, 0 / 6, 0 / 8, 1 / 8),
      not = c(1 / 8, 1 / 6, 1 / 8, 1 / 8),
      or = c(1 / 8, 0 / 6, 0 / 8, 0 / 8),
      sam = c(0 / 8, 0 / 6, 0 / 8, 1 / 8),
      them = c(1 / 8, 1 / 6, 0 / 8, 1 / 8),
      there = c(1 / 8, 0 / 6, 0 / 8, 0 / 8),
      would = c(1 / 8, 1 / 6, 1 / 8, 0 / 8)
    )
  )

  expect_identical(
    as.matrix(rec_answer),
    as.matrix(manual_answer)
  )
})

test_that("step_tf term frequency returns 0 with no tokens", {
  d <- tibble(text = c("a b a d", ""))

  res <- recipe(~text, data = d) %>%
    step_tokenize(text) %>%
    step_tf(text, weight_scheme = "term frequency") %>%
    prep() %>%
    bake(new_data = NULL)

  exp_res <- tibble(
    tf_text_a = c(0.5, 0),
    tf_text_b = c(0.25, 0),
    tf_text_d = c(0.25, 0)
  )
  expect_identical(res, exp_res)
})

test_that("check_name() is used", {
  dat <- test_data
  dat$tf_text_i <- dat$text

  rec <- recipe(~., data = dat) %>%
    step_tokenize(text) %>%
    step_tf(text)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) %>%
    step_tf(all_predictors())
  rec_param <- tunable.step_tf(rec$steps[[1]])
  expect_equal(rec_param$name, c("weight_scheme", "weight"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_tf(weight_scheme = "wrong") %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_tf(weight = "wrong") %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_tf(vocabulary = 1:10) %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_tf(prefix = NULL) %>%
      prep()
  )
})

test_that("sparse = 'yes' works", {
  rec <- recipe(~., data = test_data)

  dense <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, weight_scheme = "raw count", sparse = "no") %>%
    prep() %>%
    bake(NULL)
  sparse <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, weight_scheme = "raw count", sparse = "yes") %>%
    prep() %>%
    bake(NULL)

  expect_identical(dense, sparse)

  expect_false(any(vapply(dense, sparsevctrs::is_sparse_integer, logical(1))))
  expect_true(all(vapply(sparse, sparsevctrs::is_sparse_integer, logical(1))))

  dense <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, weight_scheme = "binary", sparse = "no") %>%
    prep() %>%
    bake(NULL)
  sparse <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, weight_scheme = "binary", sparse = "yes") %>%
    prep() %>%
    bake(NULL)

  expect_identical(dense, sparse)

  expect_false(any(vapply(dense, sparsevctrs::is_sparse_integer, logical(1))))
  expect_true(all(vapply(sparse, sparsevctrs::is_sparse_integer, logical(1))))

  dense <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, weight_scheme = "term frequency", sparse = "no") %>%
    prep() %>%
    bake(NULL)
  sparse <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, weight_scheme = "term frequency", sparse = "yes") %>%
    prep() %>%
    bake(NULL)

  expect_identical(dense, sparse)

  expect_false(any(vapply(dense, sparsevctrs::is_sparse_double, logical(1))))
  expect_true(all(vapply(sparse, sparsevctrs::is_sparse_double, logical(1))))

  dense <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, weight_scheme = "log normalization", sparse = "no") %>%
    prep() %>%
    bake(NULL)
  sparse <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, weight_scheme = "log normalization", sparse = "yes") %>%
    prep() %>%
    bake(NULL)

  expect_identical(dense, sparse)

  expect_false(any(vapply(dense, sparsevctrs::is_sparse_double, logical(1))))
  expect_true(all(vapply(sparse, sparsevctrs::is_sparse_double, logical(1))))
})

test_that("sparse argument is backwards compatible", {
  rec <- recipe(~., data = test_data) %>%
    step_tokenize(text) %>%
    step_tf(text, sparse = "no") %>%
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
  rec <- recipe(~., data = test_data) %>%
    step_tokenize(text) %>%
    step_tf(text, sparse = "auto")

  exp <- rec %>% prep() %>% bake(NULL) %>% sparsevctrs::sparsity()

  expect_true(.recipes_estimate_sparsity(rec) >= exp)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  tokenized_test_data <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    prep() %>%
    bake(new_data = NULL)

  rec <- recipe(tokenized_test_data) %>%
    update_role(text, new_role = "predictor") %>%
    step_tf(text) %>%
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
  rec <- step_tf(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_tf(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tf(rec)

  expect <- tibble(terms = character(), value = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- c(
    "tf_text_am",
    "tf_text_and",
    "tf_text_anywhere",
    "tf_text_do",
    "tf_text_eat",
    "tf_text_eggs",
    "tf_text_green",
    "tf_text_ham",
    "tf_text_here",
    "tf_text_i",
    "tf_text_like",
    "tf_text_not",
    "tf_text_or",
    "tf_text_sam",
    "tf_text_them",
    "tf_text_there",
    "tf_text_would"
  )

  rec <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    step_tf(text, keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    step_tf(text, keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("text", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    step_tf(text)

  rec$steps[[2]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = test_data)
  )
})

test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) %>%
    step_tf(
      all_predictors(),
      weight_scheme = hardhat::tune(),
      weight = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})
