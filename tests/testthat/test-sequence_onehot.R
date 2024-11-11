test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec <- recipe(~., data = test_data)

test_that("sequence encoding is done correctly", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_sequence_onehot(text, sequence_length = 10)

  obj <- rec %>%
    prep()

  baked_data <- bake(obj, new_data = NULL)

  expect_equal(dim(baked_data), c(nrow(test_data), 10))

  expect_true(all(vapply(baked_data, function(x) all(is.integer(x)),
    FUN.VALUE = logical(1)
  )))

  expect_equal(
    dim(tidy(rec, 2)),
    c(1, 4)
  )

  expect_equal(
    dim(tidy(obj, 2)),
    c(length(unique(unlist(tokenizers::tokenize_words(test_data$text)))), 4)
  )
})

test_that("padding and truncating works correctly", {
  data <- tibble(text = c(
    "a b c d e f g",
    "a b c",
    ""
  ))

  pad_trunc <- function(seq_length, padding, truncating) {
    recipe(~text, data = data) %>%
      step_tokenize(text) %>%
      step_sequence_onehot(text,
        sequence_length = seq_length,
        padding = padding,
        truncating = truncating
      ) %>%
      prep() %>%
      bake(new_data = NULL, composition = "matrix") %>%
      unname()
  }

  expect_equal(
    pad_trunc(5, "pre", "pre"),
    matrix(c(
      3, 4, 5, 6, 7,
      0, 0, 1, 2, 3,
      0, 0, 0, 0, 0
    ), nrow = 3, byrow = TRUE)
  )

  expect_equal(
    pad_trunc(5, "post", "pre"),
    matrix(c(
      3, 4, 5, 6, 7,
      1, 2, 3, 0, 0,
      0, 0, 0, 0, 0
    ), nrow = 3, byrow = TRUE)
  )

  expect_equal(
    pad_trunc(5, "pre", "post"),
    matrix(c(
      1, 2, 3, 4, 5,
      0, 0, 1, 2, 3,
      0, 0, 0, 0, 0
    ), nrow = 3, byrow = TRUE)
  )

  expect_equal(
    pad_trunc(5, "post", "post"),
    matrix(c(
      1, 2, 3, 4, 5,
      1, 2, 3, 0, 0,
      0, 0, 0, 0, 0
    ), nrow = 3, byrow = TRUE)
  )

  expect_snapshot(
    error = TRUE,
    rec %>%
      step_tokenize(text) %>%
      step_sequence_onehot(text, padding = "not pre")
  )
  expect_snapshot(
    error = TRUE,
    rec %>%
      step_tokenize(text) %>%
      step_sequence_onehot(text, truncating = "Wrong")
  )
  expect_snapshot(
    error = TRUE,
    rec %>%
      step_tokenize(text) %>%
      step_sequence_onehot(text, padding = c("pre", "pre"))
  )
  expect_snapshot(
    error = TRUE,
    rec %>%
      step_tokenize(text) %>%
      step_sequence_onehot(text, truncating = "Wrong")
  )
})

test_that("check_name() is used", {
  dat <- test_data
  dat$seq1hot_text_1 <- dat$text
  
  rec <- recipe(~., data = dat) %>%
    step_tokenize(text) %>%
    step_sequence_onehot(text)
  
  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_sequence_onehot(padding = "yes")
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_sequence_onehot(truncating = "yes")
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_sequence_onehot(sequence_length = -4) %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_sequence_onehot(prefix = NULL) %>%
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  tokenized_test_data <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    prep() %>%
    bake(new_data = NULL)
  
  rec <- recipe(tokenized_test_data) %>%
    update_role(text, new_role = "predictor") %>%
    step_sequence_onehot(text) %>%
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
  rec <- step_sequence_onehot(rec)
  
  expect_snapshot(rec)
  
  rec <- prep(rec, mtcars)
  
  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_sequence_onehot(rec1)
  
  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)
  
  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)
  
  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_sequence_onehot(rec)
  
  expect <- tibble(
    terms = character(),
    vocabulary = character(),
    token = integer(),
    id = character()
  )
  
  expect_identical(tidy(rec, number = 1), expect)
  
  rec <- prep(rec, mtcars)
  
  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- paste0("seq1hot_text_", 1:100)
  
  rec <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    step_sequence_onehot(text, keep_original_cols = FALSE)
  
  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)
  
  expect_equal(
    colnames(res),
    new_names
  )
  
  rec <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    step_sequence_onehot(text, keep_original_cols = TRUE)
  
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
    step_sequence_onehot(text)
  
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
    step_sequence_onehot(text)
  
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
