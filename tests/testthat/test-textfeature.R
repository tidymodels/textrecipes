test_data <- tibble(
  text = c(
    "I would not eat them here or there.",
    "I would not eat them anywhere.",
    "I would not eat green eggs and ham.",
    "I do not like them, Sam-I-am."
  )
)

rec <- recipe(~., data = test_data)

test_that("textfeature extraction is done correctly", {
  rec <- rec %>%
    step_textfeature(text)

  obj <- rec %>%
    prep()

  baked_data <- bake(obj, new_data = NULL)

  expect_equal(dim(baked_data), c(nrow(test_data), length(count_functions)))

  expect_true(
    all(
      vapply(baked_data, function(x) all(is.numeric(x)), FUN.VALUE = logical(1))
    )
  )

  expect_equal(dim(tidy(rec, 1)), c(1, 3))
  expect_equal(dim(tidy(obj, 1)), c(length(count_functions), 3))
})

test_that("custom extraction functions work works", {
  nchar1 <- function(x) nchar(x) + 1
  nchar2 <- function(x) nchar(x) + 2
  nchar3 <- function(x) nchar(x) + 3

  rec <- rec %>%
    step_textfeature(
      text,
      extract_functions = list(
        nchar1 = nchar1,
        nchar2 = nchar2,
        nchar3 = nchar3
      )
    )

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

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_textfeature(prefix = NULL) %>%
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~text, data = test_data) %>%
    step_textfeature(text) %>%
    update_role(text, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = test_data, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = test_data[, -1])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_textfeature(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
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

  expect <- tibble(
    terms = character(),
    functions = character(),
    id = character()
  )

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
test_that("keep_original_cols works", {
  new_names <- paste0("textfeature_text_", names(count_functions))

  rec <- recipe(~text, data = test_data) %>%
    step_textfeature(text, keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~text, data = test_data) %>%
    step_textfeature(text, keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("text", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~text, data = test_data) %>%
    step_textfeature(text)

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = test_data)
  )
})

test_that("printing", {
  rec <- rec %>%
    step_textfeature(text)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
