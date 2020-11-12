library(testthat)
library(recipes)
library(textrecipes)

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

  juiced_data <- bake(obj, new_data = NULL)

  expect_equal(dim(juiced_data), c(nrow(test_data), 10))

  expect_true(all(vapply(juiced_data, function(x) all(is.numeric(x)),
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

  expect_error(
    rec %>%
      step_tokenize(text) %>%
      step_sequence_onehot(text, padding = "not pre")
  )
  expect_error(
    rec %>%
      step_tokenize(text) %>%
      step_sequence_onehot(text, truncating = "Wrong")
  )
  expect_error(
    rec %>%
      step_tokenize(text) %>%
      step_sequence_onehot(text, padding = c("pre", "pre"))
  )
  expect_error(
    rec %>%
      step_tokenize(text) %>%
      step_sequence_onehot(text, truncating = "Wrong")
  )
})


test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_sequence_onehot(text)
  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})
