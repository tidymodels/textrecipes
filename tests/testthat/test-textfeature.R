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

  juiced_data <- bake(obj, new_data = NULL)

  expect_equal(dim(juiced_data), c(nrow(test_data), length(count_functions)))

  expect_true(all(vapply(juiced_data, function(x) all(is.numeric(x)),
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

  expect_error(
    rec %>%
      step_textfeature(text, extract_functions = list(as.character)) %>%
      prep()
  )

  expect_error(
    rec %>%
      step_textfeature(
        text,
        extract_functions = list(function(x) 1)
      ) %>%
      prep()
  )
})

test_that("printing", {
  skip_if_not_installed("textfeatures")
  rec <- rec %>%
    step_textfeature(text)
  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})
