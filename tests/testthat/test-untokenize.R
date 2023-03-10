library(recipes)
library(textrecipes)

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec <- recipe(~., data = test_data)

test_that("output is not a list", {
  data <- tibble(a = rep("a", 20))

  rec <- recipe(~., data = data) %>%
    step_tokenize(a) %>%
    step_untokenize(a)

  obj <- rec %>%
    prep()

  expect_true(is.factor(bake(obj, new_data = NULL, a)[, 1, drop = TRUE]))

  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 3))
})

test_that("working as intended", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_untokenize(text)

  obj <- rec %>%
    prep()

  expect_equal(
    bake(obj, new_data = NULL) %>% pull(text) %>% as.character(),
    c(
      "i would not eat them here or there",
      "i would not eat them anywhere",
      "i would not eat green eggs and ham",
      "i do not like them sam i am"
    )
  )
})

test_that("working as intended", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_untokenize(text, sep = "-")

  obj <- rec %>%
    prep()

  expect_equal(
    bake(obj, new_data = NULL) %>% pull(text) %>% as.character(),
    c(
      "i-would-not-eat-them-here-or-there",
      "i-would-not-eat-them-anywhere",
      "i-would-not-eat-green-eggs-and-ham",
      "i-do-not-like-them-sam-i-am"
    )
  )
})

test_that("bake method errors when needed non-standard role columns are missing", {
  tokenized_test_data <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    prep() %>%
    bake(new_data = NULL)

  rec <- recipe(tokenized_test_data) %>%
    update_role(text, new_role = "predictor") %>%
    step_untokenize(text) %>%
    update_role(text, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = tokenized_test_data, verbose = FALSE)

  expect_error(bake(trained, new_data = tokenized_test_data[, -1]),
    class = "new_data_missing_column"
  )
})

test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_untokenize(text)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_untokenize(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_untokenize(rec)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), value = character(), id = character())
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), value = character(), id = character())
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_untokenize(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
