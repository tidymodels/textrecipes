library(recipes)
library(textrecipes)

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec <- recipe(~., data = test_data)

test_that("stemming is done correctly", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_stem(text)

  obj <- rec %>%
    prep()

  expect_equal(
    tokenizers::tokenize_words(test_data$text[1])[[1]] %>%
      SnowballC::wordStem(),
    bake(obj, new_data = NULL) %>%
      slice(1) %>%
      pull(text) %>%
      vctrs::field("tokens") %>%
      unlist()
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 3))
})

test_that("custom stemmer works", {
  custom_stem_fun <- function(x) substr(x, 1, 2)

  rec <- rec %>%
    step_tokenize(text) %>%
    step_stem(text, custom_stemmer = custom_stem_fun)

  obj <- rec %>%
    prep()

  expect_equal(
    tokenizers::tokenize_words(test_data$text[1])[[1]] %>%
      custom_stem_fun(),
    bake(obj, new_data = NULL) %>%
      slice(1) %>%
      pull(text) %>%
      vctrs::field("tokens") %>%
      unlist()
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 3))
})

test_that("arguments are passed by options", {
  data <- tibble(y = 0, text = "коты")

  expect_equal(
    recipe(y ~ ., data = data) %>%
      step_tokenize(text) %>%
      step_stem(text, options = list(language = "russian")) %>%
      prep(data) %>%
      bake(new_data = NULL) %>%
      pull(text) %>%
      vctrs::field("tokens"),
    list("кот")
  )
})

test_that("bake method errors when needed non-standard role columns are missing", {
  tokenized_test_data <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    prep() %>%
    bake(new_data = NULL)
  
  rec <- recipe(tokenized_test_data) %>%
    update_role(text, new_role = "predictor") %>%
    step_stem(text) %>%
    update_role(text, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)
  
  trained <- prep(rec, training = tokenized_test_data, verbose = FALSE)
  
  expect_error(bake(trained, new_data = tokenized_test_data[, -1]),
               class = "new_data_missing_column")
})

test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_stem(text)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_stem(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_stem(rec)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), is_custom_stemmer = logical(), id = character())
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), is_custom_stemmer = logical(), id = character())
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_stem(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
