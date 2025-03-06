test_data <- tibble(
  text = c(
    "I would not eat them here or there.",
    "I would not eat them anywhere.",
    "I would not eat green eggs and ham.",
    "I do not like them, Sam-I-am."
  )
)

rec <- recipe(~., data = test_data)

test_that("stopwords are removed correctly", {
  skip_if_not_installed("stopwords")
  rec <- rec %>%
    step_tokenize(text) %>%
    step_stopwords(text)

  obj <- rec %>%
    prep()

  token_words <- tokenizers::tokenize_words(test_data$text[1])[[1]]

  expect_equal(
    token_words[!is.element(token_words, stopwords::stopwords())],
    bake(obj, new_data = NULL) %>%
      slice(1) %>%
      pull(text) %>%
      vctrs::field("tokens") %>%
      unlist()
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 4))
  expect_equal(dim(tidy(obj, 2)), c(1, 4))
})

test_that("stopwords are kept correctly", {
  skip_if_not_installed("stopwords")
  rec <- rec %>%
    step_tokenize(text) %>%
    step_stopwords(text, keep = TRUE) %>%
    prep()

  token_words <- tokenizers::tokenize_words(test_data$text[1])[[1]]

  expect_equal(
    token_words[is.element(token_words, stopwords::stopwords())],
    bake(rec, new_data = NULL) %>%
      slice(1) %>%
      pull(text) %>%
      vctrs::field("tokens") %>%
      unlist()
  )
})

test_that("custom stopwords are supported", {
  custom_stopwords <- c("i", "not")

  rec <- rec %>%
    step_tokenize(text) %>%
    step_stopwords(text, custom_stopword_source = custom_stopwords) %>%
    prep()

  token_words <- tokenizers::tokenize_words(test_data$text[1])[[1]]

  expect_equal(
    list(
      c("would", "eat", "them", "here", "or", "there"),
      c("would", "eat", "them", "anywhere"),
      c("would", "eat", "green", "eggs", "and", "ham"),
      c("do", "like", "them", "sam", "am")
    ),
    bake(rec, new_data = NULL) %>%
      pull(text) %>%
      vctrs::field("tokens")
  )
})

test_that("bad args", {
  skip_if_not_installed("stopwords")

  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_stopwords(language = -4) %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_stopwords(keep = -4) %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_stopwords(stopword_source = -4) %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_stopwords(custom_stopword_source = 1:10) %>%
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("stopwords")

  tokenized_test_data <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    prep() %>%
    bake(new_data = NULL)

  rec <- recipe(tokenized_test_data) %>%
    update_role(text, new_role = "predictor") %>%
    step_stopwords(text) %>%
    update_role(text, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = tokenized_test_data, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = tokenized_test_data[, -1])
  )
})

test_that("empty printing", {
  skip_if_not_installed("stopwords")

  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_stopwords(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  skip_if_not_installed("stopwords")

  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_stopwords(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  skip_if_not_installed("stopwords")

  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_stopwords(rec)

  expect <- tibble(
    terms = character(),
    value = character(),
    keep = logical(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  skip_if_not_installed("stopwords")
  rec <- rec %>%
    step_tokenize(text) %>%
    step_stopwords(text)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
