library(textrecipes)
library(recipes)
library(tibble)

text <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

test_that("part of speech filtering works", {
  skip_on_cran()
  skip_if_no_python_or_no_spacy()

  rec <- recipe(~text, data = text) %>%
    step_tokenize(all_predictors(), engine = "spacyr") %>%
    step_pos_filter(all_predictors())

  prepped_data <- rec %>%
    prep() %>%
    bake(new_data = NULL)

  expect_s3_class(prepped_data$text, "textrecipes_tokenlist")

  expect_equal(
    vctrs::field(prepped_data$text, "tokens"),
    list(
      character(),
      character(),
      c("eggs", "ham"),
      c("am")
    )
  )
})

test_that("part of speech filtering removes everything", {
  skip_on_cran()
  skip_if_no_python_or_no_spacy()

  rec <- recipe(~text, data = text) %>%
    step_tokenize(all_predictors(), engine = "spacyr") %>%
    step_pos_filter(all_predictors(), keep_tags = character())

  prepped_data <- rec %>%
    prep() %>%
    bake(new_data = NULL)

  expect_s3_class(prepped_data$text, "textrecipes_tokenlist")

  expect_equal(
    vctrs::field(prepped_data$text, "tokens"),
    list(
      character(),
      character(),
      character(),
      character()
    )
  )
})

test_that("part of speech filtering works with multiple tags", {
  skip_on_cran()
  skip_if_no_python_or_no_spacy()

  rec <- recipe(~text, data = text) %>%
    step_tokenize(all_predictors(), engine = "spacyr") %>%
    step_pos_filter(all_predictors(), keep_tags = c("VERB", "NOUN"))

  prepped_data <- rec %>%
    prep() %>%
    bake(new_data = NULL)

  expect_s3_class(prepped_data$text, "textrecipes_tokenlist")

  expect_equal(
    vctrs::field(prepped_data$text, "tokens"),
    list(
      c("would", "eat"),
      c("would", "eat"),
      c("would", "eat", "eggs", "ham"),
      c("do", "like", "am")
    )
  )
})

test_that("lemmatization errors if lemma attribute doesn't exists", {
  rec <- recipe(~text, data = text) %>%
    step_tokenize(all_predictors()) %>%
    step_pos_filter(all_predictors())

  expect_error(
    prep(rec)
  )
})

test_that("printing", {
  skip_on_cran()
  skip_if_no_python_or_no_spacy()
  rec <- recipe(~text, data = text) %>%
    step_tokenize(all_predictors(), engine = "spacyr") %>%
    step_pos_filter(all_predictors())

  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})
