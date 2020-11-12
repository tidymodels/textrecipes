library(textrecipes)
library(recipes)
library(tibble)

text <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

test_that("lemmatization works", {
  skip_on_cran()
  skip_if_no_python_or_no_spacy()

  rec <- recipe(~text, data = text) %>%
    step_tokenize(all_predictors(), engine = "spacyr") %>%
    step_lemma(all_predictors())

  prepped_data <- rec %>%
    prep() %>%
    bake(new_data = NULL)

  expect_s3_class(prepped_data$text, "textrecipes_tokenlist")

  expect_equal(
    vctrs::field(prepped_data$text, "tokens"),
    list(
      c("-PRON-", "would", "not", "eat", "-PRON-", "here", "or", "there", "."),
      c("-PRON-", "would", "not", "eat", "-PRON-", "anywhere", "."),
      c("-PRON-", "would", "not", "eat", "green", "egg", "and", "ham", "."),
      c("-PRON-", "do", "not", "like", "-PRON-", ",", "sam", "-", "i", "-", "am", ".")
    )
  )

  expect_null(
    attr(prepped_data$text, "lemma")
  )
})

test_that("lemmatization errors if lemma attribute doesn't exists", {
  rec <- recipe(~text, data = text) %>%
    step_tokenize(all_predictors()) %>%
    step_lemma(all_predictors())

  expect_error(
    prep(rec)
  )
})


test_that("printing", {
  skip_on_cran()
  skip_if_no_python_or_no_spacy()
  rec <- recipe(~text, data = text) %>%
    step_tokenize(all_predictors(), engine = "spacyr") %>%
    step_lemma(all_predictors())

  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})
