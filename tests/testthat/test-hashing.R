context("test-hashing")

library(textrecipes)
library(recipes)

test_data <- tibble(text = c("I would not eat them here or there.",
                             "I would not eat them anywhere.",
                             "I would not eat green eggs and ham.",
                             "I do not like them, Sam-I-am.")
)

rec <- recipe(~ ., data = test_data)

test_that("hashing gives double outputs", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text)

  obj <- rec %>%
    prep()

  expect_true(
    juice(obj) %>%
      select(contains("hash")) %>%
      lapply(is.double) %>%
      unlist() %>%
      all()
    )
  
  expect_equal(dim(tidy(rec, 2)), c(1, 4))
  expect_equal(dim(tidy(obj, 2)), c(1, 4))
})

test_that("hashing output width changes accordingly with num_terms", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text, num_terms = 256) %>%
    prep()

  expect_equal(
    juice(rec) %>%
      select(contains("hash")) %>%
      ncol(),
    256
  )
})

test_that("hashing output width changes accordingly with num_terms", {
  skip_if_not_installed("text2vec")
  
  signed <- recipe(~ ., data = test_data) %>%
    step_tokenize(all_predictors()) %>%
    step_texthash(all_predictors(), num_terms = 2) %>%
    prep() %>%
    juice()
  
  unsigned <- recipe(~ ., data = test_data) %>%
    step_tokenize(all_predictors()) %>%
    step_texthash(all_predictors(), num_terms = 2, signed = FALSE) %>%
    prep() %>%
    juice()
  
  all(unsigned$text_hash1 == signed$text_hash1)
  all(unsigned$text_hash2 == signed$text_hash2)
  expect_false(all(unsigned$text_hash1 == signed$text_hash1))
  expect_false(all(unsigned$text_hash2 == signed$text_hash2))
})



test_that("printing", {
  skip_if_not_installed("text2vec")
  rec <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text)
  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})

