context("test-hashing")

library(recipes)
library(textrecipes)

data(okc_text)
rec <- recipe(~ ., data = okc_text)

test_that("hashing gives integer outputs", {
  rec <- rec %>%
    step_tokenize(essay0) %>%
    # This step is to speed up calculations for faster test
    step_textfilter(essay0, max.words = 20) %>%
    step_hashing(essay0) %>%
    prep(training = okc_text, retain = TRUE)
    
  expect_true(
    juice(rec) %>%
      select(contains("hashing")) %>%
      lapply(is.integer) %>%
      unlist() %>%
      all()
    )
})

test_that("hashing output width changes accordingly with num", {
  rec <- rec %>%
    step_tokenize(essay0) %>%
    # This step is to speed up calculations for faster test
    step_textfilter(essay0, max.words = 20) %>%
    step_hashing(essay0, num = 256) %>%
    prep(training = okc_text, retain = TRUE)
  
  expect_equal(
    juice(rec) %>%
      select(contains("hashing")) %>%
      ncol(),
    256
  )
})

test_that('printing', {
  rec <- rec %>%
    step_tokenize(essay0) %>%
    # This step is to speed up calculations for faster test
    step_textfilter(essay0, max.words = 20) %>%
    step_hashing(essay0)
  expect_output(print(rec))
  expect_output(prep(rec, training = okc_text, verbose = TRUE))
})
