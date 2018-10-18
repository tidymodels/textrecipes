context("test-hashing")

library(recipes)
library(textrecipes)

data(okc_text)
rec <- recipe(~ ., data = okc_text)

test_that("hashing gives integer outputs", {
  rec <- rec %>%
    step_tokenize(essay0) %>%
    # This step is to speed up calculations for faster test
    step_tokenfilter(essay0, max_tokens = 20) %>%
    step_texthash(essay0) 
  
  obj <- rec %>%
    prep(training = okc_text, retain = TRUE)
    
  expect_true(
    juice(obj) %>%
      select(contains("hashing")) %>%
      lapply(is.integer) %>%
      unlist() %>%
      all()
    )
  
  expect_equal(dim(tidy(rec, 3)), c(1, 3))
  expect_equal(dim(tidy(obj, 3)), c(1, 3))
})

test_that("hashing output width changes accordingly with num_terms", {
  rec <- rec %>%
    step_tokenize(essay0) %>%
    # This step is to speed up calculations for faster test
    step_tokenfilter(essay0, max_tokens = 20) %>%
    step_texthash(essay0, num_terms = 256) %>%
    prep(training = okc_text, retain = TRUE)
  
  expect_equal(
    juice(rec) %>%
      select(contains("hash")) %>%
      ncol(),
    256
  )
})

test_that('printing', {
  rec <- rec %>%
    step_tokenize(essay0) %>%
    # This step is to speed up calculations for faster test
    step_tokenfilter(essay0, max_tokens = 20) %>%
    step_texthash(essay0)
  expect_output(print(rec))
  expect_output(prep(rec, training = okc_text, verbose = TRUE))
})
