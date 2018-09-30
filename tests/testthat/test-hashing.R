context("test-hashing")

library(recipes)
library(textrecipes)

data(okc_text)
okc_rec <- recipe(~ ., data = okc_text)

test_that("hashing gives integer outputs", {
  okc_rec <- okc_rec %>%
    step_tokenize(essay0) %>%
    # This step is to speed up calculations for faster test
    step_textfilter(essay0, max.words = 20) %>%
    step_hashing(essay0) %>%
    prep(training = okc_text, retain = TRUE)
    
  expect_true(
    juice(okc_rec) %>%
      select(contains("hashing")) %>%
      lapply(is.integer) %>%
      unlist() %>%
      all()
    )
})

test_that("hashing output width changes accordingly with num", {
  okc_rec <- okc_rec %>%
    step_tokenize(essay0) %>%
    # This step is to speed up calculations for faster test
    step_textfilter(essay0, max.words = 20) %>%
    step_hashing(essay0, num = 256) %>%
    prep(training = okc_text, retain = TRUE)
  
  expect_equal(
    juice(okc_rec) %>%
      select(contains("hashing")) %>%
      ncol(),
    256
  )
})