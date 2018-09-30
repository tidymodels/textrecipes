context("test-stem")

library(recipes)
library(textrecipes)

data(okc_text)
rec <- recipe(~ ., data = okc_text)

test_that("stemming is done correctly", {
  rec <- rec %>%
    step_tokenize(essay0) %>%
    step_stem(essay0) %>%
    prep(training = okc_text, retain = TRUE)
  
  expect_equal(
    tokenizers::tokenize_words(okc_text$essay0[1])[[1]] %>%
      SnowballC::wordStem(),
    juice(rec) %>% 
      slice(1) %>% 
      pull(essay0) %>%
      unlist()
  )
})

test_that('printing', {
  rec <- rec %>%
    step_tokenize(essay0) %>%
    step_stem(essay0)
  expect_output(print(rec))
  expect_output(prep(rec, training = okc_text, verbose = TRUE))
})
