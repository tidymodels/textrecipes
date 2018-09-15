context("test-tokenize")

library(recipes)
library(textrecipes)

data(okc_text)
okc_rec <- recipe(~ ., data = okc_text)


test_that("output is list when length is 1", {
  data <- tibble(a = rep("a", 20))
  
  data_rec <- recipe(~ ., data = data) %>%
    step_tokenize(a) %>%
    prep(training = data, retain = TRUE)
  
  expect_true(is.list(juice(data_rec, a)[, 1, drop = TRUE]))
})

test_that("tokenization is done correctly", {
  okc_rec <- okc_rec %>%
    step_tokenize(essay0) %>%
    prep(training = okc_text, retain = TRUE)
  
  expect_equal(
    tokenizers::tokenize_words(okc_text$essay0[1]),
    juice(okc_rec) %>% 
      slice(1) %>% 
      pull(essay0)
  )
})