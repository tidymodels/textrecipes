context("test-untokenize")

library(recipes)
library(textrecipes)

data(okc_text)
rec <- recipe(~ ., data = okc_text)

test_that("output is not a list", {
  data <- tibble(a = rep("a", 20))
  
  data_rec <- recipe(~ ., data = data) %>%
    step_tokenize(a) %>%
    step_untokenize(a) %>%
    prep(training = data, retain = TRUE)
  
  expect_true(is.factor(juice(data_rec, a)[, 1, drop = TRUE]))
})

test_that('printing', {
  rec <- rec %>%
    step_tokenize(essay0) %>%
    step_untokenize(essay0)
  expect_output(print(rec))
  expect_output(prep(rec, training = okc_text, verbose = TRUE))
})
