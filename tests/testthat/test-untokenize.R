context("test-untokenize")

library(recipes)
library(textrecipes)

data(okc_text)
rec <- recipe(~ ., data = okc_text)

test_that("output is not a list", {
  data <- tibble(a = rep("a", 20))
  
  rec <- recipe(~ ., data = data) %>%
    step_tokenize(a) %>%
    step_untokenize(a) 
  
  obj <- rec %>%
    prep(training = data, retain = TRUE)
  
  expect_true(is.factor(juice(obj, a)[, 1, drop = TRUE]))
  
  expect_equal(dim(tidy(rec, 2)), c(1, 2))
  expect_equal(dim(tidy(obj, 2)), c(1, 2))
})

test_that('printing', {
  rec <- rec %>%
    step_tokenize(essay0) %>%
    step_untokenize(essay0)
  expect_output(print(rec))
  expect_output(prep(rec, training = okc_text, verbose = TRUE))
})
