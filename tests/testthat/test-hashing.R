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
  rec <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text) 
  
  obj <- rec %>%
    prep(training = test_data, retain = TRUE)
    
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
  rec <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text, num_terms = 256) %>%
    prep(training = test_data, retain = TRUE)
  
  expect_equal(
    juice(rec) %>%
      select(contains("hash")) %>%
      ncol(),
    256
  )
})

test_that('printing', {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_texthash(text)
  expect_output(print(rec))
  expect_output(prep(rec, training = test_data, verbose = TRUE))
})
