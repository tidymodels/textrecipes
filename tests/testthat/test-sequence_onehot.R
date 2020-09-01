library(testthat)
library(recipes)
library(textrecipes)

test_data <- tibble(text = c("I would not eat them here or there.",
                             "I would not eat them anywhere.",
                             "I would not eat green eggs and ham.",
                             "I do not like them, Sam-I-am.")
)

rec <- recipe(~ ., data = test_data)

test_that("sequence encoding is done correctly", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_sequence_onehot(text, string_length = 10)
  
  obj <- rec %>%
    prep()
  
  juiced_data <- juice(obj)  
  
  expect_equal(dim(juiced_data), c(nrow(test_data), 10))
  
  expect_true(all(vapply(juiced_data, function(x) all(is.numeric(x)), 
                         FUN.VALUE = logical(1))))
  
  expect_equal(dim(tidy(rec, 2)), 
               c(1, 4))
  
  expect_equal(
    dim(tidy(obj, 2)), 
    c(length(unique(unlist(tokenizers::tokenize_words(test_data$text)))), 4)
  )
})

test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_sequence_onehot(text)
  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})
