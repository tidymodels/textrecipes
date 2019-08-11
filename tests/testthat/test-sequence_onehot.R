library(recipes)
library(textrecipes)

test_data <- tibble(text = c("I would not eat them here or there.",
                             "I would not eat them anywhere.",
                             "I would not eat green eggs and ham.",
                             "I do not like them, Sam-I-am.")
)

rec <- recipe(~ ., data = test_data)

test_that("sequence encoding is done correctly", {
  seq_length <- max(nchar(test_data$text))
  
  rec <- rec %>%
    step_sequence_onehot(text, length = seq_length)
  
  obj <- rec %>%
    prep(training = test_data, retain = TRUE)
  
  juiced_data <- juice(obj)  
  
  expect_equal(dim(juiced_data), c(nrow(test_data), seq_length))
  
  expect_true(all(vapply(juiced_data, function(x) all(is.numeric(x)), 
                         FUN.VALUE = logical(1))))
  
  expect_equal(dim(tidy(rec, 1)), c(1, 3))
  expect_equal(dim(tidy(obj, 1)), c(length(letters), 3))
})

test_that("custom extraction functions work works", {

  rec <- rec %>%
    step_sequence_onehot(text, key = "I", length = 10)
  
  obj <- rec %>%
    prep(training = test_data, retain = TRUE)
  
  expect_equal(dim(juice(obj)), c(nrow(test_data), 10))
  
  expect_equal(juice(obj)[[1]], c(1, 1, 1, 1))
})

test_that("printing", {
  rec <- rec %>%
    step_sequence_onehot(text)
  expect_output(print(rec))
  expect_output(prep(rec, training = test_data, verbose = TRUE))
})
