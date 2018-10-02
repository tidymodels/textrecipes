context("test-tf")

library(recipes)
library(textrecipes)

set.seed(1)
data_tf <- tibble(text = purrr::map_chr(1:100, 
                                        ~ paste(sample(letters, 10, TRUE), 
                                                collapse = " ")))
rec <- recipe(~ ., data = data_tf)

test_that("step_tf works as intended", {
   rec <- rec %>%
     step_tokenize(text) %>%
     step_tf(text) 
   
   obj <- rec %>%
     prep(training = data_tf, retain = TRUE)
   
   # Reference calcutation
   tokenized <- tokenizers::tokenize_characters(as.list(unlist(data_tf)))
   tokenized_factor <- lapply(tokenized, factor, letters)
   
   expect_equal(
     juice(obj) %>% as.matrix() %>% unname(),
     lapply(tokenized_factor, tabulate, 26) %>% purrr::reduce(rbind) %>% unname()
     )
   
   expect_equal(dim(tidy(rec)), c(2, 5))
   expect_equal(dim(tidy(obj)), c(2, 5))
})

test_that('printing', {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text)
  expect_output(print(rec))
  expect_output(prep(rec, training = data_tf, verbose = TRUE))
})
