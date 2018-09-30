context("test-tf")

library(recipes)
library(textrecipes)

set.seed(1)
data_tf <- tibble(text = purrr::map_chr(1:100, 
                                        ~ paste(sample(letters, 10, TRUE), 
                                                collapse = " ")))
rec <- recipe(~ ., data = data_tf)

test_that("step_tf works as intended", {
   data_preped <- rec %>%
     step_tokenize(text) %>%
     step_tf(text) %>%
     prep(training = data_tf, retain = TRUE)
   
   # Reference calcutation
   tokenized <- tokenizers::tokenize_characters(as.list(unlist(data_tf)))
   tokenized_factor <- lapply(tokenized, factor, letters)
   
   expect_equal(
     juice(data_preped) %>% as.matrix() %>% unname(),
     lapply(tokenized_factor, tabulate, 26) %>% purrr::reduce(rbind) %>% unname()
     )
})

test_that('printing', {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text)
  expect_output(print(rec))
  expect_output(prep(rec, training = data_tf, verbose = TRUE))
})
