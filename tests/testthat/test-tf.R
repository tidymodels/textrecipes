context("test-tf")

library(recipes)
library(textrecipes)

set.seed(1)
data_tf <- tibble(text = purrr::map_chr(1:100, 
                                        ~ paste(sample(letters, 10, TRUE), 
                                                collapse = " ")))
data_rec <- recipe(~ ., data = data_tf)

test_that("step_tf works as intended", {
   data_preped <- data_rec %>%
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
