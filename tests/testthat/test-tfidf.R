context("test-tfidf")

library(recipes)
library(textrecipes)

set.seed(1)
data_tf <- tibble(text = purrr::map_chr(1:100, 
                                        ~ paste(sample(letters, 10, TRUE), 
                                                collapse = " ")))
data_rec <- recipe(~ ., data = data_tf)

test_that("step_tfidf works as intended", {
  data_preped <- data_rec %>%
    step_tokenize(text) %>%
    step_tfidf(text) %>%
    prep(training = data_tf, retain = TRUE)
  
  # Reference calcutation
  tokenized <- tokenizers::tokenize_characters(as.list(unlist(data_tf)))
  tokenized_factor <- lapply(tokenized, factor, letters)
  counts <- lapply(tokenized_factor, tabulate, 26) %>% 
    purrr::reduce(rbind) %>% 
    unname()
  tf <- counts
  N <- nrow(counts)
  idf <- log(N / (colSums(counts > 0) + 1))
  
  expect_equal(
    juice(data_preped) %>% as.matrix() %>% unname(),
    t(t(tf) * idf)
  )
})
