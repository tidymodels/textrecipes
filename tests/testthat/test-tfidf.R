context("test-tfidf")

library(recipes)
library(textrecipes)

set.seed(1)
data_tf <- tibble(text = purrr::map_chr(1:100, 
                                        ~ paste(sample(letters, 10, TRUE), 
                                                collapse = " ")))
rec <- recipe(~ ., data = data_tf)

test_that("step_tfidf works as intended", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tfidf(text) 
  
  obj <- rec %>%
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
    juice(obj) %>% as.matrix() %>% unname(),
    t(t(tf) * idf)
  )
  
  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 3))
})

test_that('printing', {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tfidf(text)
  expect_output(print(rec))
  expect_output(prep(rec, training = data_tf, verbose = TRUE))
})
