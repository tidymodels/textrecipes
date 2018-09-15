context("test-stem")

library(recipes)
library(textrecipes)

data(okc_text)
okc_rec <- recipe(~ ., data = okc_text)

test_that("stemming is done correctly", {
  okc_rec <- okc_rec %>%
    step_tokenize(essay0) %>%
    step_stem(essay0) %>%
    prep(training = okc_text, retain = TRUE)
  
  expect_equal(
    tokenizers::tokenize_words(okc_text$essay0[1])[[1]] %>%
      SnowballC::wordStem(),
    juice(okc_rec) %>% 
      slice(1) %>% 
      pull(essay0) %>%
      unlist()
  )
})
