context("test-textfilter")

library(recipes)
library(textrecipes)

data(okc_text)
okc_rec <- recipe(~ ., data = okc_text)

test_that("textfilter does nothing if argument are untouched", {
  okc_rec_ref <- okc_rec %>%
    step_tokenize(essay0) %>%
    prep(training = okc_text, retain = TRUE)
  
  okc_rec_test <- okc_rec %>%
    step_tokenize(essay0) %>%
    step_textfilter(essay0) %>%
    prep(training = okc_text, retain = TRUE)
  
  expect_equal(
    juice(okc_rec_ref) %>% pull(essay0),
    juice(okc_rec_test) %>% pull(essay0)
  )
})

test_that("textfilter removes words correctly", {
  okc_rec <- okc_rec %>%
    step_tokenize(essay0) %>%
    step_textfilter(essay0, max.tf = 50, min.tf = 5, max.words = 100) %>%
    prep(training = okc_text, retain = TRUE)
  
  all_words <- tokenizers::tokenize_words(okc_text$essay0) %>% unlist()
  counted_words <- table(all_words)
  counted_words <- counted_words[which(counted_words > 5 & counted_words < 50)]
  kept_words <- names(sort(counted_words, decreasing = TRUE)[1:100])
  
  words <- tokenizers::tokenize_words(okc_text$essay0[1])[[1]]
  
  expect_equal(
    juice(okc_rec) %>% slice(1) %>% pull(essay0) %>% unlist(),
    purrr::keep(words, words %in% kept_words)
  )
})

