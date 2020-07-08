library(testthat)
library(recipes)
library(tibble)

context("text normalization")

ex_dat <- data.frame(text = c("sch\U00f6n", "scho\U0308n"))

test_that('simple sqrt trans', {
  skip_if_not_installed("stringi")
  
  rec <- recipe(~ ., data = ex_dat) %>%
    step_text_normalization(text)
  
  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)
  
  exp_res <- as_tibble(lapply(ex_dat, stringi::stri_trans_nfc))
  expect_equal(rec_trans$text, factor(exp_res$text))
  
})


test_that('printing', {
  skip_if_not_installed("stringi")
  
  rec <- recipe(~., data = ex_dat) %>%
    step_text_normalization(text)
  expect_output(print(rec))
  expect_output(prep(rec, training = ex_dat, verbose = TRUE))
})
