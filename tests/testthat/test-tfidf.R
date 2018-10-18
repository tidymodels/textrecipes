context("test-tfidf")

library(recipes)
library(textrecipes)

test_data <- tibble(text = c("I would not eat them here or there.",
                             "I would not eat them anywhere.",
                             "I would not eat green eggs and ham.",
                             "I do not like them, Sam-I-am.")
)

rec <- recipe(~ ., data = test_data)

test_that("step_tfidf works as intended", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tfidf(text) 
  
  obj <- rec %>%
    prep(training = test_data, retain = TRUE)
  
  rec_answer <- unname(juice(obj))
  
  manual_answer <- unname(tibble(am =       c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(4 / (1 + 0.5)),
                                 and =      c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(4 / (1 + 0.5)),
                                 anywhere = c(0 / 8, 1 / 6, 0 / 8, 0 / 8) * log(4 / (1 + 0.5)),
                                 do =       c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(4 / (1 + 0.5)),
                                 eat =      c(1 / 8, 1 / 6, 1 / 8, 0 / 8) * log(4 / (3 + 0.5)),
                                 eggs =     c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(4 / (1 + 0.5)),
                                 green =    c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(4 / (1 + 0.5)),
                                 ham =      c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(4 / (1 + 0.5)),
                                 here =     c(1 / 8, 0 / 6, 0 / 8, 0 / 8) * log(4 / (1 + 0.5)),
                                 i =        c(1 / 8, 1 / 6, 1 / 8, 2 / 8) * log(4 / (4 + 0.5)),
                                 like =     c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(4 / (1 + 0.5)),
                                 not =      c(1 / 8, 1 / 6, 1 / 8, 1 / 8) * log(4 / (4 + 0.5)),
                                 or =       c(1 / 8, 0 / 6, 0 / 8, 0 / 8) * log(4 / (1 + 0.5)),
                                 sam =      c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(4 / (1 + 0.5)), 
                                 them =     c(1 / 8, 1 / 6, 0 / 8, 1 / 8) * log(4 / (3 + 0.5)),
                                 there =    c(1 / 8, 0 / 6, 0 / 8, 0 / 8) * log(4 / (1 + 0.5)),
                                 would =    c(1 / 8, 1 / 6, 1 / 8, 0 / 8) * log(4 / (3 + 0.5))))
  
  expect_equal(
    as.matrix(rec_answer),
    as.matrix(manual_answer)
  )
  
  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 3))
})

test_that("step_tfidf works with other weighting schemes", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tfidf(text, weight_scheme = "idf smooth") 
  
  obj <- rec %>%
    prep(training = test_data, retain = TRUE)
  
  rec_answer <- unname(juice(obj))
  
  manual_answer <- unname(tibble(am =       c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(1 + 4 / (1 + 0.5)),
                                 and =      c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(1 + 4 / (1 + 0.5)),
                                 anywhere = c(0 / 8, 1 / 6, 0 / 8, 0 / 8) * log(1 + 4 / (1 + 0.5)),
                                 do =       c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(1 + 4 / (1 + 0.5)),
                                 eat =      c(1 / 8, 1 / 6, 1 / 8, 0 / 8) * log(1 + 4 / (3 + 0.5)),
                                 eggs =     c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(1 + 4 / (1 + 0.5)),
                                 green =    c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(1 + 4 / (1 + 0.5)),
                                 ham =      c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(1 + 4 / (1 + 0.5)),
                                 here =     c(1 / 8, 0 / 6, 0 / 8, 0 / 8) * log(1 + 4 / (1 + 0.5)),
                                 i =        c(1 / 8, 1 / 6, 1 / 8, 2 / 8) * log(1 + 4 / (4 + 0.5)),
                                 like =     c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(1 + 4 / (1 + 0.5)),
                                 not =      c(1 / 8, 1 / 6, 1 / 8, 1 / 8) * log(1 + 4 / (4 + 0.5)),
                                 or =       c(1 / 8, 0 / 6, 0 / 8, 0 / 8) * log(1 + 4 / (1 + 0.5)),
                                 sam =      c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(1 + 4 / (1 + 0.5)), 
                                 them =     c(1 / 8, 1 / 6, 0 / 8, 1 / 8) * log(1 + 4 / (3 + 0.5)),
                                 there =    c(1 / 8, 0 / 6, 0 / 8, 0 / 8) * log(1 + 4 / (1 + 0.5)),
                                 would =    c(1 / 8, 1 / 6, 1 / 8, 0 / 8) * log(1 + 4 / (3 + 0.5))))
  
  expect_equal(
    as.matrix(rec_answer),
    as.matrix(manual_answer)
  )
})

test_that('printing', {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tfidf(text)
  expect_output(print(rec))
  expect_output(prep(rec, training = test_data, verbose = TRUE))
})
