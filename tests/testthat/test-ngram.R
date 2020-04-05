
test_data <- list(c("not", "eat", "them", "here", "or", "there."),
                  c("not", "eat", "them", "anywhere."))

test_that("ngram works with varrying number of `n`", {
  
  expect_equal(
    rcpp_ngram(test_data, n = 1L, delim = "_"),
    test_data
  )
  
  expect_equal(
    rcpp_ngram(test_data, n = 2L, delim = "_"),
    list(c("not_eat", "eat_them", "them_here", "here_or", "or_there."),
         c("not_eat", "eat_them", "them_anywhere."))
  )
  
  expect_equal(
    rcpp_ngram(test_data, n = 3L, delim = "_"),
    list(c("not_eat_them", "eat_them_here", "them_here_or", "here_or_there."),
         c("not_eat_them", "eat_them_anywhere."))
  )
  
  expect_error(
    rcpp_ngram(test_data, n = 0L, delim = "_")
  )
  
  expect_error(
    rcpp_ngram(test_data, n = -1L, delim = "_")
  )
})

test_that("ngram works with delim", {
  
  expect_equal(
    rcpp_ngram(test_data, n = 3L, delim = ""),
    list(c("noteatthem", "eatthemhere", "themhereor", "hereorthere."),
         c("noteatthem", "eatthemanywhere."))
  )
  
  expect_equal(
    rcpp_ngram(test_data, n = 3L, delim = " "),
    list(c("not eat them", "eat them here", "them here or", "here or there."),
         c("not eat them", "eat them anywhere."))
  )
})

test_that("ngram returns length zero vectors when length(x) < n", {
  
  expect_equal(
    rcpp_ngram(list(c("a", "b")), n = 3L, delim = "_"),
    list(character())
  )
  
  expect_equal(
    rcpp_ngram(list(c("a")), n = 3L, delim = "_"),
    list(character())
  )
  
  expect_equal(
    rcpp_ngram(list(character()), n = 3L, delim = "_"),
    list(character())
  )
})


library(recipes)
library(textrecipes)

test_tibble <- tibble(text = c("not eat them here or there.",
                               "not eat them anywhere.")
)

rec <- recipe(~ ., data = test_tibble)

test_that("ngramming is done correctly", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_ngram(text) 
  
  obj <- rec %>%
    prep(training = test_tibble)
  
  expect_equal(
    juice(obj) %>% 
      pull(text) %>%
      vctrs::vec_data(),
    list(c("not_eat_them", "eat_them_here", "them_here_or", "here_or_there"),
         c("not_eat_them", "eat_them_anywhere"))
  )
  
  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 2))
})

test_that("`n` argument works", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_ngram(text, n = 2) 
  
  obj <- rec %>%
    prep(training = test_tibble)
  
  expect_equal(
    juice(obj) %>% 
      pull(text) %>%
      vctrs::vec_data(),
    list(c("not_eat", "eat_them", "them_here", "here_or", "or_there"),
         c("not_eat", "eat_them", "them_anywhere"))
  )
})

test_that("`delim` argument works", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_ngram(text, delim = " ") 
  
  obj <- rec %>%
    prep(training = test_tibble)
  
  expect_equal(
    juice(obj) %>% 
      pull(text) %>%
      vctrs::vec_data(),
    list(c("not eat them", "eat them here", "them here or", "here or there"),
         c("not eat them", "eat them anywhere"))
  )
})


test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_stem(text)
  expect_output(print(rec))
  expect_output(prep(rec, training = test_tibble, verbose = TRUE))
})


