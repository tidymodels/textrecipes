
test_data <- list(c("not", "eat", "them", "here", "or", "there."),
                  c("not", "eat", "them", "anywhere."))

test_that("ngram works with varrying number of `n`", {
  
  expect_equal(
    rcpp_ngram(test_data, n = 1L, n_min = 1, delim = "_"),
    test_data
  )
  
  expect_equal(
    rcpp_ngram(test_data, n = 2L, n_min = 2L, delim = "_"),
    list(c("not_eat", "eat_them", "them_here", "here_or", "or_there."),
         c("not_eat", "eat_them", "them_anywhere."))
  )
  
  expect_equal(
    rcpp_ngram(test_data, n = 3L, n_min = 3L, delim = "_"),
    list(c("not_eat_them", "eat_them_here", "them_here_or", "here_or_there."),
         c("not_eat_them", "eat_them_anywhere."))
  )
  
  expect_error(
    rcpp_ngram(test_data, n = 0L, n_min = 0L, delim = "_")
  )
  
  expect_error(
    rcpp_ngram(test_data, n = -1L, n_min = -1L, delim = "_")
  )
})


test_that("tokenlist_ngram works with n_min and n", {
  tknlist <- list(c("a", "b", "c", "d", "e"),
                 c("a", "b"),
                 c("a"),
                 character(0))

  expect_equal(
    rcpp_ngram(tknlist, 1, 1, " "),
    tknlist
  )
  
  expect_equal(
    rcpp_ngram(tknlist, 2, 2, " "),
    list(c("a b", "b c", "c d", "d e"),
         c("a b"),
         character(0),
         character(0))
  )
  
  expect_equal(
    rcpp_ngram(tknlist, 3, 3, " "),
    list(c("a b c", "b c d", "c d e"),
         character(0),
         character(0),
         character(0))
  )
  
  expect_equal(
    rcpp_ngram(tknlist, 2, 1, " "),
    list(c("a", "b", "c", "d", "e", "a b", "b c", "c d", "d e"),
         c("a", "b", "a b"),
         c("a"),
         character(0))
  )
  
  expect_equal(
    rcpp_ngram(tknlist, 3, 1, " "),
    list(c("a", "b", "c", "d", "e", "a b", "b c", "c d", "d e", "a b c", "b c d", "c d e"),
         c("a", "b", "a b"),
         c("a"),
         character(0))
  )
  
  expect_equal(
    rcpp_ngram(tknlist, 3, 2, " "),
    list(c("a b", "b c", "c d", "d e", "a b c", "b c d", "c d e"),
         c("a b"),
         character(0),
         character(0))
  )
})


test_that("ngram works with delim", {
  
  expect_equal(
    rcpp_ngram(test_data, n = 3L, n_min = 3L, delim = ""),
    list(c("noteatthem", "eatthemhere", "themhereor", "hereorthere."),
         c("noteatthem", "eatthemanywhere."))
  )
  
  expect_equal(
    rcpp_ngram(test_data, n = 3L, n_min = 3L, delim = " "),
    list(c("not eat them", "eat them here", "them here or", "here or there."),
         c("not eat them", "eat them anywhere."))
  )
})

test_that("ngram returns length zero vectors when length(x) < n", {
  
  expect_equal(
    rcpp_ngram(list(c("a", "b")), n = 3L, n_min = 3L, delim = "_"),
    list(character())
  )
  
  expect_equal(
    rcpp_ngram(list(c("a")), n = 3L, n_min = 3L, delim = "_"),
    list(character())
  )
  
  expect_equal(
    rcpp_ngram(list(character()), n = 3L, n_min = 3L, delim = "_"),
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
    step_ngram(text, num_tokens = 3, min_num_tokens = 3) 
  
  obj <- rec %>%
    prep()
  
  expect_equal(
    juice(obj) %>% 
      pull(text) %>%
      vctrs::field("tokens"),
    list(c("not_eat_them", "eat_them_here", "them_here_or", "here_or_there"),
         c("not_eat_them", "eat_them_anywhere"))
  )
  
  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 2))
})

test_that("`n` argument works", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_ngram(text, num_tokens = 2, min_num_tokens = 2) 
  
  obj <- rec %>%
    prep()
  
  expect_equal(
    juice(obj) %>% 
      pull(text) %>%
      vctrs::field("tokens"),
    list(c("not_eat", "eat_them", "them_here", "here_or", "or_there"),
         c("not_eat", "eat_them", "them_anywhere"))
  )
})

test_that("ngramming works with min_num_tokens", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_ngram(text, num_tokens = 3, min_num_tokens = 1) 
  
  obj <- rec %>%
    prep()
  
  expect_equal(
    juice(obj) %>% 
      pull(text) %>%
      vctrs::field("tokens"),
    list(c("not", "eat", "them", "here", "or", "there", 
           "not_eat", "eat_them", "them_here", "here_or", "or_there",
           "not_eat_them", "eat_them_here", "them_here_or", "here_or_there"),
         c("not", "eat", "them", "anywhere",
           "not_eat", "eat_them", "them_anywhere",
           "not_eat_them", "eat_them_anywhere"))
  )
  
  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 2))
})

test_that("`delim` argument works", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_ngram(text, delim = " ", num_tokens = 3, min_num_tokens = 3) 
  
  obj <- rec %>%
    prep()
  
  expect_equal(
    juice(obj) %>% 
      pull(text) %>%
      vctrs::field("tokens"),
    list(c("not eat them", "eat them here", "them here or", "here or there"),
         c("not eat them", "eat them anywhere"))
  )
})


test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_ngram(text)
  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})

test_that("tunable", {
  rec <-
    recipe(~ ., data = iris) %>%
    step_ngram(all_predictors())
  rec_param <- tunable.step_ngram(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_tokens"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})
