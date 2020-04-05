
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


