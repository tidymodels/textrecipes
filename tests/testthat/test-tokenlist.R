library(testthat)
library(vctrs)

## Creation -------------------------------------------------------------------

test_that("tokenlist creation works", {
  # Tokens
  tkn_list <- tokenlist(list(letters, letters))

  expect_s3_class(tkn_list, "textrecipes_tokenlist")
  
  expect_equal(
    fields(tkn_list),
    "tokens"
  )
  
  expect_equal(
    field(tkn_list, "tokens"),
    list(letters, letters)
  )
  
  expect_equal(
    attr(tkn_list, "unique_tokens"),
    letters
  )
  
  # Tokens, lemma
  tkn_list <- tokenlist(list(letters, letters), lemma = list(LETTERS, LETTERS))
  
  expect_s3_class(tkn_list, "textrecipes_tokenlist")
  
  expect_equal(
    fields(tkn_list),
    c("tokens", "lemma")
  )
  
  expect_equal(
    field(tkn_list, "tokens"),
    list(letters, letters)
  )
  
  expect_equal(
    field(tkn_list, "lemma"),
    list(LETTERS, LETTERS)
  )
  
  expect_equal(
    attr(tkn_list, "unique_tokens"),
    letters
  )
  
  # Tokens, pos
  tkn_list <- tokenlist(list(letters, letters), pos = list(LETTERS, LETTERS))
  
  expect_s3_class(tkn_list, "textrecipes_tokenlist")
  
  expect_equal(
    fields(tkn_list),
    c("tokens", "pos")
  )
  
  expect_equal(
    field(tkn_list, "tokens"),
    list(letters, letters)
  )
  
  expect_equal(
    field(tkn_list, "pos"),
    list(LETTERS, LETTERS)
  )
  
  expect_equal(
    attr(tkn_list, "unique_tokens"),
    letters
  )
  
  # Tokens, lemma, pos
  tkn_list <- tokenlist(list(letters, letters), 
                        lemma = list(letters, LETTERS),
                        pos = list(LETTERS, LETTERS))
  
  expect_s3_class(tkn_list, "textrecipes_tokenlist")
  
  expect_equal(
    fields(tkn_list),
    c("tokens", "lemma", "pos")
  )
  
  expect_equal(
    field(tkn_list, "tokens"),
    list(letters, letters)
  )
  
  expect_equal(
    field(tkn_list, "lemma"),
    list(letters, LETTERS)
  )
  
  expect_equal(
    field(tkn_list, "pos"),
    list(LETTERS, LETTERS)
  )
  
  expect_equal(
    attr(tkn_list, "unique_tokens"),
    letters
  )
})

test_that("tokenlist errors with diffent length input", {
  expect_error(
    tokenlist(list(letters), lemma = list(letters, letters))
  )
  
  expect_error(
    tokenlist(list(letters), pos = list(letters, letters))
  )
})

test_that("new_tokenlist errors with wrong input", {
  expect_error(
    new_tokenlist(letters)
  )
  
  expect_error(
    new_tokenlist(list(letters), lemma = letters),
    "`lemma` must be NULL or a list."
  )
  
  expect_error(
    new_tokenlist(list(letters), pos = letters),
    "`pos` must be NULL or a list."
  )
})

## Subsetting -----------------------------------------------------------------

test_that("subsetting works as intended", {
  data <- list(letters, LETTERS)

  expect_length(tokenlist(data)[1:2], 2)
  expect_length(tokenlist(data)[2], 1)
  expect_error(tokenlist(data)[3], class = "vctrs_error_subscript_oob")
  expect_equal(tokenlist(data)[0], tokenlist(list()))
})

test_that("subsetting respects lemma", {
  
  data <- list(c("hello", "there"),
               c("No"),
               character(0))

  lemma_tokenlist <- tokenlist(data, data)
  
  expect_equal(
    lemma_tokenlist[1],
    tokenlist(data[1], data[1])
  )
})

test_that("subsetting respects pos", {
  
  data <- list(c("hello", "there"),
               c("No"),
               character(0))
  
  pos_tokenlist <- tokenlist(data, pos = data)
  
  expect_equal(
    pos_tokenlist[1],
    tokenlist(data[1], pos = data[1])
  )
})

## Tokenfilter ----------------------------------------------------------------
test_that("tokenlist_filter works", {
  tkn_list <- tokenlist(list(letters, character()))
  
  expect_equal(
    tokenlist_filter(tkn_list, letters[1:10]),
    tokenlist(list(letters[-(1:10)], character()))
  )
  
  expect_equal(
    tokenlist_filter(tkn_list, "hello"),
    tkn_list
  )
  
  expect_equal(
    tokenlist_filter(tkn_list, letters),
    tokenlist(list(character(), character()))
  )
  
  
  expect_equal(
    tokenlist_filter(tkn_list, letters[1:10], keep = TRUE),
    tokenlist(list(letters[1:10], character()))
  )
  
  expect_equal(
    tokenlist_filter(tkn_list, "hello", keep = TRUE),
    new_tokenlist(list(character(), character()), unique_tokens = "hello")
  )
  
  expect_equal(
    tokenlist_filter(tkn_list, letters, keep = TRUE),
    tkn_list
  )
  
  expect_error(
   tokenlist_filter(LETTERS, letters)
  )
})

test_that("tokenlist_filter respects lemma", {
  
  data <- list(c("hello", "there"),
               c("No"),
               character(0))
  lemma <- list(1:2, 1, numeric(0))
  
  lemma_tokenlist <- tokenlist(data, lemma)
  
  expect_equal(
    tokenlist_filter(x = lemma_tokenlist, dict = "hello"),
    tokenlist(list("there", "No", character()), list(2, 1, numeric(0)))
  )
  
  expect_equal(
    tokenlist_filter(x = lemma_tokenlist, dict = "hello", keep = TRUE),
    tokenlist(list("hello", character(), character()), 
              list(1, numeric(0), numeric(0)))
  )
})

test_that("tokenlist_filter respects pos", {
  
  data <- list(c("hello", "there"),
               c("No"),
               character(0))
  pos <- list(1:2, 1, numeric(0))
  
  pos_tokenlist <- tokenlist(data, pos = pos)
  
  expect_equal(
    tokenlist_filter(pos_tokenlist, "hello"),
    tokenlist(list("there", "No", character()), pos = list(2, 1, numeric(0)))
  )
  
  expect_equal(
    tokenlist_filter(x = pos_tokenlist, dict = "hello", keep = TRUE),
    tokenlist(list("hello", character(), character()), 
              pos = list(1, numeric(0), numeric(0)))
  )
})

## tokenlist_apply ------------------------------------------------------------
test_that("tokenlist_apply works", {
  tkn_list <- tokenlist(list(letters, letters[1:5], LETTERS),
                        list(letters, letters[1:5], LETTERS),
                        list(letters, letters[1:5], LETTERS))

  expect_equal(
    tokenlist_apply(tkn_list, toupper),
    tokenlist(list(LETTERS, LETTERS[1:5], LETTERS))
  )
  
  letter_filter <- function(x, let = "a") {
    x[x == let]
  }
  
  expect_equal(
    tokenlist_apply(tkn_list, letter_filter),
    tokenlist(list("a", "a", character()))
  )
  
  expect_equal(
    tokenlist_apply(tkn_list, letter_filter, list(let = "D")),
    tokenlist(list(character(), character(), "D"))
  )
  
  expect_error(
    tokenlist_apply(tkn_list, letter_filter, let = "D"),
    "unused argument"
  )
  
  expect_error(
    tokenlist_apply(letters, toupper),
    "Input must be a tokenlist"
  )
})

## tokenlist_to_dtm -----------------------------------------------------------
test_that("tokenlist_to_dtm works", {
  tkn_list <- tokenlist(list(c("a", "b", "c"), "b", "c"))
  
  expect_equal(
    tokenlist_to_dtm(tkn_list, get_unique_tokens(tkn_list)),
    Matrix::sparseMatrix(i = c(1, 1, 1, 2, 3), j = c(1, 2, 3, 2, 3), 
                         dimnames = list(NULL, c("a", "b", "c")), x = 1)
  )
  
  expect_equal(
    tokenlist_to_dtm(tkn_list, c("a", "b")),
    Matrix::sparseMatrix(i = c(1, 1, 2), j = c(1, 2, 2), dims = c(3, 2),
                         dimnames = list(NULL, c("a", "b")), x = 1)
  )
  
  expect_equal(
    tokenlist_to_dtm(tkn_list, character()),
    {
      ref_mat <- Matrix::sparseMatrix(i = NULL, j = NULL, dims = c(3, 0),
                                      dimnames = list(NULL, character()), x = 1)
      
      ref_mat@Dimnames[[2]] <- character()
      ref_mat
    }
  )
  
  expect_equal(
    tokenlist_to_dtm(tkn_list, letters),
    Matrix::sparseMatrix(i = c(1, 1, 1, 2, 3), j = c(1, 2, 3, 2, 3), 
                         dims = c(3, 26),
                         dimnames = list(NULL, letters), x = 1)
  )
  
  expect_error(
    tokenlist_to_dtm(tkn_list),
    "is missing"
  )
  
  expect_error(
    tokenlist_to_dtm(letters),
    "Input must be a tokenlist"
  )
})

## tokenlist_lemma ------------------------------------------------------------
test_that("tokenlist_lemma works", {
  tkn_list <- tokenlist(list(letters, letters), 
                        lemma = list(letters, LETTERS),
                        pos = list(LETTERS, LETTERS))

  expect_equal(
    tokenlist_lemma(tkn_list),
    tokenlist(list(letters, LETTERS), 
              pos = list(LETTERS, LETTERS))
  )
  
  expect_error(
    tokenlist_lemma(letters),
    "Input must be a tokenlist"
  )
  
  expect_error(
    tokenlist_lemma(tokenlist(list(letters))),
    "attribute not avaliable"
  )
})

## tokenlist_pos_filter -------------------------------------------------------
test_that("tokenlist_pos_filter works", {
  data <- list(c("hello", "there"),
               c("dog"),
               character(0))
  pos <- list(c("INTJ", "ADV"), "NOUN", character(0))
  
  pos_tokenlist <- tokenlist(data, pos = pos)
  
  expect_equal(
    tokenlist_pos_filter(x = pos_tokenlist, pos_tags = c("INTJ", "NOUN")),
    tokenlist(list("hello", "dog", character()), 
              pos = list("INTJ", "NOUN", character()))
  )
  
  expect_equal(
    tokenlist_pos_filter(pos_tokenlist, "NOUN"),
    tokenlist(list(character(), "dog", character()), 
              pos = list(character(), "NOUN", character()))
  )
  
  expect_equal(
    tokenlist_pos_filter(pos_tokenlist, character()),
    tokenlist(list(character(), character(), character()), 
              pos = list(character(), character(), character()))
  )
  
  tkn_list <- tokenlist(list(letters), list(letters), list(letters))
  
  expect_equal(
    tokenlist_pos_filter(tkn_list, letters[1:2]),
    tokenlist(list(letters[1:2]), list(letters[1:2]), list(letters[1:2]))
  )
  
  expect_error(
    tokenlist_pos_filter(letters, "NOUN"),
    "must be a tokenlist"
  )
  expect_error(
    tokenlist_pos_filter(tokenlist(data), "NOUN"),
    "pos attribute not avaliable"
  )
})

## tokenlist_ngram ------------------------------------------------------------
test_that("tokenlist_ngram works", {
  data <- list(c("not", "eat", "them", "here", "or", "there."),
               c("not", "eat", "them", "anywhere."),
               character(0))
  
  pos_tokenlist <- tokenlist(data)
  
  ngrams <- tokenlist_ngram(pos_tokenlist, 3, 3, "_")
  
  expect_s3_class(ngrams, "textrecipes_tokenlist")
  
  
  expect_equal(
    vctrs::field(ngrams, "tokens"),
    list(c("not_eat_them", "eat_them_here", "them_here_or", "here_or_there."),
         c("not_eat_them", "eat_them_anywhere."),
         character())
  )
  
  expect_null(attr(ngrams, "lemma"))
  expect_null(attr(ngrams, "pos"))
})

test_that("tokenlist_ngram works with n_min and n", {
  tokens <- list(c("a", "b", "c", "d", "e"),
                 c("a", "b"),
                 c("a"),
                 character(0))
  
  tknlist <- tokenlist(tokens)
  
  expect_equal(
    get_tokens(tokenlist_ngram(tknlist, 1, 1, " ")),
    tokens
  )
  
  expect_equal(
    get_tokens(tokenlist_ngram(tknlist, 2, 2, " ")),
    list(c("a b", "b c", "c d", "d e"),
         c("a b"),
         character(0),
         character(0))
  )
  
  expect_equal(
    get_tokens(tokenlist_ngram(tknlist, 3, 3, " ")),
    list(c("a b c", "b c d", "c d e"),
         character(0),
         character(0),
         character(0))
  )
  
  expect_equal(
    get_tokens(tokenlist_ngram(tknlist, 2, 1, " ")),
    list(c("a", "b", "c", "d", "e", "a b", "b c", "c d", "d e"),
         c("a", "b", "a b"),
         c("a"),
         character(0))
  )
  
  expect_equal(
    get_tokens(tokenlist_ngram(tknlist, 3, 1, " ")),
    list(c("a", "b", "c", "d", "e", "a b", "b c", "c d", "d e", "a b c", "b c d", "c d e"),
         c("a", "b", "a b"),
         c("a"),
         character(0))
  )
  
  expect_equal(
    get_tokens(tokenlist_ngram(tknlist, 3, 2, " ")),
    list(c("a b", "b c", "c d", "d e", "a b c", "b c d", "c d e"),
         c("a b"),
         character(0),
         character(0))
  )
})

test_that("tokenlist_ngram errors", {
  expect_error(
    tokenlist_ngram(letters),
    "Input must be a tokenlist"
  )
  
  data <- list(c("not", "eat", "them", "here", "or", "there."),
               c("not", "eat", "them", "anywhere."),
               character(0))
  
  expect_error(
    tokenlist_ngram(tokenlist(data), 0, 3, " "),
    "'n' must be a positive integer."
  )
  
  expect_error(
    tokenlist_ngram(tokenlist(data), 3, 0, " "),
    "'n_min' must be a positive integer."
  )
  
  expect_error(
    tokenlist_ngram(tokenlist(data), 1, 2, " "),
    "'n_min' must be larger then 'n'."
  ) 
  
})
  
  
  
  
  