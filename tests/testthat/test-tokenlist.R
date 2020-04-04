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

test_that("tokenlist_filter respects lemma", {
  
  data <- list(c("hello", "there"),
               c("No"),
               character(0))
  lemma <- list(1:2, 1, numeric(0))
  
  lemma_tokenlist <- tokenlist(data, lemma)
  
  expect_equal(
    tokenlist_filter(lemma_tokenlist, "hello"),
    tokenlist(list("there", "No", character()), list(2, 1, numeric(0)))
  )
})

test_that("subsetting respects pos", {
  
  data <- list(c("hello", "there"),
               c("No"),
               character(0))
  
  lemma_tokenlist <- tokenlist(data, pos = data)
  
  expect_equal(
    lemma_tokenlist[1],
    tokenlist(data[1], pos = data[1])
  )
})

test_that("tokenlist_filter respects lemma", {
  
  data <- list(c("hello", "there"),
               c("No"),
               character(0))
  pos <- list(1:2, 1, numeric(0))
  
  lemma_tokenlist <- tokenlist(data, pos = pos)
  
  expect_equal(
    tokenlist_filter(lemma_tokenlist, "hello"),
    tokenlist(list("there", "No", character()), pos = list(2, 1, numeric(0)))
  )
})


test_that("tokenlist_pos_filter works", {
  data <- list(c("hello", "there"),
               c("dog"),
               character(0))
  pos <- list(c("INTJ", "ADV"), "NOUN", character(0))
  
  pos_tokenlist <- tokenlist(data, pos = pos)
  
  expect_equal(
    tokenlist_pos_filter(pos_tokenlist, c("INTJ", "NOUN")),
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
})