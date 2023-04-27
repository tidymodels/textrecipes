library(textrecipes)
library(recipes)

text1 <- c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
)

text2 <- c(
  "You would not eat them here or there.",
  "You would not eat them anywhere.",
  "You would not eat green eggs and ham.",
  "You do not like them, Sam-I-am."
)

test_data <- tibble(text1, text2)

text1_out <- list(
  c("i", "would", "not", "eat", "them", "here", "or", "there", "."),
  c("i", "would", "not", "eat", "them", "anywhere", "."),
  c("i", "would", "not", "eat", "green", "eggs", "and", "ham", "."),
  c("i", "do", "not", "like", "them", ",", "sam", "-", "i", "-", "am", ".")
)

text2_out <- list(
  c("you", "would", "not", "eat", "them", "here", "or", "there", "."),
  c("you", "would", "not", "eat", "them", "anywhere", "."),
  c("you", "would", "not", "eat", "green", "eggs", "and", "ham", "."),
  c("you", "do", "not", "like", "them", ",", "sam", "-", "i", "-", "am", ".")
)

test_that("step_tokenize_wordpiece works", {
  res <- recipe(~text1, data = test_data) %>%
    step_tokenize_wordpiece(text1) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    vctrs::field(res$text1, "tokens"),
    text1_out
  )
})

test_that("step_tokenize_wordpiece works with tokenizers.wordpiece and multiple colunms", {
  res <- recipe(~., data = test_data) %>%
    step_tokenize_wordpiece(all_predictors()) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    vctrs::field(res$text1, "tokens"),
    text1_out
  )

  expect_equal(
    vctrs::field(res$text2, "tokens"),
    text2_out
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~ text1 + text2, data = test_data) %>%
    step_tokenize_wordpiece(text1, text2) %>%
    update_role(text1, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)
  
  trained <- prep(rec, training = test_data, verbose = FALSE)
  
  expect_error(bake(trained, new_data = test_data[, -1]),
               class = "new_data_missing_column"
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tokenize_wordpiece(rec)
  
  expect_snapshot(rec)
  
  rec <- prep(rec, mtcars)
  
  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_tokenize_wordpiece(rec1)
  
  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)
  
  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)
  
  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tokenize_wordpiece(rec)
  
  expect <- tibble(terms = character(), id = character())
  
  expect_identical(tidy(rec, number = 1), expect)
  
  rec <- prep(rec, mtcars)
  
  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = test_data) %>%
    step_tokenize_wordpiece(text1)
  
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})