library(recipes)
library(textrecipes)

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec <- recipe(~., data = test_data)

test_that("step_tfidf works as intended", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tfidf(text)

  obj <- rec %>%
    prep()

  rec_answer <- unname(as.data.frame(bake(obj, new_data = NULL)))

  manual_answer <- unname(
    data.frame(
      am = c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(1 + 4 / 1),
      and = c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(1 + 4 / 1),
      anywhere = c(0 / 8, 1 / 6, 0 / 8, 0 / 8) * log(1 + 4 / 1),
      do = c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(1 + 4 / 1),
      eat = c(1 / 8, 1 / 6, 1 / 8, 0 / 8) * log(1 + 4 / 3),
      eggs = c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(1 + 4 / 1),
      green = c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(1 + 4 / 1),
      ham = c(0 / 8, 0 / 6, 1 / 8, 0 / 8) * log(1 + 4 / 1),
      here = c(1 / 8, 0 / 6, 0 / 8, 0 / 8) * log(1 + 4 / 1),
      i = c(1 / 8, 1 / 6, 1 / 8, 2 / 8) * log(1 + 4 / 4),
      like = c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(1 + 4 / 1),
      not = c(1 / 8, 1 / 6, 1 / 8, 1 / 8) * log(1 + 4 / 4),
      or = c(1 / 8, 0 / 6, 0 / 8, 0 / 8) * log(1 + 4 / 1),
      sam = c(0 / 8, 0 / 6, 0 / 8, 1 / 8) * log(1 + 4 / 1),
      them = c(1 / 8, 1 / 6, 0 / 8, 1 / 8) * log(1 + 4 / 3),
      there = c(1 / 8, 0 / 6, 0 / 8, 0 / 8) * log(1 + 4 / 1),
      would = c(1 / 8, 1 / 6, 1 / 8, 0 / 8) * log(1 + 4 / 3)
    )
  )

  expect_equal(
    as.matrix(rec_answer),
    as.matrix(manual_answer)
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 4))
  expect_equal(dim(tidy(obj, 2)), c(17, 4))
})

test_that("step_tfidf works with vocabulary argument", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tfidf(text, vocabulary = letters)

  obj <- rec %>%
    prep()

  expect_length(
    bake(obj, new_data = NULL),
    26
  )
})

test_that("check_name() is used", {
  dat <- test_data
  dat$tfidf_text_i <- dat$text
  
  rec <- recipe(~., data = dat) %>%
    step_tokenize(text) %>%
    step_tfidf(text)
  
  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("idf valeus are trained on training data and applied on test data", {
  data <- tibble(text = c("i g", "i i i"))

  rec <- recipe(~text, data) %>%
    step_tokenize(text) %>%
    step_tfidf(text) %>%
    prep()

  expect_equal(
    bake(rec, data %>% slice(1)),
    bake(rec, data) %>% slice(1)
  )

  expect_equal(
    rec$steps[[2]]$res[[1]],
    c(g = log(1 + 2 / 1), i = log(1 + 2 / 2))
  )
})

test_that("Backwards compatibility with 1592690d36581fc5f4952da3e9b02351b31f1a2e", {
  # Test that recipes trained with version <= 0.5.0 keep previous behavoir and
  # throw warning about the data leakage.

  data <- tibble(text = c("i g", "i i i"))

  rec <- recipe(~text, data) %>%
    step_tokenize(text) %>%
    step_tfidf(text) %>%
    prep()

  rec$steps[[2]]$res <- list(c("g", "i"))

  expect_snapshot(
    expect_equal(
      bake(rec, data) %>% slice(1),
      tibble(tfidf_text_g = log(1 + 2 / 1) / 2, tfidf_text_i = log(1 + 2 / 2) / 2)
    )
  )

  expect_snapshot(
    expect_equal(
      bake(rec, data %>% slice(1)),
      tibble(tfidf_text_g = log(1 + 2 / 2) / 2, tfidf_text_i = log(1 + 2 / 2) / 2)
    )
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  tokenized_test_data <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    prep() %>%
    bake(new_data = NULL)
  
  rec <- recipe(tokenized_test_data) %>%
    update_role(text, new_role = "predictor") %>%
    step_tfidf(text) %>%
    update_role(text, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)
  
  trained <- prep(rec, training = tokenized_test_data, verbose = FALSE)
  
  expect_error(
    bake(trained, new_data = tokenized_test_data[, -1]),
    class = "new_data_missing_column"
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tfidf(rec)
  
  expect_snapshot(rec)
  
  rec <- prep(rec, mtcars)
  
  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_tfidf(rec1)
  
  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)
  
  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)
  
  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tfidf(rec)
  
  expect <- tibble(
    terms = character(),
    token = character(),
    weight = double(),
    id = character()
  )
  
  expect_identical(tidy(rec, number = 1), expect)
  
  rec <- prep(rec, mtcars)
  
  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- c(
    "tfidf_text_am", "tfidf_text_and", "tfidf_text_anywhere", "tfidf_text_do", 
    "tfidf_text_eat", "tfidf_text_eggs", "tfidf_text_green", "tfidf_text_ham", 
    "tfidf_text_here", "tfidf_text_i", "tfidf_text_like", "tfidf_text_not", 
    "tfidf_text_or", "tfidf_text_sam", "tfidf_text_them", "tfidf_text_there", 
    "tfidf_text_would"
  )
  
  rec <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    step_tfidf(text, keep_original_cols = FALSE)
  
  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)
  
  expect_equal(
    colnames(res),
    new_names
  )
  
  rec <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    step_tfidf(text, keep_original_cols = TRUE)
  
  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)
  
  expect_equal(
    colnames(res),
    c("text", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    step_tfidf(text)
  
  rec$steps[[2]]$keep_original_cols <- NULL
  
  expect_snapshot(
    rec <- prep(rec)
  )
  
  expect_error(
    bake(rec, new_data = test_data),
    NA
  )
})

test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tfidf(text)
  
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})