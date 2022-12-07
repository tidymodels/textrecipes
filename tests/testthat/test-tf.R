library(recipes)
library(textrecipes)

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec <- recipe(~., data = test_data)

test_that("step_tf works as intended", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text)

  obj <- rec %>%
    prep()

  rec_answer <- unname(as.data.frame(bake(obj, new_data = NULL)))
  manual_answer <- unname(data.frame(
    am = c(0L, 0L, 0L, 1L),
    and = c(0L, 0L, 1L, 0L),
    anywhere = c(0L, 1L, 0L, 0L),
    do = c(0L, 0L, 0L, 1L),
    eat = c(1L, 1L, 1L, 0L),
    eggs = c(0L, 0L, 1L, 0L),
    green = c(0L, 0L, 1L, 0L),
    ham = c(0L, 0L, 1L, 0L),
    here = c(1L, 0L, 0L, 0L),
    i = c(1L, 1L, 1L, 2L),
    like = c(0L, 0L, 0L, 1L),
    not = c(1L, 1L, 1L, 1L),
    or = c(1L, 0L, 0L, 0L),
    sam = c(0L, 0L, 0L, 1L),
    them = c(1L, 1L, 0L, 1L),
    there = c(1L, 0L, 0L, 0L),
    would = c(1L, 1L, 1L, 0L)
  ))

  expect_identical(
    as.matrix(rec_answer),
    as.matrix(manual_answer)
  )

  expect_identical(dim(tidy(rec, 2)), c(1L, 3L))
  expect_identical(dim(tidy(obj, 2)), c(1L, 3L))
})

test_that("step_tf works with vocabulary argument", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, vocabulary = letters)

  obj <- rec %>%
    prep()

  expect_length(
    bake(obj, new_data = NULL),
    26
  )
})


test_that("step_tf works with other weighting schemes", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, weight_scheme = "term frequency")

  obj <- rec %>%
    prep()

  rec_answer <- unname(as.data.frame(bake(obj, new_data = NULL)))
  manual_answer <- unname(data.frame(
    am = c(0 / 8, 0 / 6, 0 / 8, 1 / 8),
    and = c(0 / 8, 0 / 6, 1 / 8, 0 / 8),
    anywhere = c(0 / 8, 1 / 6, 0 / 8, 0 / 8),
    do = c(0 / 8, 0 / 6, 0 / 8, 1 / 8),
    eat = c(1 / 8, 1 / 6, 1 / 8, 0 / 8),
    eggs = c(0 / 8, 0 / 6, 1 / 8, 0 / 8),
    green = c(0 / 8, 0 / 6, 1 / 8, 0 / 8),
    ham = c(0 / 8, 0 / 6, 1 / 8, 0 / 8),
    here = c(1 / 8, 0 / 6, 0 / 8, 0 / 8),
    i = c(1 / 8, 1 / 6, 1 / 8, 2 / 8),
    like = c(0 / 8, 0 / 6, 0 / 8, 1 / 8),
    not = c(1 / 8, 1 / 6, 1 / 8, 1 / 8),
    or = c(1 / 8, 0 / 6, 0 / 8, 0 / 8),
    sam = c(0 / 8, 0 / 6, 0 / 8, 1 / 8),
    them = c(1 / 8, 1 / 6, 0 / 8, 1 / 8),
    there = c(1 / 8, 0 / 6, 0 / 8, 0 / 8),
    would = c(1 / 8, 1 / 6, 1 / 8, 0 / 8)
  ))

  expect_identical(
    as.matrix(rec_answer),
    as.matrix(manual_answer)
  )
})

test_that("step_tf term frequency returns 0 with no tokens", {
  d <- tibble(text = c("a b a d", ""))

  res <- recipe(~text, data = d) %>%
    step_tokenize(text) %>%
    step_tf(text, weight_scheme = "term frequency") %>%
    prep() %>%
    bake(new_data = NULL)

  exp_res <- tibble(
    tf_text_a = c(0.5, 0),
    tf_text_b = c(0.25, 0),
    tf_text_d = c(0.25, 0)
  )
  expect_identical(res, exp_res)
})

test_that("bake method errors when needed non-standard role columns are missing", {
  tokenized_test_data <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    prep() %>%
    bake(new_data = NULL)
  
  rec <- recipe(tokenized_test_data) %>%
    update_role(text, new_role = "predictor") %>%
    step_tf(text) %>%
    update_role(text, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)
  
  trained <- prep(rec, training = tokenized_test_data, verbose = FALSE)
  
  expect_error(bake(trained, new_data = tokenized_test_data[, -1]),
               class = "new_data_missing_column")
})

test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("keep_original_cols works", {
  koc_rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, keep_original_cols = TRUE)
  
  koc_trained <- prep(koc_rec, training = test_data, verbose = FALSE)
  
  koc_pred <- bake(koc_trained, new_data = test_data, all_predictors())
  
  expect_identical(
    colnames(koc_pred),
    c("text", "tf_text_am", "tf_text_and", "tf_text_anywhere", "tf_text_do", 
      "tf_text_eat", "tf_text_eggs", "tf_text_green", "tf_text_ham", 
      "tf_text_here", "tf_text_i", "tf_text_like", "tf_text_not", "tf_text_or", 
      "tf_text_sam", "tf_text_them", "tf_text_there", "tf_text_would"
    )
  )
})

test_that("can prep recipes with no keep_original_cols", {
  koc_rec <- rec %>%
    step_tokenize(text) %>%
    step_tf(text, keep_original_cols = TRUE)
  
  koc_rec$steps[[2]]$keep_original_cols <- NULL
  
  expect_snapshot(
    koc_trained <- prep(koc_rec, training = test_data, verbose = FALSE)
  )
  
  expect_error(
    pca_pred <- bake(koc_trained, new_data = test_data, all_predictors()),
    NA
  )
})


test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_tf(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tf(rec)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), value = character(), id = character())
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), value = character(), id = character())
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tf(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
