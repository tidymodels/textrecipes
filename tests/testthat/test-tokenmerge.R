library(textrecipes)
library(recipes)

test_data <- tibble(
  text1 = c(
    "I would not eat them here or there.",
    "I would not eat them anywhere.",
    "I would not eat green eggs and ham.",
    "I do not like them, Sam-I-am."
  ),
  text2 = c(
    "I would not eat them here or there.",
    "I would not eat them anywhere.",
    "I would not eat green eggs and ham.",
    "I do not like them, Sam-I-am."
  )
)

rec <- recipe(~., data = test_data)

test_that("merging is done correctly", {
  rec <- rec %>%
    step_tokenize(text1, text2) %>%
    step_tokenmerge(text1, text2)

  obj <- rec %>%
    prep()

  baked_data <- bake(obj, new_data = NULL)

  rec2 <- recipe(~., data = test_data) %>%
    step_tokenize(text1, text2) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    lengths(vctrs::field(baked_data$tokenmerge, "tokens")),
    lengths(vctrs::field(rec2$text1, "tokens")) +
      lengths(vctrs::field(rec2$text2, "tokens"))
  )

  expect_equal(dim(recipes:::tidy.recipe(rec, 1)), c(2, 3))
  expect_equal(dim(recipes:::tidy.recipe(obj, 1)), c(2, 3))
})

test_that("it complains when the selected column isn't a tokenlist", {
  rec <- rec %>%
    step_tokenmerge(text1, text2)

  expect_snapshot(
    error = TRUE,
    prep(rec)
  )
})

test_that("check_name() is used", {
  dat <- test_data
  dat$tokenmerge <- dat$text1
  
  rec <- recipe(~., data = dat) %>%
    step_tokenize(text1, text2) %>%
    step_tokenmerge(text1, text2)
  
  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  tokenized_test_data <- recipe(~ text1 + text2, data = test_data) %>%
    step_tokenize(text1, text2) %>%
    prep() %>%
    bake(new_data = NULL)
  
  rec <- recipe(tokenized_test_data) %>%
    update_role(text1, text2, new_role = "predictor") %>%
    step_tokenmerge(text1, text2) %>%
    update_role(text1, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)
  
  trained <- prep(rec, training = tokenized_test_data, verbose = FALSE)
  
  expect_error(bake(trained, new_data = tokenized_test_data[, -1]),
               class = "new_data_missing_column"
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tokenmerge(rec)
  
  expect_snapshot(rec)
  
  rec <- prep(rec, mtcars)
  
  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_tokenmerge(rec1)
  
  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)
  
  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)
  
  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tokenmerge(rec)
  
  expect <- tibble(terms = character(), id = character())
  
  expect_identical(tidy(rec, number = 1), expect)
  
  rec <- prep(rec, mtcars)
  
  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- c("tokenmerge")
  
  rec <-  recipe(~., data = test_data) %>%
    step_tokenize(text1, text2) %>%
    step_tokenmerge(text1, text2, keep_original_cols = FALSE)
  
  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)
  
  expect_equal(
    colnames(res),
    new_names
  )
  
  rec <- recipe(~., data = test_data) %>%
    step_tokenize(text1, text2) %>%
    step_tokenmerge(text1, text2, keep_original_cols = TRUE)
  
  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)
  
  expect_equal(
    colnames(res),
    c("text1", "text2", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~., data = test_data) %>%
    step_tokenize(text1, text2) %>%
    step_tokenmerge(text1, text2)
  
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
    step_tokenize(text1, text2) %>%
    step_tokenmerge(text1, text2)
  
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})