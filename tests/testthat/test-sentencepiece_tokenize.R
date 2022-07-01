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
  c(
    "\U2581I", "\U2581would", "\U2581not", "\U2581\U0065\U0061\U0074",
    "\U2581them", "\U2581here", "\U2581or", "\U2581there", "."
  ),
  c(
    "\U2581I", "\U2581would", "\U2581not", "\U2581\U0065\U0061\U0074",
    "\U2581them",
    "\U2581\U0061\U006E\U0079\U0077\U0068\U0065\U0072\U0065", "."
  ),
  c(
    "\U2581I", "\U2581would", "\U2581not", "\U2581\U0065\U0061\U0074",
    "\U2581green", "\U2581\U0065\U0067\U0067\U0073",
    "\U2581\U0061\U006E\U0064", "\U2581ham", "."
  ),
  c(
    "\U2581I", "\U2581\U0064\U006F", "\U2581not", "\U2581like",
    "\U2581them", ",", "\U2581Sam", "-", "I", "-", "am", "."
  )
)

text2_out <- list(
  c(
    "\U2581You", "\U2581would", "\U2581not", "\U2581\U0065\U0061\U0074",
    "\U2581them", "\U2581here", "\U2581or", "\U2581there", "."
  ),
  c(
    "\U2581You", "\U2581would", "\U2581not", "\U2581\U0065\U0061\U0074",
    "\U2581them",
    "\U2581\U0061\U006E\U0079\U0077\U0068\U0065\U0072\U0065", "."
  ),
  c(
    "\U2581You", "\U2581would", "\U2581not", "\U2581\U0065\U0061\U0074",
    "\U2581green", "\U2581\U0065\U0067\U0067\U0073",
    "\U2581\U0061\U006E\U0064", "\U2581ham", "."
  ),
  c(
    "\U2581You", "\U2581\U0064\U006F", "\U2581not", "\U2581like",
    "\U2581them", ",", "\U2581Sam", "-", "I", "-", "am", "."
  )
)

test_that("step_tokenize_sentencepiece works", {
  res <- recipe(~text1, data = test_data) %>%
    step_tokenize_sentencepiece(text1, vocabulary_size = 80) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    vctrs::field(res$text1, "tokens"),
    text1_out
  )
})

test_that("step_tokenize_sentencepiece works with tokenizers.sentencepiece and multiple colunms", {
  res <- recipe(~., data = test_data) %>%
    step_tokenize_sentencepiece(all_predictors(), vocabulary_size = 80) %>%
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

test_that("arguments are passed to tokenizers.sentencepiece", {
  res <- recipe(~text1, data = test_data) %>%
    step_tokenize_sentencepiece(text1, vocabulary_size = 60) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    length(textrecipes:::get_unique_tokens(res$text1)),
    60
  )

  res <- recipe(~text1, data = test_data) %>%
    step_tokenize_sentencepiece(text1, vocabulary_size = 80) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    length(textrecipes:::get_unique_tokens(res$text1)),
    80
  )
})

test_that("Errors if vocabulary size is set to low.", {
  expect_snapshot(error = TRUE,
    recipe(~text1, data = test_data) %>%
      step_tokenize_sentencepiece(text1, vocabulary_size = 10) %>%
      prep()
  )
})

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~text1, data = test_data) %>%
    step_tokenize_sentencepiece(text1, vocabulary_size = 100) %>%
    update_role(text1, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)
  
  trained <- prep(rec, training = test_data, verbose = FALSE)
  
  expect_error(bake(trained, new_data = test_data[, -1]),
               class = "new_data_missing_column")
})

test_that("printing", {
  rec <- recipe(~., data = test_data) %>%
    step_tokenize_sentencepiece(text1, vocabulary_size = 100)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_tokenize_sentencepiece(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tokenize_sentencepiece(rec)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), id = character())
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), id = character())
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tokenize_sentencepiece(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
