r_version <- function() paste0("R", getRversion()[, 1:2])

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
    "\U2581I",
    "\U2581would",
    "\U2581not",
    "\U2581\U0065\U0061\U0074",
    "\U2581them",
    "\U2581here",
    "\U2581or",
    "\U2581there."
  ),
  c(
    "\U2581I",
    "\U2581would",
    "\U2581not",
    "\U2581\U0065\U0061\U0074",
    "\U2581them",
    "\U2581\U0061\U006E\U0079\U0077\U0068\U0065\U0072\U0065\U002E"
  ),
  c(
    "\U2581I",
    "\U2581would",
    "\U2581not",
    "\U2581\U0065\U0061\U0074",
    "\U2581green",
    "\U2581\U0065\U0067\U0067\U0073",
    "\U2581\U0061\U006E\U0064",
    "\U2581ham."
  ),
  c(
    "\U2581I",
    "\U2581\U0064\U006F",
    "\U2581not",
    "\U2581like",
    "\U2581them,",
    "\U2581Sam-I-am."
  )
)

text2_out <- list(
  c(
    "\U2581You",
    "\U2581would",
    "\U2581not",
    "\U2581\U0065\U0061\U0074",
    "\U2581them",
    "\U2581here",
    "\U2581or",
    "\U2581there."
  ),
  c(
    "\U2581You",
    "\U2581would",
    "\U2581not",
    "\U2581\U0065\U0061\U0074",
    "\U2581them",
    "\U2581\U0061\U006E\U0079\U0077\U0068\U0065\U0072\U0065\U002E"
  ),
  c(
    "\U2581You",
    "\U2581would",
    "\U2581not",
    "\U2581\U0065\U0061\U0074",
    "\U2581green",
    "\U2581\U0065\U0067\U0067\U0073",
    "\U2581\U0061\U006E\U0064",
    "\U2581ham."
  ),
  c(
    "\U2581You",
    "\U2581\U0064\U006F",
    "\U2581not",
    "\U2581like",
    "\U2581them,",
    "\U2581Sam-I-am."
  )
)

test_that("output is list when length is 1 or 0", {
  skip_if_not_installed("tokenizers.bpe")

  data <- tibble(a = rep(c("a", ""), 20))

  data_rec <- recipe(~., data = data) %>%
    step_tokenize_bpe(a) %>%
    prep()

  expect_true(is.list(bake(data_rec, new_data = NULL, a)[, 1, drop = TRUE]))
})

test_that("step_tokenize_bpe works", {
  skip_if_not_installed("tokenizers.bpe")

  res <- recipe(~text1, data = test_data) %>%
    step_tokenize_bpe(text1) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    vctrs::field(res$text1, "tokens"),
    text1_out
  )
})

test_that("step_tokenize_bpe works with tokenizers.bpe and multiple colunms", {
  skip_if_not_installed("tokenizers.bpe")

  res <- recipe(~., data = test_data) %>%
    step_tokenize_bpe(all_predictors()) %>%
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

test_that("arguments are passed to tokenizers.bpe", {
  skip_if_not_installed("tokenizers.bpe")

  res <- recipe(~text1, data = test_data) %>%
    step_tokenize_bpe(text1, vocabulary_size = 60) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    length(textrecipes:::get_unique_tokens(res$text1)),
    60
  )

  res <- recipe(~text1, data = test_data) %>%
    step_tokenize_bpe(text1, vocabulary_size = 80) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    length(textrecipes:::get_unique_tokens(res$text1)),
    80
  )
})

test_that("Errors if vocabulary size is set to low.", {
  skip_if_not_installed("tokenizers.bpe")

  expect_snapshot(
    error = TRUE,
    variant = r_version(),
    recipe(~text1, data = test_data) %>%
      step_tokenize_bpe(text1, vocabulary_size = 10) %>%
      prep()
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) %>%
      step_tokenize_bpe(all_predictors())
  rec_param <- tunable.step_tokenize_bpe(rec$steps[[1]])
  expect_equal(rec_param$name, c("vocabulary_size"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("bad args", {
  skip_if_not_installed("tokenizers.bpe")

  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_tokenize_bpe(vocabulary_size = -4) %>%
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("tokenizers.bpe")

  rec <- recipe(~text1, data = test_data) %>%
    step_tokenize_bpe(text1) %>%
    update_role(text1, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = test_data, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = test_data[, -1])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tokenize_bpe(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_tokenize_bpe(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tokenize_bpe(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  skip_if_not_installed("tokenizers.bpe")

  rec <- recipe(~., data = test_data) %>%
    step_tokenize_bpe(text1)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = test_data) %>%
    step_tokenize_bpe(text1, vocabulary_size = hardhat::tune())

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})
