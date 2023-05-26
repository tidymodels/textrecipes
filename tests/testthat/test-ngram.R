test_data <- list(
  c("not", "eat", "them", "here", "or", "there."),
  c("not", "eat", "them", "anywhere.")
)

test_that("ngram works with varrying number of `n`", {
  expect_equal(
    cpp11_ngram(test_data, n = 1L, n_min = 1, delim = "_"),
    test_data
  )

  expect_equal(
    cpp11_ngram(test_data, n = 2L, n_min = 2L, delim = "_"),
    list(
      c("not_eat", "eat_them", "them_here", "here_or", "or_there."),
      c("not_eat", "eat_them", "them_anywhere.")
    )
  )

  expect_equal(
    cpp11_ngram(test_data, n = 3L, n_min = 3L, delim = "_"),
    list(
      c("not_eat_them", "eat_them_here", "them_here_or", "here_or_there."),
      c("not_eat_them", "eat_them_anywhere.")
    )
  )

  expect_snapshot(
    error = TRUE,
    cpp11_ngram(test_data, n = 0L, n_min = 0L, delim = "_")
  )

  expect_snapshot(
    error = TRUE,
    cpp11_ngram(test_data, n = -1L, n_min = -1L, delim = "_")
  )
})

test_that("tokenlist_ngram works with n_min and n", {
  tknlist <- list(
    c("a", "b", "c", "d", "e"),
    c("a", "b"),
    c("a"),
    character(0)
  )

  expect_equal(
    cpp11_ngram(tknlist, 1, 1, " "),
    tknlist
  )

  expect_equal(
    cpp11_ngram(tknlist, 2, 2, " "),
    list(
      c("a b", "b c", "c d", "d e"),
      c("a b"),
      character(0),
      character(0)
    )
  )

  expect_equal(
    cpp11_ngram(tknlist, 3, 3, " "),
    list(
      c("a b c", "b c d", "c d e"),
      character(0),
      character(0),
      character(0)
    )
  )

  expect_equal(
    cpp11_ngram(tknlist, 2, 1, " "),
    list(
      c("a", "b", "c", "d", "e", "a b", "b c", "c d", "d e"),
      c("a", "b", "a b"),
      c("a"),
      character(0)
    )
  )

  expect_equal(
    cpp11_ngram(tknlist, 3, 1, " "),
    list(
      c("a", "b", "c", "d", "e", "a b", "b c", "c d", "d e", "a b c", "b c d", "c d e"),
      c("a", "b", "a b"),
      c("a"),
      character(0)
    )
  )

  expect_equal(
    cpp11_ngram(tknlist, 3, 2, " "),
    list(
      c("a b", "b c", "c d", "d e", "a b c", "b c d", "c d e"),
      c("a b"),
      character(0),
      character(0)
    )
  )
})

test_that("ngram works with delim", {
  expect_equal(
    cpp11_ngram(test_data, n = 3L, n_min = 3L, delim = ""),
    list(
      c("noteatthem", "eatthemhere", "themhereor", "hereorthere."),
      c("noteatthem", "eatthemanywhere.")
    )
  )

  expect_equal(
    cpp11_ngram(test_data, n = 3L, n_min = 3L, delim = " "),
    list(
      c("not eat them", "eat them here", "them here or", "here or there."),
      c("not eat them", "eat them anywhere.")
    )
  )
})

test_that("ngram returns length zero vectors when length(x) < n", {
  expect_equal(
    cpp11_ngram(list(c("a", "b")), n = 3L, n_min = 3L, delim = "_"),
    list(character())
  )

  expect_equal(
    cpp11_ngram(list(c("a")), n = 3L, n_min = 3L, delim = "_"),
    list(character())
  )

  expect_equal(
    cpp11_ngram(list(character()), n = 3L, n_min = 3L, delim = "_"),
    list(character())
  )
})

library(recipes)
library(textrecipes)

test_tibble <- tibble(text = c(
  "not eat them here or there.",
  "not eat them anywhere."
))

rec <- recipe(~., data = test_tibble)

test_that("ngramming is done correctly", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_ngram(text, num_tokens = 3, min_num_tokens = 3)

  obj <- rec %>%
    prep()

  expect_equal(
    bake(obj, new_data = NULL) %>%
      pull(text) %>%
      vctrs::field("tokens"),
    list(
      c("not_eat_them", "eat_them_here", "them_here_or", "here_or_there"),
      c("not_eat_them", "eat_them_anywhere")
    )
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 2))
  expect_equal(dim(tidy(obj, 2)), c(1, 2))
})

test_that("`n` argument works", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_ngram(text, num_tokens = 2, min_num_tokens = 2)

  obj <- rec %>%
    prep()

  expect_equal(
    bake(obj, new_data = NULL) %>%
      pull(text) %>%
      vctrs::field("tokens"),
    list(
      c("not_eat", "eat_them", "them_here", "here_or", "or_there"),
      c("not_eat", "eat_them", "them_anywhere")
    )
  )
})

test_that("ngramming works with min_num_tokens", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_ngram(text, num_tokens = 3, min_num_tokens = 1)

  obj <- rec %>%
    prep()

  expect_equal(
    bake(obj, new_data = NULL) %>%
      pull(text) %>%
      vctrs::field("tokens"),
    list(
      c(
        "not", "eat", "them", "here", "or", "there",
        "not_eat", "eat_them", "them_here", "here_or", "or_there",
        "not_eat_them", "eat_them_here", "them_here_or", "here_or_there"
      ),
      c(
        "not", "eat", "them", "anywhere",
        "not_eat", "eat_them", "them_anywhere",
        "not_eat_them", "eat_them_anywhere"
      )
    )
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 2))
  expect_equal(dim(tidy(obj, 2)), c(1, 2))
})

test_that("`delim` argument works", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_ngram(text, delim = " ", num_tokens = 3, min_num_tokens = 3)

  obj <- rec %>%
    prep()

  expect_equal(
    bake(obj, new_data = NULL) %>%
      pull(text) %>%
      vctrs::field("tokens"),
    list(
      c("not eat them", "eat them here", "them here or", "here or there"),
      c("not eat them", "eat them anywhere")
    )
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) %>%
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

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  tokenized_test_data <- recipe(~text, data = test_tibble) %>%
    step_tokenize(text) %>%
    prep() %>%
    bake(new_data = NULL)
  
  rec <- recipe(tokenized_test_data) %>%
    update_role(text, new_role = "predictor") %>%
    step_ngram(text) %>%
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
  rec <- step_ngram(rec)
  
  expect_snapshot(rec)
  
  rec <- prep(rec, mtcars)
  
  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_ngram(rec1)
  
  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)
  
  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)
  
  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_ngram(rec)
  
  expect <- tibble(terms = character(), id = character())
  
  expect_identical(tidy(rec, number = 1), expect)
  
  rec <- prep(rec, mtcars)
  
  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_ngram(text)
  
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) %>%
    step_ngram(
      all_predictors(),
      num_tokens = hardhat::tune()
    )
  
  params <- extract_parameter_set_dials(rec)
  
  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})