test_data <- tibble(
  text = c(
    "I would not eat them here or there.",
    "I would not eat them anywhere.",
    "I would not eat green eggs and ham.",
    "I do not like them, Sam-I-am."
  )
)

rec <- recipe(~., data = test_data)

test_that("tokenfilter removes words correctly using min_times and max_times", {
  rec <- rec |>
    step_tokenize(text) |>
    step_tokenfilter(text, max_times = 3, min_times = 2)

  expect_snapshot(
    obj <- rec |>
      prep()
  )

  expect_equal(
    bake(obj, new_data = NULL) |> pull(text) |> vctrs::field("tokens"),
    list(
      c("would", "eat", "them"),
      c("would", "eat", "them"),
      c("would", "eat"),
      c("them")
    )
  )

  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 3))
})

test_that("removes words correctly with min_times, max_times and procentage", {
  rec <- rec |>
    step_tokenize(text) |>
    step_tokenfilter(text, max_times = 0.04, min_times = 0, percentage = TRUE)

  expect_snapshot(
    obj <- rec |>
      prep()
  )

  expect_equal(
    bake(obj, new_data = NULL) |> pull(text) |> vctrs::field("tokens"),
    list(
      c("here", "or", "there"),
      c("anywhere"),
      c("green", "eggs", "and", "ham"),
      c("do", "like", "sam", "am")
    )
  )
})

test_that("tokenfilter removes words correctly using max_tokens", {
  rec <- rec |>
    step_tokenize(text) |>
    step_tokenfilter(text, max_tokens = 10)

  obj <- rec |>
    prep()

  expect_equal(
    bake(obj, new_data = NULL) |> pull(text) |> vctrs::field("tokens"),
    list(
      c("i", "would", "not", "eat", "them"),
      c("i", "would", "not", "eat", "them", "anywhere"),
      c("i", "would", "not", "eat", "eggs", "and"),
      c("i", "do", "not", "them", "i", "am")
    )
  )
})

test_that("tokenfilter throws warning when max_tokens > words", {
  rec <- rec |>
    step_tokenize(text) |>
    step_tokenfilter(text, max_tokens = 10000)

  expect_snapshot(
    rec |>
      prep()
  )
})

test_that("tokenfilter works with filter_fun", {
  obj <- recipe(~., data = test_data) |>
    step_tokenize(text) |>
    step_tokenfilter(text, filter_fun = function(x) nchar(x) >= 5) |>
    prep()

  expect_equal(
    bake(obj, new_data = NULL) |> pull(text) |> vctrs::field("tokens"),
    list(
      c("would", "there"),
      c("would", "anywhere"),
      c("would", "green"),
      character()
    )
  )

  obj <- recipe(~., data = test_data) |>
    step_tokenize(text) |>
    step_tokenfilter(text, filter_fun = function(x) grepl("^e", x)) |>
    prep()

  expect_equal(
    bake(obj, new_data = NULL) |> pull(text) |> vctrs::field("tokens"),
    list(
      c("eat"),
      c("eat"),
      c("eat", "eggs"),
      character()
    )
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) |>
    step_tokenfilter(all_predictors())
  rec_param <- tunable.step_tokenfilter(rec$steps[[1]])
  expect_equal(rec_param$name, c("max_times", "min_times", "max_tokens"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 3)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_tokenfilter(percentage = "yes") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_tokenfilter(max_tokens = -4) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_tokenfilter(filter_fun = -4) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_tokenfilter(percentage = TRUE, max_times = 2) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_tokenfilter(percentage = TRUE, min_times = 2) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_tokenfilter(percentage = FALSE, max_times = -1) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_tokenfilter(percentage = FALSE, min_times = -1) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  tokenized_test_data <- recipe(~text, data = test_data) |>
    step_tokenize(text) |>
    prep() |>
    bake(new_data = NULL)

  rec <- recipe(tokenized_test_data) |>
    update_role(text, new_role = "predictor") |>
    step_tokenfilter(text, max_tokens = 10) |>
    update_role(text, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = tokenized_test_data, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = tokenized_test_data[, -1])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tokenfilter(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_tokenfilter(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tokenfilter(rec)

  expect <- tibble(terms = character(), value = integer(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- rec |>
    step_tokenize(text) |>
    step_tokenfilter(text)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_tokenfilter(
      all_predictors(),
      max_times = hardhat::tune(),
      min_times = hardhat::tune(),
      max_tokens = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 3L)
})
