text <- tibble(
  text = c(
    "I would not eat them here or there.",
    "I would not eat them anywhere.",
    "I would not eat green eggs and ham.",
    "I do not like them, Sam-I-am."
  )
)

test_that("part of speech filtering works", {
  skip_on_cran()
  skip_if_not_installed("spacyr")
  skip_if_no_python_or_no_spacy()

  rec <- recipe(~text, data = text) |>
    step_tokenize(all_predictors(), engine = "spacyr") |>
    step_pos_filter(all_predictors())

  prepped_data <- rec |>
    prep() |>
    bake(new_data = NULL)

  expect_s3_class(prepped_data$text, "textrecipes_tokenlist")

  expect_equal(
    vctrs::field(prepped_data$text, "tokens"),
    list(
      character(),
      character(),
      c("eggs"),
      character()
    )
  )
})

test_that("part of speech filtering removes everything", {
  skip_on_cran()
  skip_if_not_installed("spacyr")
  skip_if_no_python_or_no_spacy()

  rec <- recipe(~text, data = text) |>
    step_tokenize(all_predictors(), engine = "spacyr") |>
    step_pos_filter(all_predictors(), keep_tags = character())

  prepped_data <- rec |>
    prep() |>
    bake(new_data = NULL)

  expect_s3_class(prepped_data$text, "textrecipes_tokenlist")

  expect_equal(
    vctrs::field(prepped_data$text, "tokens"),
    list(
      character(),
      character(),
      character(),
      character()
    )
  )
})

test_that("part of speech filtering works with multiple tags", {
  skip_on_cran()
  skip_if_not_installed("spacyr")
  skip_if_no_python_or_no_spacy()

  rec <- recipe(~text, data = text) |>
    step_tokenize(all_predictors(), engine = "spacyr") |>
    step_pos_filter(all_predictors(), keep_tags = c("VERB", "NOUN"))

  prepped_data <- rec |>
    prep() |>
    bake(new_data = NULL)

  expect_s3_class(prepped_data$text, "textrecipes_tokenlist")

  expect_equal(
    vctrs::field(prepped_data$text, "tokens"),
    list(
      c("eat"),
      c("eat"),
      c("eat", "eggs"),
      c("like")
    )
  )
})

test_that("lemmatization errors if lemma attribute doesn't exists", {
  rec <- recipe(~text, data = text) |>
    step_tokenize(all_predictors()) |>
    step_pos_filter(all_predictors())

  expect_snapshot(
    error = TRUE,
    prep(rec)
  )
})

test_that("bad args", {
  skip_on_cran()
  skip_if_not_installed("spacyr")
  skip_if_no_python_or_no_spacy()

  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_pos_filter(keep_tags = -4) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_on_cran()
  skip_if_not_installed("spacyr")
  skip_if_no_python_or_no_spacy()

  tokenized_test_data <- recipe(~text, data = text) |>
    step_tokenize(all_predictors(), engine = "spacyr") |>
    prep() |>
    bake(new_data = NULL)

  rec <- recipe(tokenized_test_data) |>
    step_pos_filter(text) |>
    update_role(text, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = tokenized_test_data[, -1])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_pos_filter(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_pos_filter(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_pos_filter(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  skip_on_cran()
  skip_if_not_installed("spacyr")
  skip_if_no_python_or_no_spacy()

  rec <- recipe(~text, data = text) |>
    step_tokenize(all_predictors(), engine = "spacyr") |>
    step_pos_filter(all_predictors())

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
