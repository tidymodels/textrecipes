test_data <- tibble(
  text = c(
    "I would not eat them here or there.",
    "I would not eat them anywhere.",
    "I would not eat green eggs and ham.",
    "I do not like them, Sam-I-am."
  )
)

rec <- recipe(~., data = test_data)

get_tokens_baked <- function(x, i = 1) {
  vctrs::field(x$text, "tokens")[[i]]
}

test_that("exact replacement works", {
  rec <- rec |>
    step_tokenize(text) |>
    step_token_replace(
      text,
      pattern = c("eat", "them"),
      replacement = c("EAT", "THEM")
    )

  obj <- prep(rec)
  baked <- bake(obj, new_data = NULL)

  expect_equal(
    get_tokens_baked(baked),
    c("i", "would", "not", "EAT", "THEM", "here", "or", "there")
  )

  expect_equal(
    tidy(rec, 2),
    tibble(
      terms = "text",
      pattern = c("eat", "them"),
      replacement = c("EAT", "THEM"),
      id = rec$steps[[2]]$id
    )
  )
  expect_equal(dim(tidy(obj, 2)), c(2, 4))
})

test_that("replacement of length 1 is recycled", {
  obj <- rec |>
    step_tokenize(text) |>
    step_token_replace(text, pattern = c("eat", "ham"), replacement = "FOOD") |>
    prep()

  expect_equal(
    get_tokens_baked(bake(obj, new_data = NULL), 3),
    c("i", "would", "not", "FOOD", "green", "eggs", "and", "FOOD")
  )
})

test_that("regex replacement works and first match wins", {
  obj <- rec |>
    step_tokenize(text) |>
    step_token_replace(
      text,
      pattern = c("^th", "^t"),
      replacement = c("TH", "T"),
      fixed = FALSE
    ) |>
    prep()

  expect_equal(
    get_tokens_baked(bake(obj, new_data = NULL)),
    c("i", "would", "not", "eat", "TH", "here", "or", "TH")
  )
})

test_that("empty tokens and no matches work", {
  data <- tibble(text = c("", "a b"))
  obj <- recipe(~text, data = data) |>
    step_tokenize(text) |>
    step_token_replace(text, pattern = "z", replacement = "y") |>
    prep()

  baked <- bake(obj, new_data = NULL)
  expect_equal(
    vctrs::field(baked$text, "tokens"),
    list(character(), c("a", "b"))
  )
})

test_that("replacement preserves lemma and pos attributes", {
  tkn_list <- tokenlist(
    list(c("running", "dogs"), c("jumped")),
    lemma = list(c("run", "dog"), c("jump")),
    pos = list(c("VERB", "NOUN"), c("VERB"))
  )
  data <- tibble(text = tkn_list)

  baked <- recipe(~text, data = data) |>
    step_token_replace(text, pattern = "dogs", replacement = "cats") |>
    prep(data) |>
    bake(new_data = NULL)

  expect_equal(vctrs::field(baked$text, "tokens")[[1]], c("running", "cats"))
  expect_equal(
    vctrs::field(baked$text, "lemma"),
    vctrs::field(tkn_list, "lemma")
  )
  expect_equal(vctrs::field(baked$text, "pos"), vctrs::field(tkn_list, "pos"))
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    step_token_replace(rec, text, pattern = 1, replacement = "a")
  )
  expect_snapshot(
    error = TRUE,
    step_token_replace(
      rec,
      text,
      pattern = c("a", "b"),
      replacement = c("a", "b", "c")
    )
  )
  expect_snapshot(
    error = TRUE,
    step_token_replace(
      rec,
      text,
      pattern = "a",
      replacement = "b",
      fixed = "yes"
    )
  )
  expect_snapshot(error = TRUE, step_token_replace(rec, text))
  expect_snapshot(
    error = TRUE,
    step_token_replace(rec, text, pattern = NA_character_, replacement = "b")
  )
  expect_snapshot(
    error = TRUE,
    step_token_replace(rec, text, pattern = "a", replacement = NA_character_)
  )
  expect_snapshot(
    error = TRUE,
    step_token_replace(
      rec,
      text,
      pattern = "[",
      replacement = "b",
      fixed = FALSE
    )
  )
  expect_snapshot(
    error = TRUE,
    step_token_replace(
      rec,
      text,
      pattern = character(),
      replacement = character()
    )
  )
  expect_snapshot(
    error = TRUE,
    step_token_replace(
      rec,
      text,
      pattern = "",
      replacement = "b",
      fixed = FALSE
    )
  )
  expect_snapshot(
    error = TRUE,
    rec |>
      step_token_replace(text, pattern = "a", replacement = "b") |>
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
    step_token_replace(text, pattern = "a", replacement = "b") |>
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
  rec <- step_token_replace(rec, pattern = "a", replacement = "b")

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_token_replace(rec1, pattern = "a", replacement = "b")

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  expect_identical(bake(rec1, mtcars), bake(rec2, mtcars))
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_token_replace(rec, pattern = "a", replacement = "b")

  expect <- tibble(
    terms = character(),
    pattern = character(),
    replacement = character(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- rec |>
    step_tokenize(text) |>
    step_token_replace(text, pattern = "a", replacement = "b")

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  rec <- recipe(~text, data = test_data) |>
    step_tokenize(text) |>
    step_token_replace(text, pattern = "not", replacement = "NOT") |>
    prep()

  expect_identical(nrow(bake(rec, dplyr::slice(test_data, 1))), 1L)
  expect_identical(nrow(bake(rec, dplyr::slice(test_data, 0))), 0L)
})
