test_that("show_tokens() returns the tokens of a prepped recipe", {
  text_tibble <- tibble(text = c("This is words", "They are nice!"))

  rec <- recipe(~text, data = text_tibble) |>
    step_tokenize(text)

  res <- show_tokens(rec, text, n = 2)

  expect_type(res, "list")
  expect_length(res, 2)
  expect_equal(res[[1]], c("this", "is", "words"))
  expect_equal(res[[2]], c("they", "are", "nice"))
})

test_that("show_tokens() respects `n` and returns at most that many elements", {
  text_tibble <- tibble(text = c("This is words", "They are nice!"))

  rec <- recipe(~text, data = text_tibble) |>
    step_tokenize(text)

  res <- show_tokens(rec, text, n = 1)

  expect_type(res, "list")
  expect_length(res, 1)
  expect_equal(res[[1]], c("this", "is", "words"))
})

test_that("show_tokens() errors clearly when `n` is above nrow(rec$template)", {
  text_tibble <- tibble(text = c("This is words", "They are nice!"))

  rec <- recipe(~text, data = text_tibble) |>
    step_tokenize(text)

  expect_snapshot(
    error = TRUE,
    show_tokens(rec, text, n = 100)
  )
})

test_that("show_tokens() errors clearly when `n` is below the min bound", {
  text_tibble <- tibble(text = c("This is words", "They are nice!"))

  rec <- recipe(~text, data = text_tibble) |>
    step_tokenize(text)

  expect_snapshot(
    error = TRUE,
    show_tokens(rec, text, n = -1)
  )
})

test_that("show_tokens() errors clearly when `n` isn't a whole number", {
  text_tibble <- tibble(text = c("This is words", "They are nice!"))

  rec <- recipe(~text, data = text_tibble) |>
    step_tokenize(text)

  expect_snapshot(
    error = TRUE,
    show_tokens(rec, text, n = 2.5)
  )
})
