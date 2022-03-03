library(testthat)
library(recipes)
library(tibble)

ex_dat <- data.frame(text = c("sch\U00f6n", "scho\U0308n"))

test_that("simple sqrt trans", {
  skip_if_not_installed("stringi")

  rec <- recipe(~., data = ex_dat) %>%
    step_text_normalization(text)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  exp_res <- as_tibble(lapply(ex_dat, stringi::stri_trans_nfc))
  expect_equal(rec_trans$text, factor(exp_res$text))
})


test_that("printing", {
  skip_if_not_installed("stringi")

  rec <- recipe(~., data = ex_dat) %>%
    step_text_normalization(text)
  expect_snapshot(print(rec))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_text_normalization(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_text_normalization(rec)

  expect_identical(
    tidy(rec, number = 1),
    tibble(
      terms = character(),
      normalization_form = character(),
      id = character()
    )
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(
      terms = character(),
      normalization_form = character(),
      id = character()
    )
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_text_normalization(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
