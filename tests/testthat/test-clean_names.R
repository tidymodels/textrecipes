library(testthat)
library(textrecipes)
data(airquality)

air_tr <- airquality[1:20, ]
air_te <- airquality[101:110, ]

rec <- recipe(~., data = air_tr)

test_that("can clean names", {
  skip_if_not_installed("janitor")
  cleaned <- rec %>% step_clean_names(all_predictors(), id = "")

  tidy_exp_un <- tibble(
    terms = c("all_predictors()"),
    id = ""
  )
  expect_equal(tidy_exp_un, tidy(cleaned, number = 1))

  cleaned <- prep(cleaned, training = air_tr)
  cleaned_tr <- bake(cleaned, new_data = NULL)
  cleaned_te <- bake(cleaned, new_data = air_te)

  expect_equal(sum(grepl(" ", colnames(cleaned_tr))), 0)
  expect_equal(sum(colnames(cleaned_tr) %in% colnames(air_tr)), 0)

  tidy_exp_tr <- tibble(
    terms = c("ozone", "solar_r", "wind", "temp", "month", "day"),
    value = c("Ozone", "Solar.R", "Wind", "Temp", "Month", "Day"),
    id = ""
  )
  expect_equal(tidy_exp_tr, tidy(cleaned, number = 1))
})


test_that("printing", {
  skip_if_not_installed("janitor")
  rec <- rec %>% step_clean_names(all_predictors())
  expect_snapshot(print(rec))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_clean_names(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_clean_names(rec)

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
  rec <- step_clean_names(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
