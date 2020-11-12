context("test-clean-names")

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
  expect_output(print(rec))
  expect_output(prep(rec, training = air_tr, verbose = TRUE))
})
