test_that("can clean names", {
  skip_if_not_installed("janitor")
  skip_if_not_installed("modeldata")

  air_tr <- airquality[1:20, ]
  air_te <- airquality[101:110, ]

  cleaned <- recipe(~., data = air_tr) %>% 
  step_clean_names(all_predictors(), id = "")

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

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("janitor")
  
  rec <- recipe(mtcars) %>%
    step_clean_names(disp) %>%
    update_role(disp, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)
  
  trained <- prep(rec)
  
  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = mtcars[, -3])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_clean_names(rec)
  
  expect_snapshot(rec)
  
  rec <- prep(rec, mtcars)
  
  expect_snapshot(rec)
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
  
  expect <- tibble(terms = character(), id = character())
  
  expect_identical(tidy(rec, number = 1), expect)
  
  rec <- prep(rec, mtcars)
  
  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  skip_if_not_installed("janitor")
  
  rec <- recipe(~., data = mtcars) %>% 
    step_clean_names(all_predictors())
  
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
