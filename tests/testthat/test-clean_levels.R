library(testthat)
library(textrecipes)
library(modeldata)
data("Smithsonian")
smith_tr <- Smithsonian[1:15, ]
smith_te <- Smithsonian[16:20, ]

rec <- recipe(~., data = smith_tr)

test_that("character input", {
  skip_if_not_installed("janitor")
  cleaned <- rec %>% step_clean_levels(name, id = "")

  tidy_exp_un <- tibble(
    terms = c("name"),
    id = ""
  )
  expect_equal(tidy_exp_un, tidy(cleaned, number = 1))

  cleaned <- prep(cleaned, training = smith_tr[1:2, ])
  cleaned_tr <- bake(cleaned, new_data = NULL)
  cleaned_te <- bake(cleaned, new_data = smith_te)

  expect_equal(sum(grepl(" ", cleaned_tr$name)), 0)
  expect_equal(sum(levels(cleaned_tr$name) %in% smith_tr$name), 0)

  tidy_exp_tr <- tibble(
    terms = rep(c("name"), c(2)),
    original = c(
      "Anacostia Community Museum",
      "Arthur M. Sackler Gallery"
    ),
    value = c(
      "anacostia_community_museum",
      "arthur_m_sackler_gallery"
    ),
    id = ""
  )
  expect_equal(tidy_exp_tr, tidy(cleaned, number = 1))
  expect_equal(
    cleaned_tr$name,
    factor(c(
      "anacostia_community_museum",
      "arthur_m_sackler_gallery"
    ))
  )
  expect_equal(sum(is.na(cleaned_te$name)), 5)
})

test_that("factor input", {
  skip_if_not_installed("janitor")
  smith_tr <- Smithsonian[1:15, ]
  smith_tr$name <- as.factor(smith_tr$name)
  smith_te <- Smithsonian[16:20, ]
  smith_te$name <- as.factor(smith_te$name)

  rec <- recipe(~., data = smith_tr)

  cleaned <- rec %>% step_clean_levels(name)
  cleaned <- prep(cleaned, training = smith_tr)
  cleaned_tr <- bake(cleaned, new_data = smith_tr)
  cleaned_te <- bake(cleaned, new_data = smith_te)

  expect_equal(sum(grepl(" ", cleaned_tr$name)), 0)
  expect_equal(sum(levels(cleaned_tr$name) %in% smith_tr$name), 0)
  expect_equal(sum(is.na(cleaned_te$name)), 5)
})

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("janitor")
  rec <- recipe(~name, data = smith_tr) %>%
    step_clean_levels(name) %>%
    update_role(name, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = smith_tr, verbose = FALSE)

  expect_error(
    bake(trained, new_data = smith_tr[, -1]),
    class = "new_data_missing_column"
  )
})

test_that("printing", {
  skip_if_not_installed("janitor")
  rec <- rec %>% step_clean_levels(name)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_clean_levels(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_clean_levels(rec)

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
  rec <- step_clean_levels(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
