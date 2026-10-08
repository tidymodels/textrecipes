test_that("character input", {
  skip_if_not_installed("janitor")
  skip_if_not_installed("modeldata")

  data("Smithsonian", package = "modeldata")
  smith_tr <- Smithsonian[1:15, ]
  smith_te <- Smithsonian[16:20, ]

  cleaned <- recipe(~., data = smith_tr) |>
    step_clean_levels(name, id = "")

  tidy_exp_un <- tibble(
    terms = c("name"),
    id = ""
  )
  expect_equal(tidy_exp_un, tidy(cleaned, number = 1))

  cleaned <- prep(cleaned, training = smith_tr[1:2, ])
  cleaned_tr <- bake(cleaned, new_data = NULL)
  cleaned_te <- bake(cleaned, new_data = smith_te)

  expect_equal(sum(grepl(" ", cleaned_tr$name)), 0)
  expect_equal(sum(is.na(cleaned_tr$name)), 0)
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
    factor(
      c(
        "anacostia_community_museum",
        "arthur_m_sackler_gallery"
      )
    )
  )
  expect_equal(sum(is.na(cleaned_te$name)), 5)
})

test_that("factor input", {
  skip_if_not_installed("janitor")
  skip_if_not_installed("modeldata")

  data("Smithsonian", package = "modeldata")
  smith_tr <- Smithsonian[1:15, ]
  smith_tr$name <- as.factor(smith_tr$name)
  smith_te <- Smithsonian[16:20, ]
  smith_te$name <- as.factor(smith_te$name)

  rec <- recipe(~., data = smith_tr)

  cleaned <- rec |> step_clean_levels(name)
  cleaned <- prep(cleaned, training = smith_tr)
  cleaned_tr <- bake(cleaned, new_data = smith_tr)
  cleaned_te <- bake(cleaned, new_data = smith_te)

  expect_equal(sum(grepl(" ", cleaned_tr$name)), 0)
  expect_equal(sum(levels(cleaned_tr$name) %in% smith_tr$name), 0)
  expect_equal(sum(is.na(cleaned_te$name)), 5)
})

test_that("character columns get a trained lookup table and are cleaned consistently (#320)", {
  skip_if_not_installed("janitor")

  dat_tr <- tibble::tibble(name = c("Foo Bar", "Foo Bar", "Baz Qux"))
  dat_te <- tibble::tibble(name = c("Foo Bar", "New Value"))

  rec <- recipe(~., data = dat_tr, strings_as_factors = FALSE) |>
    step_clean_levels(name) |>
    prep()

  baked_tr <- bake(rec, new_data = NULL)

  # identical inputs must clean identically regardless of row position
  expect_equal(baked_tr$name[1], baked_tr$name[2])
  expect_equal(baked_tr$name, c("foo_bar", "foo_bar", "baz_qux"))

  # tidy() reports a non-empty mapping for character columns
  tidy_res <- tidy(rec, number = 1)
  expect_equal(nrow(tidy_res), 2)

  # novel values not seen at prep time become NA at bake time
  baked_te <- bake(rec, new_data = dat_te)
  expect_equal(baked_te$name[1], "foo_bar")
  expect_true(is.na(baked_te$name[2]))
})

test_that("backwards compatibility with unnamed `clean` still cleans data (#321)", {
  skip_if_not_installed("janitor")

  dat <- tibble::tibble(name = factor(c("Foo Bar", "Baz Qux")))

  rec <- recipe(~., data = dat) |>
    step_clean_levels(name) |>
    prep()

  # simulate a legacy trained object where `clean` lost its outer names
  rec$steps[[1]]$clean <- unname(rec$steps[[1]]$clean)

  baked <- bake(rec, new_data = dat)

  expect_equal(as.character(baked$name), c("foo_bar", "baz_qux"))
})

test_that("columns argument is backwards compatible", {
  skip_if_not_installed("janitor")

  dat <- tibble::tibble(name = factor(c("Foo Bar", "Baz Qux")))

  rec <- recipe(~., data = dat) |>
    step_clean_levels(name) |>
    prep()

  exp <- bake(rec, new_data = dat)

  # simulate a legacy trained object made before the `columns` field existed
  rec$steps[[1]]$columns <- NULL

  expect_identical(
    bake(rec, new_data = dat),
    exp
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("janitor")
  skip_if_not_installed("modeldata")

  data("Smithsonian", package = "modeldata")
  smith_tr <- Smithsonian[1:15, ]

  rec <- recipe(~name, data = smith_tr) |>
    step_clean_levels(name) |>
    update_role(name, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = smith_tr, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = smith_tr[, -1])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_clean_levels(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
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

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  skip_if_not_installed("janitor")
  rec <- recipe(~., data = iris) |>
    step_clean_levels(Species)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  skip_if_not_installed("janitor")
  data <- tibble(x = factor(c("a b", "c d", "e f")))
  rec <- recipe(~x, data = data) |>
    step_clean_levels(x) |>
    prep()

  expect_identical(nrow(bake(rec, dplyr::slice(data, 1))), 1L)
  expect_identical(nrow(bake(rec, dplyr::slice(data, 0))), 0L)
})
