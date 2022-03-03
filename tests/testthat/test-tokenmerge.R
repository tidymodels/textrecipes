library(textrecipes)
library(recipes)

test_data <- tibble(
  text1 = c(
    "I would not eat them here or there.",
    "I would not eat them anywhere.",
    "I would not eat green eggs and ham.",
    "I do not like them, Sam-I-am."
  ),
  text2 = c(
    "I would not eat them here or there.",
    "I would not eat them anywhere.",
    "I would not eat green eggs and ham.",
    "I do not like them, Sam-I-am."
  )
)

rec <- recipe(~., data = test_data)

test_that("merging is done correctly", {
  rec <- rec %>%
    step_tokenize(text1, text2) %>%
    step_tokenmerge(text1, text2)

  obj <- rec %>%
    prep()

  juiced_data <- bake(obj, new_data = NULL)

  rec2 <- recipe(~., data = test_data) %>%
    step_tokenize(text1, text2) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    lengths(vctrs::field(juiced_data$tokenmerge, "tokens")),
    lengths(vctrs::field(rec2$text1, "tokens")) +
      lengths(vctrs::field(rec2$text2, "tokens"))
  )

  expect_equal(dim(recipes:::tidy.recipe(rec, 1)), c(2, 3))
  expect_equal(dim(recipes:::tidy.recipe(obj, 1)), c(2, 3))
})

test_that("it complains when the selected column isn't a tokenlist", {
  rec <- rec %>%
    step_tokenmerge(text1, text2)

  expect_snapshot(error = TRUE,
    prep(rec)
  )
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_tokenmerge(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tokenmerge(rec)

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
  rec <- step_tokenmerge(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
