context("test-tokenmerge")

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

  expect_error(
    prep(rec)
  )
})
