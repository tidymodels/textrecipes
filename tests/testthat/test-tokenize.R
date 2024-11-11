test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec <- recipe(~., data = test_data)

test_that("output is list when length is 1 or 0", {
  data <- tibble(a = rep(c("a", ""), 20))

  data_rec <- recipe(~., data = data) %>%
    step_tokenize(a) %>%
    prep()

  expect_true(is.list(bake(data_rec, new_data = NULL, a)[, 1, drop = TRUE]))
})

test_that("tokenization is done correctly", {
  rec <- rec %>%
    step_tokenize(text)

  obj <- rec %>%
    prep()

  expect_equal(
    list(
      c("i", "would", "not", "eat", "them", "here", "or", "there"),
      c("i", "would", "not", "eat", "them", "anywhere"),
      c("i", "would", "not", "eat", "green", "eggs", "and", "ham"),
      c("i", "do", "not", "like", "them", "sam", "i", "am")
    ),
    bake(obj, new_data = NULL) %>% pull(text) %>% vctrs::field("tokens")
  )

  expect_equal(dim(recipes:::tidy.recipe(rec, 1)), c(1, 3))
  expect_equal(dim(recipes:::tidy.recipe(obj, 1)), c(1, 3))
})

test_that("step throws an error if unavaliable tokenizer is picked", {
  expect_snapshot(
    error = TRUE,
    rec %>%
      step_tokenize(text, token = "wrong") %>%
      prep()
  )
})

test_that("tokenization works with other built-in tokenizers", {
  rec <- rec %>%
    step_tokenize(text, token = "characters") %>%
    prep()

  expect_equal(
    tokenizers::tokenize_characters(test_data$text[1]),
    bake(rec, new_data = NULL) %>% slice(1) %>% pull(text) %>% vctrs::field("tokens")
  )
})

test_that("tokenization works with custom tokenizer", {
  rec <- rec %>%
    step_tokenize(text, custom_token = tokenizers::tokenize_characters) %>%
    prep()

  expect_equal(
    tokenizers::tokenize_characters(test_data$text[1]),
    bake(rec, new_data = NULL) %>%
      slice(1) %>%
      pull(text) %>%
      vctrs::field("tokens")
  )
})

test_that("arguments are passed using options argument", {
  rec <- rec %>%
    step_tokenize(text, options = list(lowercase = FALSE)) %>%
    prep()

  expect_equal(
    list(
      c("I", "would", "not", "eat", "them", "here", "or", "there"),
      c("I", "would", "not", "eat", "them", "anywhere"),
      c("I", "would", "not", "eat", "green", "eggs", "and", "ham"),
      c("I", "do", "not", "like", "them", "Sam", "I", "am")
    ),
    bake(rec, new_data = NULL) %>% pull(text) %>% vctrs::field("tokens")
  )
})

test_that("tokenization errors with wrong engines", {
  expect_snapshot(
    error = TRUE,
    rec %>%
      step_tokenize(text, engine = "fake") %>%
      prep()
  )
})

test_that("tokenization includes lemma attribute when avaliable", {
  skip_on_cran()
  skip_if_not_installed("spacyr")
  skip_if_no_python_or_no_spacy()

  expect_type(
    rec %>%
      step_tokenize(text, engine = "spacyr") %>%
      prep() %>%
      bake(new_data = NULL) %>%
      .$text %>%
      vctrs::field("lemma"),
    "list"
  )
})

test_that("tokenization doesn't includes lemma attribute when unavaliable", {
  expect_null(
    rec %>%
      step_tokenize(text) %>%
      prep() %>%
      bake(new_data = NULL) %>%
      .$text %>%
      attr("lemma")
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) %>%
    step_tokenize(all_predictors())
  rec_param <- tunable.step_tokenize(rec$steps[[1]])
  expect_equal(rec_param$name, c("token"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_tokenize(token = letters) %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_tokenize(engine = letters) %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_tokenize(custom_token = "yes") %>%
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    update_role(text, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)
  
  trained <- prep(rec, training = test_data, verbose = FALSE)
  
  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = test_data[, -1])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tokenize(rec)
  
  expect_snapshot(rec)
  
  rec <- prep(rec, mtcars)
  
  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_tokenize(rec1)
  
  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)
  
  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)
  
  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tokenize(rec)
  
  expect <- tibble(terms = character(), value = character(), id = character())
  
  expect_identical(tidy(rec, number = 1), expect)
  
  rec <- prep(rec, mtcars)
  
  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- rec %>%
    step_tokenize(text)
  
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) %>%
    step_tokenize(
      all_predictors(),
      token = hardhat::tune()
    )
  
  params <- extract_parameter_set_dials(rec)
  
  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})
