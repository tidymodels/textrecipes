library(recipes)

embeddings <- readRDS(test_path("emb-data", "embeddings.rds"))

sentence_embeddings_long <- readRDS(test_path("emb-data", "long.rds"))
sentence_embeddings_max <- readRDS(test_path("emb-data", "max.rds"))
sentence_embeddings_mean <- readRDS(test_path("emb-data", "mean.rds"))
sentence_embeddings_min <- readRDS(test_path("emb-data", "min.rds"))
sentence_embeddings_sum <- readRDS(test_path("emb-data", "sum.rds"))

eps <- if (capabilities("long.double")) {
  sqrt(.Machine$double.eps)
} else {
  0.1
}

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec_base <- recipe(~., data = test_data)

rec <- rec_base %>%
  step_tokenize(text) %>%
  step_word_embeddings(text, embeddings = embeddings)

obj <- rec %>%
  prep()

baked <- bake(obj, new_data = NULL)

test_that("step_word_embeddings adds the appropriate number of columns.", {
  ncol_given <- ncol(embeddings) - 1L
  ncol_baked <- baked %>%
    select(contains("wordembed_")) %>%
    ncol()
  expect_identical(ncol_baked, ncol_given)
})

test_that("step_word_embeddings gives numeric output.", {
  expect_true(
    baked %>%
      select(contains("wordembed")) %>%
      lapply(is.numeric) %>%
      unlist() %>%
      all()
  )
})


# Run the tests. ----------------------------------------------------------

test_that("step_word_embeddings tidy method works.", {
  rec_tidied <- tidy(rec, 2)
  obj_tidied <- tidy(obj, 2)
  expected_cols <- c("terms", "embeddings_rows", "aggregation", "id")

  expect_equal(dim(rec_tidied), c(1, 4))
  expect_equal(dim(obj_tidied), c(1, 4))
  expect_identical(colnames(rec_tidied), expected_cols)
  expect_identical(colnames(obj_tidied), expected_cols)
  expect_identical(rec_tidied$embeddings_rows, 17L)
  expect_identical(rec_tidied$aggregation, "sum")
})

test_that("step_word_embeddings aggregates vectors as expected.", {
  # By default, step_word_embeddings sums the vectors of the tokens it is given.
  expect_equal(
    as.data.frame(baked),
    as.data.frame(select(sentence_embeddings_sum, -text)),
    tolerance = eps
  )

  # Also allow the user to choose an aggregation function.
  baked_max <- rec_base %>%
    step_tokenize(text) %>%
    step_word_embeddings(
      text,
      embeddings = embeddings, aggregation = "max"
    ) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    as.data.frame(baked_max),
    as.data.frame(select(sentence_embeddings_max, -text)),
    tolerance = eps
  )

  baked_min <- rec_base %>%
    step_tokenize(text) %>%
    step_word_embeddings(
      text,
      embeddings = embeddings, aggregation = "min"
    ) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    as.data.frame(baked_min),
    as.data.frame(select(sentence_embeddings_min, -text)),
    tolerance = eps
  )

  baked_mean <- rec_base %>%
    step_tokenize(text) %>%
    step_word_embeddings(
      text,
      embeddings = embeddings, aggregation = "mean"
    ) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    as.data.frame(baked_mean),
    as.data.frame(select(sentence_embeddings_mean, -text)),
    tolerance = eps
  )
})

test_that("step_word_embeddings deals with missing words appropriately.", {
  new_text <- tibble(
    text = c(
      "I would not eat red beans and rice.",
      "I do not like them, they're not nice."
    )
  )
  expect_warning(
    bake(obj, new_data = new_text),
    NA
  )
  expect_warning(
    bake(obj, new_data = test_data),
    NA
  )

  new_text <- tibble(
    text = "aksjdf nagjli aslkfa"
  )
  expect_error(
    bake(obj, new_data = new_text),
    NA
  )
})

test_that("bake method errors when needed non-standard role columns are missing", {
  tokenized_test_data <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    prep() %>%
    bake(new_data = NULL)
  
  rec <- recipe(tokenized_test_data) %>%
    update_role(text, new_role = "predictor") %>%
    step_word_embeddings(text, embeddings = embeddings) %>%
    update_role(text, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)
  
  trained <- prep(rec, training = tokenized_test_data, verbose = FALSE)
  
  expect_error(bake(trained, new_data = tokenized_test_data[, -1]),
               class = "new_data_missing_column")
})

test_that("printing", {
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("NA tokens work.", {
  new_text <- tibble(
    text = c("am", "and", NA)
  )
  test_result <- bake(obj, new_data = new_text)
  expected_result <- rbind(
    bake(obj, new_data = new_text[1:2, ]),
    c(0, 0, 0, 0, 0)
  )
  expect_identical(test_result, expected_result)
})

test_that("Embeddings work with empty documents", {
  empty_data <- data.frame(text = "")

  expect_equal(
    recipe(~text, data = empty_data) %>%
      step_tokenize(text) %>%
      step_word_embeddings(text, embeddings = embeddings, aggregation = "sum") %>%
      prep() %>%
      bake(new_data = NULL) %>%
      as.numeric(),
    rep(0, 5)
  )

  expect_equal(
    recipe(~text, data = empty_data) %>%
      step_tokenize(text) %>%
      step_word_embeddings(text, embeddings = embeddings, aggregation = "mean") %>%
      prep() %>%
      bake(new_data = NULL) %>%
      as.numeric(),
    rep(0, 5)
  )

  expect_equal(
    recipe(~text, data = empty_data) %>%
      step_tokenize(text) %>%
      step_word_embeddings(text, embeddings = embeddings, aggregation = "min") %>%
      prep() %>%
      bake(new_data = NULL) %>%
      as.numeric(),
    rep(0, 5)
  )

  expect_equal(
    recipe(~text, data = empty_data) %>%
      step_tokenize(text) %>%
      step_word_embeddings(text, embeddings = embeddings, aggregation = "max") %>%
      prep() %>%
      bake(new_data = NULL) %>%
      as.numeric(),
    rep(0, 5)
  )
})

test_that("aggregation_default argument works", {
  empty_data <- data.frame(text = "")

  expect_equal(
    recipe(~text, data = empty_data) %>%
      step_tokenize(text) %>%
      step_word_embeddings(text,
        embeddings = embeddings, aggregation = "sum",
        aggregation_default = 3
      ) %>%
      prep() %>%
      bake(new_data = NULL) %>%
      as.numeric(),
    rep(3, 5)
  )

  expect_equal(
    recipe(~text, data = empty_data) %>%
      step_tokenize(text) %>%
      step_word_embeddings(text,
        embeddings = embeddings, aggregation = "mean",
        aggregation_default = 3
      ) %>%
      prep() %>%
      bake(new_data = NULL) %>%
      as.numeric(),
    rep(3, 5)
  )

  expect_equal(
    recipe(~text, data = empty_data) %>%
      step_tokenize(text) %>%
      step_word_embeddings(text,
        embeddings = embeddings, aggregation = "min",
        aggregation_default = 3
      ) %>%
      prep() %>%
      bake(new_data = NULL) %>%
      as.numeric(),
    rep(3, 5)
  )

  expect_equal(
    recipe(~text, data = empty_data) %>%
      step_tokenize(text) %>%
      step_word_embeddings(text,
        embeddings = embeddings, aggregation = "max",
        aggregation_default = 3
      ) %>%
      prep() %>%
      bake(new_data = NULL) %>%
      as.numeric(),
    rep(3, 5)
  )
})

test_that("keep_original_cols works", {
  koc_rec <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    step_word_embeddings(text, embeddings = embeddings, aggregation = "mean", 
                         keep_original_cols = TRUE)
  
  koc_trained <- prep(koc_rec, training = test_data, verbose = FALSE)
  
  koc_pred <- bake(koc_trained, new_data = test_data, all_predictors())
  
  expect_equal(
    colnames(koc_pred),
    c(
      "text", "wordembed_text_d1", "wordembed_text_d2", "wordembed_text_d3", 
      "wordembed_text_d4", "wordembed_text_d5"
    )
  )
})

test_that("can prep recipes with no keep_original_cols", {
  koc_rec <- recipe(~text, data = test_data) %>%
    step_tokenize(text) %>%
    step_word_embeddings(text, embeddings = embeddings, aggregation = "mean", 
                         keep_original_cols = TRUE)
  
  koc_rec$steps[[2]]$keep_original_cols <- NULL
  
  expect_snapshot(
    koc_trained <- prep(koc_rec, training = test_data, verbose = FALSE)
  )
  
  expect_error(
    pca_pred <- bake(koc_trained, new_data = test_data, all_predictors()),
    NA
  )
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_word_embeddings(rec1, embeddings = embeddings)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_word_embeddings(rec, embeddings = embeddings)

  expect_identical(
    tidy(rec, number = 1),
    tibble(
      terms = character(),
      embeddings_rows = integer(),
      aggregation = character(),
      id = character()
    )
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(
      terms = character(),
      embeddings_rows = integer(),
      aggregation = character(),
      id = character()
    )
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_word_embeddings(rec, embeddings = embeddings)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
