test_that("step_lda works as intended", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("tate_text", package = "modeldata")

  n_rows <- 100
  n_top <- 10

  rec1 <- recipe(~medium + artist, data = tate_text[seq_len(n_rows), ]) %>%
    step_tokenize(medium) %>%
    step_lda(medium, num_topics = n_top)

  obj <- rec1 %>%
    prep()

  expect_equal(dim(bake(obj, new_data = NULL)), c(n_rows, n_top + 1))

  expect_equal(dim(tidy(rec1, 1)), c(1, 3))
  expect_equal(dim(tidy(obj, 1)), c(1, 3))
})

test_that("step_lda works with num_topics argument", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("tate_text", package = "modeldata")

  n_rows <- 100
  n_top <- 100
  rec1 <- recipe(~medium + artist, data = tate_text[seq_len(n_rows), ]) %>%
    step_tokenize(medium) %>%
    step_lda(medium, num_topics = n_top)

  obj <- rec1 %>%
    prep()

  expect_equal(dim(bake(obj, new_data = NULL)), c(n_rows, n_top + 1))
})

test_that("check_name() is used", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("tate_text", package = "modeldata")

  dat <- tate_text[seq_len(100), ]
  dat$text <- dat$medium
  dat$lda_text_1 <- dat$text

  rec <- recipe(~., data = dat) %>%
    step_tokenize(text) %>%
    step_lda(text)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("bad args", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_lda(num_topics = -4) %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_lda(prefix = NULL) %>%
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("tate_text", package = "modeldata")

  n_rows <- 100

  tokenized_test_data <- recipe(
    ~medium + artist,
    data = tate_text[seq_len(n_rows), ]
  ) %>%
    step_tokenize(medium) %>%
    prep() %>%
    bake(new_data = NULL)

  rec <- recipe(tokenized_test_data) %>%
    update_role(medium, new_role = "predictor") %>%
    step_lda(medium, num_topics = 10) %>%
    update_role(medium, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = tokenized_test_data, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = tokenized_test_data[, -1])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_lda(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_lda(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_lda(rec)

  expect <- tibble(
    terms = character(),
    num_topics = integer(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("tate_text", package = "modeldata")

  new_names <- paste0("lda_medium_", 1:10)

  n_rows <- 100

  rec <- recipe(~medium, data = tate_text[seq_len(n_rows), ]) %>%
    step_tokenize(medium) %>%
    step_lda(medium, keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~medium, data = tate_text[seq_len(n_rows), ]) %>%
    step_tokenize(medium) %>%
    step_lda(medium, keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("medium", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("tate_text", package = "modeldata")

  n_rows <- 100

  rec <- recipe(~medium, data = tate_text[seq_len(n_rows), ]) %>%
    step_tokenize(medium) %>%
    step_lda(medium, keep_original_cols = TRUE)

  rec$steps[[2]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = tate_text[seq_len(n_rows), ])
  )
})

test_that("printing", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  rec <- recipe(~., data = iris) %>%
    step_tokenize(Species) %>%
    step_lda(Species)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
