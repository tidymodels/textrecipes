test_that("step_lda works as intended", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("tate_text", package = "modeldata")

  n_rows <- 100
  n_top <- 10

  rec1 <- recipe(~ medium + artist, data = tate_text[seq_len(n_rows), ]) |>
    step_tokenize(medium) |>
    step_lda(medium, num_topics = n_top)

  obj <- rec1 |>
    prep()

  expect_equal(dim(bake(obj, new_data = NULL)), c(n_rows, n_top + 1))

  expect_equal(dim(tidy(rec1, 1)), c(1, 3))
  expect_equal(dim(tidy(obj, 1)), c(1, 3))
})

test_that("bake() is a deterministic, row-independent transform (#315)", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("tate_text", package = "modeldata")

  n_rows <- 100
  n_top <- 10

  rec1 <- recipe(~medium, data = tate_text[seq_len(n_rows), ]) |>
    step_tokenize(medium) |>
    step_lda(medium, num_topics = n_top)

  obj <- prep(rec1)

  lda_col_names <- paste0("lda_medium_", seq_len(n_top))

  # baking a single document alone must give the same result as baking that
  # same document as part of a larger batch (bake() must not re-fit topics
  # based on whatever else happens to be in `new_data`)
  row_alone <- bake(obj, new_data = tate_text[2, ])
  row_in_batch <- bake(obj, new_data = tate_text[seq_len(n_rows), ])[2, ]

  expect_equal(
    as.data.frame(row_alone[lda_col_names]),
    as.data.frame(row_in_batch[lda_col_names])
  )

  # topic weights for a non-empty document must sum to ~1
  row_sums <- rowSums(as.data.frame(row_in_batch[lda_col_names]))
  expect_equal(row_sums, 1, tolerance = 1e-8, ignore_attr = TRUE)

  all_baked <- bake(obj, new_data = NULL)
  all_row_sums <- rowSums(as.data.frame(all_baked[lda_col_names]))
  # documents with at least one token that survives vocabulary pruning
  # should have topic weights summing to ~1; documents with no surviving
  # tokens legitimately get all-zero topic weights
  nonzero <- all_row_sums > 0
  expect_true(any(nonzero))
  expect_true(all(abs(all_row_sums[nonzero] - 1) < 1e-8))
})

test_that("bake() works without warnings for a small batch (#315)", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("tate_text", package = "modeldata")

  n_rows <- 100
  n_top <- 10

  rec1 <- recipe(~medium, data = tate_text[seq_len(n_rows), ]) |>
    step_tokenize(medium) |>
    step_lda(medium, num_topics = n_top)

  obj <- prep(rec1)

  small_batch <- tate_text[seq_len(5), ]

  expect_no_warning(
    baked_small <- bake(obj, new_data = small_batch)
  )

  lda_col_names <- paste0("lda_medium_", seq_len(n_top))
  row_sums <- rowSums(as.data.frame(baked_small[lda_col_names]))
  expect_true(all(row_sums > 0))
})

test_that("step_lda works with num_topics argument", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  skip_if_not_installed("modeldata")
  data.table::setDTthreads(2) # because data.table uses all cores by default

  data("tate_text", package = "modeldata")

  n_rows <- 100
  n_top <- 100
  rec1 <- recipe(~ medium + artist, data = tate_text[seq_len(n_rows), ]) |>
    step_tokenize(medium) |>
    step_lda(medium, num_topics = n_top)

  obj <- rec1 |>
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

  rec <- recipe(~., data = dat) |>
    step_tokenize(text) |>
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
    recipe(~., data = mtcars) |>
      step_lda(num_topics = -4) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_lda(prefix = NULL) |>
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
    ~ medium + artist,
    data = tate_text[seq_len(n_rows), ]
  ) |>
    step_tokenize(medium) |>
    prep() |>
    bake(new_data = NULL)

  rec <- recipe(tokenized_test_data) |>
    update_role(medium, new_role = "predictor") |>
    step_lda(medium, num_topics = 10) |>
    update_role(medium, new_role = "potato") |>
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

  rec <- recipe(~medium, data = tate_text[seq_len(n_rows), ]) |>
    step_tokenize(medium) |>
    step_lda(medium, keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~medium, data = tate_text[seq_len(n_rows), ]) |>
    step_tokenize(medium) |>
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

  rec <- recipe(~medium, data = tate_text[seq_len(n_rows), ]) |>
    step_tokenize(medium) |>
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

  rec <- recipe(~., data = iris) |>
    step_tokenize(Species) |>
    step_lda(Species)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("data.table")
  data <- tibble(
    text = c(
      "I would not eat them here or there.",
      "I would not eat them anywhere.",
      "I do not like them, Sam-I-am."
    )
  )
  rec <- recipe(~text, data = data) |>
    step_tokenize(text) |>
    step_lda(text, num_topics = 2) |>
    prep()

  expect_identical(nrow(bake(rec, dplyr::slice(data, 1))), 1L)
  expect_identical(nrow(bake(rec, dplyr::slice(data, 0))), 0L)
})
