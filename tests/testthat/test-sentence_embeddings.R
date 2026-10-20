# A deterministic fake encoder. Counts how many strings it has been asked to
# embed, so tests can see what reaches the backend.
fake_embedder <- function(dimension = 3L) {
  calls <- new.env()
  calls$n <- 0L
  calls$batches <- list()

  fun <- function(x) {
    calls$n <- calls$n + length(x)
    calls$batches <- c(calls$batches, list(x))
    out <- vapply(
      x,
      function(s) nchar(s) + seq_len(dimension),
      numeric(dimension)
    )
    matrix(out, nrow = length(x), ncol = dimension, byrow = TRUE)
  }

  list(engine = embedding_custom(fun), calls = calls)
}

test_data <- tibble(
  text = c(
    "I would not eat them here or there.",
    "I would not eat them anywhere.",
    "I would not eat green eggs and ham.",
    "I do not like them, Sam-I-am."
  )
)

rec_base <- recipe(~., data = test_data)

test_that("step_sentence_embeddings adds the appropriate columns", {
  fake <- fake_embedder(dimension = 3L)

  baked <- rec_base |>
    step_sentence_embeddings(text, engine = fake$engine) |>
    prep() |>
    bake(new_data = NULL)

  expect_named(
    baked,
    c("sentembed_text_1", "sentembed_text_2", "sentembed_text_3")
  )
  expect_true(all(vapply(baked, is.numeric, logical(1))))
  expect_identical(nrow(baked), 4L)
  expect_equal(
    baked$sentembed_text_1,
    nchar(test_data$text) + 1
  )
})

test_that("prefix argument works", {
  fake <- fake_embedder(dimension = 2L)

  baked <- rec_base |>
    step_sentence_embeddings(text, engine = fake$engine, prefix = "emb") |>
    prep() |>
    bake(new_data = NULL)

  expect_named(baked, c("emb_text_1", "emb_text_2"))
})

test_that("multiple columns are embedded", {
  fake <- fake_embedder(dimension = 2L)
  dat <- tibble(a = c("x", "yy"), b = c("zzz", "wwww"))

  baked <- recipe(~., data = dat) |>
    step_sentence_embeddings(a, b, engine = fake$engine) |>
    prep() |>
    bake(new_data = NULL)

  expect_named(
    baked,
    c("sentembed_a_1", "sentembed_a_2", "sentembed_b_1", "sentembed_b_2")
  )
})

test_that("engine = embedding_custom() accepts formula lambdas", {
  engine <- embedding_custom(~ cbind(nchar(.x), nchar(.x) * 2))

  baked <- rec_base |>
    step_sentence_embeddings(text, engine = engine) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(baked$sentembed_text_2, nchar(test_data$text) * 2)
})

# Missing values and empty strings ---------------------------------------------

test_that("NA stays NA and is never sent to the engine", {
  fake <- fake_embedder()
  dat <- tibble(text = c("a", NA, "bb", NA))

  rec <- recipe(~., data = dat) |>
    step_sentence_embeddings(text, engine = fake$engine) |>
    prep()
  baked <- bake(rec, new_data = NULL)

  expect_identical(is.na(baked$sentembed_text_1), c(FALSE, TRUE, FALSE, TRUE))
  expect_false(any(is.na(unlist(fake$calls$batches))))
})

test_that("empty strings are embedded as they are", {
  fake <- fake_embedder()
  dat <- tibble(text = c("", "a"))

  baked <- recipe(~., data = dat) |>
    step_sentence_embeddings(text, engine = fake$engine) |>
    prep() |>
    bake(new_data = NULL)

  expect_false(anyNA(baked))
  expect_identical("", unlist(fake$calls$batches)[[1]])
})

test_that("errors when all text is missing, as the dimension is unknown", {
  fake <- fake_embedder()
  dat <- tibble(text = c(NA_character_, NA_character_))

  expect_snapshot(
    error = TRUE,
    recipe(~., data = dat) |>
      step_sentence_embeddings(text, engine = fake$engine) |>
      prep()
  )
})

test_that("bake handles all-NA new data after prep", {
  fake <- fake_embedder()

  rec <- rec_base |>
    step_sentence_embeddings(text, engine = fake$engine) |>
    prep()

  res <- bake(rec, new_data = tibble(text = c(NA_character_, NA_character_)))

  expect_identical(dim(res), c(2L, 3L))
  expect_true(all(is.na(unlist(res))))
})

# Session cache ----------------------------------------------------------------

test_that("embeddings are cached within the session", {
  fake <- fake_embedder()

  rec <- rec_base |>
    step_sentence_embeddings(text, engine = fake$engine) |>
    prep()

  # prep() embeds the training data, and retaining it bakes it again, so the
  # second pass has to come from the cache.
  expect_identical(fake$calls$n, 4L)

  bake(rec, new_data = NULL)
  bake(rec, new_data = test_data)
  expect_identical(fake$calls$n, 4L)

  # Only unseen strings reach the engine
  bake(rec, new_data = tibble(text = c(test_data$text[[1]], "brand new")))
  expect_identical(fake$calls$n, 5L)
  expect_identical(fake$calls$batches[[2]], "brand new")
})

test_that("duplicated strings are only embedded once", {
  fake <- fake_embedder()
  dat <- tibble(text = c("a", "a", "b", "a"))

  baked <- recipe(~., data = dat) |>
    step_sentence_embeddings(text, engine = fake$engine) |>
    prep() |>
    bake(new_data = NULL)

  expect_identical(fake$calls$n, 2L)
  expect_identical(baked$sentembed_text_1[c(1, 2, 4)], rep(2, 3))
})

test_that("different engines don't share cache entries", {
  engine_a <- embedding_custom(~ cbind(nchar(.x)))
  engine_b <- embedding_custom(~ cbind(nchar(.x) * 10))

  res_a <- rec_base |>
    step_sentence_embeddings(text, engine = engine_a) |>
    prep() |>
    bake(new_data = NULL)
  res_b <- rec_base |>
    step_sentence_embeddings(text, engine = engine_b) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(res_b$sentembed_text_1, res_a$sentembed_text_1 * 10)
})

test_that("embeddings are not stored in the recipe", {
  engine <- embedding_custom(~ cbind(nchar(.x), nchar(.x)))

  # Both recipes are built here, not in a helper, because the step's quosures
  # capture the calling environment, which would hold the training data.
  small <- tibble(text = paste("sentence", 1:10))
  large <- tibble(text = paste("sentence", 1:1000))

  rec_small <- recipe(~., data = small) |>
    step_sentence_embeddings(text, engine = engine) |>
    prep()
  rec_large <- recipe(~., data = large) |>
    step_sentence_embeddings(text, engine = engine) |>
    prep()

  expect_identical(
    length(serialize(rec_small$steps[[1]], NULL)),
    length(serialize(rec_large$steps[[1]], NULL))
  )
})

test_that("a recipe survives a saveRDS() round trip", {
  fake <- fake_embedder()

  rec <- rec_base |>
    step_sentence_embeddings(text, engine = fake$engine) |>
    prep()
  expected <- bake(rec, new_data = test_data)

  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(rec, path)
  restored <- readRDS(path)

  expect_equal(bake(restored, new_data = test_data), expected)
})

# Guards -----------------------------------------------------------------------

test_that("a changed dimension is caught when baking", {
  state <- new.env()
  state$dim <- 3L
  engine <- embedding_custom(function(x) {
    matrix(1, nrow = length(x), ncol = state$dim)
  })

  rec <- rec_base |>
    step_sentence_embeddings(text, engine = engine) |>
    prep()

  state$dim <- 5L

  expect_snapshot(
    error = TRUE,
    bake(rec, new_data = tibble(text = "something unseen"))
  )
})

test_that("engine output is validated", {
  not_matrix <- embedding_custom(function(x) "no")
  wrong_rows <- embedding_custom(function(x) matrix(1, nrow = 1, ncol = 2))
  no_cols <- embedding_custom(function(x) matrix(1, nrow = length(x), ncol = 0))

  expect_snapshot(error = TRUE, {
    prep(step_sentence_embeddings(rec_base, text, engine = not_matrix))
    prep(step_sentence_embeddings(rec_base, text, engine = wrong_rows))
    prep(step_sentence_embeddings(rec_base, text, engine = no_cols))
  })
})

test_that("engine failures are wrapped with the engine name", {
  failing <- embedding_custom(function(x) stop("server on fire"))

  expect_snapshot(
    error = TRUE,
    prep(step_sentence_embeddings(rec_base, text, engine = failing))
  )
})

test_that("a failing engine never returns partial output", {
  calls <- 0L
  flaky <- embedding_custom(function(x) {
    calls <<- calls + 1L
    if (calls > 1L) stop("second batch failed")
    matrix(1, nrow = length(x), ncol = 2)
  })

  rec <- rec_base |>
    step_sentence_embeddings(text, engine = flaky) |>
    prep()

  expect_error(bake(rec, new_data = tibble(text = c("new one", "new two"))))
})

test_that("factors are embedded as their labels", {
  fake <- fake_embedder()
  dat <- tibble(x = factor(c("a", "bb", NA)))

  baked <- recipe(~., data = dat) |>
    step_sentence_embeddings(x, engine = fake$engine) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(baked$sentembed_x_1, c(2, 3, NA))
})

test_that("non-text columns are rejected", {
  fake <- fake_embedder()

  expect_snapshot(
    error = TRUE,
    recipe(~., data = tibble(x = 1:3)) |>
      step_sentence_embeddings(x, engine = fake$engine) |>
      prep()
  )

  rec <- rec_base |>
    step_sentence_embeddings(text, engine = fake$engine) |>
    prep()

  expect_snapshot(
    error = TRUE,
    bake(rec, new_data = tibble(text = 1:2))
  )
})

test_that("bad args", {
  fake <- fake_embedder()

  expect_snapshot(error = TRUE, {
    step_sentence_embeddings(rec_base, text)
    step_sentence_embeddings(rec_base, text, engine = "ollama")
    step_sentence_embeddings(rec_base, text, engine = fake$engine, prefix = 1) |>
      prep()
    embedding_custom("not a function")
  })
})

# Engine -----------------------------------------------------------------------

test_that("embedding_custom() prints", {
  expect_snapshot(embedding_custom(function(x) x))
})

# Infrastructure ---------------------------------------------------------------

test_that("tidy method works", {
  fake <- fake_embedder()
  rec <- rec_base |>
    step_sentence_embeddings(text, engine = fake$engine, id = "test")

  expect_identical(
    tidy(rec, number = 1),
    tibble(
      terms = "text",
      engine = "custom function",
      model = NA_character_,
      revision = NA_character_,
      dimension = NA_integer_,
      id = "test"
    )
  )

  expect_identical(
    tidy(prep(rec), number = 1),
    tibble(
      terms = "text",
      engine = "custom function",
      model = NA_character_,
      revision = NA_character_,
      dimension = 3L,
      id = "test"
    )
  )
})

test_that("empty selection tidy method works", {
  fake <- fake_embedder()
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_sentence_embeddings(rec, engine = fake$engine)

  expect <- tibble(
    terms = character(),
    engine = character(),
    model = character(),
    revision = character(),
    dimension = integer(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty selection prep/bake is a no-op", {
  fake <- fake_embedder()
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_sentence_embeddings(rec1, engine = fake$engine)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  expect_identical(bake(rec1, mtcars), bake(rec2, mtcars))
  expect_identical(fake$calls$n, 0L)
})

test_that("empty printing", {
  fake <- fake_embedder()
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_sentence_embeddings(rec, engine = fake$engine)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("printing", {
  fake <- fake_embedder()
  rec <- rec_base |>
    step_sentence_embeddings(text, engine = fake$engine)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("required_pkgs() works", {
  fake <- fake_embedder()
  rec <- rec_base |>
    step_sentence_embeddings(text, engine = fake$engine)

  expect_identical(required_pkgs(rec$steps[[1]]), "textrecipes")
})

test_that("keep_original_cols works", {
  fake <- fake_embedder(dimension = 2L)
  new_names <- paste0("sentembed_text_", 1:2)

  res <- rec_base |>
    step_sentence_embeddings(text, engine = fake$engine) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(colnames(res), new_names)

  res <- rec_base |>
    step_sentence_embeddings(
      text,
      engine = fake$engine,
      keep_original_cols = TRUE
    ) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(colnames(res), c("text", new_names))
})

test_that("keep_original_cols - can prep recipes with it missing", {
  fake <- fake_embedder()
  rec <- rec_base |>
    step_sentence_embeddings(text, engine = fake$engine)

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = test_data)
  )
})

test_that("check_name() is used", {
  fake <- fake_embedder()
  dat <- test_data
  dat$sentembed_text_1 <- dat$text

  rec <- recipe(~., data = dat) |>
    step_sentence_embeddings(text, engine = fake$engine)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("bake method errors when needed non-standard role columns are missing", {
  fake <- fake_embedder()
  rec <- recipe(test_data) |>
    update_role(text, new_role = "predictor") |>
    step_sentence_embeddings(text, engine = fake$engine) |>
    update_role(text, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = test_data, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = test_data[, -1])
  )
})

test_that("0 and 1 rows data work in bake method", {
  fake <- fake_embedder()
  rec <- rec_base |>
    step_sentence_embeddings(text, engine = fake$engine) |>
    prep()

  expect_identical(nrow(bake(rec, dplyr::slice(test_data, 1))), 1L)
  expect_identical(nrow(bake(rec, dplyr::slice(test_data, 0))), 0L)
  expect_identical(ncol(bake(rec, dplyr::slice(test_data, 0))), 3L)
})
