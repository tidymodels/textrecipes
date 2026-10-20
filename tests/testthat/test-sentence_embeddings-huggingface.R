skip_if_not_installed("reticulate")

# A fake sentence-transformers model. The embedding of a text is
# c(nchar(text), 1, 2) so results are easy to check, and every call to
# `encode()` is recorded so tests can inspect what was sent.
fake_hf_model <- function() {
  state <- new.env()
  state$calls <- list()

  model <- list(
    encode = function(
      x,
      batch_size,
      normalize_embeddings,
      convert_to_numpy,
      show_progress_bar
    ) {
      state$calls <- c(
        state$calls,
        list(list(
          x = unlist(x),
          batch_size = batch_size,
          normalize_embeddings = normalize_embeddings
        ))
      )
      # A single string is what reticulate makes of a character vector of length
      # 1, and sentence-transformers then returns one vector instead of a matrix.
      if (is.character(x) && length(x) == 1L) {
        return(c(nchar(x), 1, 2))
      }
      x <- unlist(x)
      cbind(nchar(x), 1, 2)
    }
  )

  list(model = model, state = state)
}

# Mocks the Python boundary. `loads` records the settings of each model load.
local_fake_huggingface <- function(
  fake = fake_hf_model(),
  module = TRUE,
  device_ok = TRUE,
  env = parent.frame()
) {
  # The caches outlive a test, and would hide calls from the mocks.
  clear_sentence_embedding_cache()
  clear_huggingface_model_cache()

  loads <- new.env()
  loads$engines <- list()

  local_mocked_bindings(
    huggingface_avoid_openmp_conflict = function() invisible(),
    huggingface_module_available = function() module,
    huggingface_device_available = function(device) device_ok,
    huggingface_sentence_transformer = function(engine) {
      loads$engines <- c(loads$engines, list(engine))
      fake$model
    },
    .env = env
  )

  list(fake = fake, loads = loads)
}

test_data <- tibble(text = c("a", "bb", "ccc", "dddd"))

rec_base <- recipe(~., data = test_data)

engine <- embedding_huggingface(
  "org/model",
  revision = "abc123",
  device = "cpu"
)

test_that("embedding_huggingface() works through step_sentence_embeddings()", {
  local_fake_huggingface()

  baked <- rec_base |>
    step_sentence_embeddings(text, engine = engine) |>
    prep() |>
    bake(new_data = NULL)

  expect_named(
    baked,
    c("sentembed_text_1", "sentembed_text_2", "sentembed_text_3")
  )
  expect_equal(baked$sentembed_text_1, c(1, 2, 3, 4))
  expect_equal(baked$sentembed_text_2, rep(1, 4))
  expect_equal(baked$sentembed_text_3, rep(2, 4))
})

test_that("settings are passed on to the model", {
  mocks <- local_fake_huggingface()

  eng <- embedding_huggingface(
    "org/model",
    revision = "abc123",
    device = "cpu",
    text_prefix = "query: ",
    normalize = TRUE,
    batch_size = 8
  )
  rec_base |>
    step_sentence_embeddings(text, engine = eng) |>
    prep()

  call <- mocks$fake$state$calls[[1]]
  expect_identical(call$x, paste0("query: ", test_data$text))
  expect_true(call$normalize_embeddings)
  expect_identical(call$batch_size, 8L)

  load <- mocks$loads$engines[[1]]
  expect_identical(load$model, "org/model")
  expect_identical(load$revision, "abc123")
  expect_identical(load$device, "cpu")
})

test_that("the model is loaded once per session and settings", {
  mocks <- local_fake_huggingface()

  rec <- rec_base |>
    step_sentence_embeddings(text, engine = engine) |>
    prep()
  # New text isn't in the embedding cache, so the model is used again
  bake(rec, new_data = tibble(text = c("x", "yy")))
  bake(rec, new_data = tibble(text = c("zzz")))

  expect_length(mocks$fake$state$calls, 3L)
  expect_length(mocks$loads$engines, 1L)

  other <- embedding_huggingface("org/model", revision = "def456", device = "cpu")
  rec_base |>
    step_sentence_embeddings(text, engine = other) |>
    prep()

  expect_length(mocks$loads$engines, 2L)
})

test_that("the cache key depends on what changes the embeddings", {
  key <- function(...) {
    engine_cache_key(embedding_huggingface("org/model", "abc123", ...))
  }

  expect_identical(key(), key(device = "cpu", batch_size = 4))
  expect_false(identical(key(), key(normalize = TRUE)))
  expect_false(identical(key(), key(text_prefix = "query: ")))
  expect_false(
    identical(
      key(),
      engine_cache_key(embedding_huggingface("org/model", "def456"))
    )
  )
  expect_false(
    identical(
      key(),
      engine_cache_key(embedding_huggingface("org/other", "abc123"))
    )
  )
})

test_that("tidy() and required_pkgs() describe the engine", {
  local_fake_huggingface()

  rec <- rec_base |>
    step_sentence_embeddings(text, engine = engine, id = "test") |>
    prep()

  expect_identical(
    tidy(rec, number = 1),
    tibble(
      terms = "text",
      engine = "huggingface",
      model = "org/model",
      revision = "abc123",
      dimension = 3L,
      id = "test"
    )
  )
  expect_identical(
    required_pkgs(rec$steps[[1]]),
    c("textrecipes", "reticulate")
  )
})

test_that("a prepped recipe stores the reference, not a model", {
  local_fake_huggingface()

  rec <- rec_base |>
    step_sentence_embeddings(text, engine = engine) |>
    prep()

  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(rec, path)
  restored <- readRDS(path)

  expect_identical(restored$steps[[1]]$engine$model, "org/model")
  expect_lt(file.size(path), 50000)

  expect_equal(
    bake(restored, new_data = tibble(text = "ee")),
    tibble(sentembed_text_1 = 2, sentembed_text_2 = 1, sentembed_text_3 = 2)
  )
})

test_that("embedding_huggingface() validates its arguments", {
  expect_snapshot(embedding_huggingface(1, "abc"), error = TRUE)
  expect_snapshot(embedding_huggingface("", "abc"), error = TRUE)
  expect_snapshot(embedding_huggingface("org/model"), error = TRUE)
  expect_snapshot(embedding_huggingface("org/model", ""), error = TRUE)
  expect_snapshot(
    embedding_huggingface("org/model", "abc", device = "cuda:x"),
    error = TRUE
  )
  expect_snapshot(
    embedding_huggingface("org/model", "abc", batch_size = 0),
    error = TRUE
  )

  expect_no_error(embedding_huggingface("org/model", "abc", device = "cuda:1"))
  expect_no_error(embedding_huggingface("org/model", "abc", device = "mps"))
})

test_that("the engine prints its label", {
  expect_snapshot(engine)
})

# Guards -----------------------------------------------------------------------

test_that("a missing Python module is reported with how to fix it", {
  local_fake_huggingface(module = FALSE)

  expect_snapshot(
    rec_base |>
      step_sentence_embeddings(text, engine = engine) |>
      prep(),
    error = TRUE
  )
  expect_error(
    engine_check(engine),
    class = "huggingface_module_missing"
  )
})

test_that("an unavailable device is reported", {
  local_fake_huggingface(device_ok = FALSE)

  eng <- embedding_huggingface("org/model", "abc123", device = "cuda")
  expect_snapshot(
    rec_base |>
      step_sentence_embeddings(text, engine = eng) |>
      prep(),
    error = TRUE
  )
  expect_error(engine_check(eng), class = "huggingface_device_unavailable")
})

test_that("a model that can't be loaded is reported with model and revision", {
  local_fake_huggingface()
  local_mocked_bindings(
    huggingface_sentence_transformer = function(engine) {
      stop("404 Client Error: Repository Not Found")
    }
  )

  expect_snapshot(
    rec_base |>
      step_sentence_embeddings(text, engine = engine) |>
      prep(),
    error = TRUE
  )
  expect_error(engine_check(engine), class = "huggingface_model_not_found")
})

test_that("a failure while embedding aborts the whole bake", {
  fake <- fake_hf_model()
  fake$model$encode <- function(x, ...) stop("out of memory")
  local_fake_huggingface(fake = fake)

  expect_snapshot(
    rec_base |>
      step_sentence_embeddings(text, engine = engine) |>
      prep(),
    error = TRUE
  )
})

test_that("a changed embedding dimension is caught when baking", {
  local_fake_huggingface()

  rec <- rec_base |>
    step_sentence_embeddings(text, engine = engine) |>
    prep()

  fake <- fake_hf_model()
  fake$model$encode <- function(x, ...) cbind(nchar(x), 1)
  local_fake_huggingface(fake = fake)

  expect_error(
    bake(rec, new_data = tibble(text = "new")),
    class = "embedding_dimension_changed"
  )
})

test_that("a single text still gives a matrix", {
  local_fake_huggingface()

  baked <- recipe(~text, data = tibble(text = "abc")) |>
    step_sentence_embeddings(text, engine = engine) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(baked$sentembed_text_1, 3)
  expect_identical(ncol(baked), 3L)
})

test_that("missing text isn't sent to the model", {
  mocks <- local_fake_huggingface()

  baked <- recipe(~text, data = tibble(text = c("a", NA, "bb"))) |>
    step_sentence_embeddings(text, engine = engine) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(baked$sentembed_text_1, c(1, NA, 2))
  expect_identical(mocks$fake$state$calls[[1]]$x, c("a", "bb"))
})

# OpenMP on macOS --------------------------------------------------------------

test_that("OpenMP variables are set on macOS when they are unset", {
  local_mocked_bindings(is_macos = function() TRUE)
  withr::local_envvar(KMP_DUPLICATE_LIB_OK = NA, OMP_NUM_THREADS = NA)

  expect_snapshot(huggingface_avoid_openmp_conflict())

  expect_identical(Sys.getenv("KMP_DUPLICATE_LIB_OK"), "TRUE")
  expect_identical(Sys.getenv("OMP_NUM_THREADS"), "1")
})

test_that("OpenMP variables the user has set are left alone", {
  local_mocked_bindings(is_macos = function() TRUE)
  withr::local_envvar(KMP_DUPLICATE_LIB_OK = NA, OMP_NUM_THREADS = "4")

  expect_snapshot(huggingface_avoid_openmp_conflict())

  expect_identical(Sys.getenv("KMP_DUPLICATE_LIB_OK"), "TRUE")
  expect_identical(Sys.getenv("OMP_NUM_THREADS"), "4")
})

test_that("nothing happens when both OpenMP variables are already set", {
  local_mocked_bindings(is_macos = function() TRUE)
  withr::local_envvar(KMP_DUPLICATE_LIB_OK = "FALSE", OMP_NUM_THREADS = "2")

  expect_no_message(huggingface_avoid_openmp_conflict())

  expect_identical(Sys.getenv("KMP_DUPLICATE_LIB_OK"), "FALSE")
  expect_identical(Sys.getenv("OMP_NUM_THREADS"), "2")
})

test_that("OpenMP variables are not touched outside macOS", {
  local_mocked_bindings(is_macos = function() FALSE)
  withr::local_envvar(KMP_DUPLICATE_LIB_OK = NA, OMP_NUM_THREADS = NA)

  expect_no_message(huggingface_avoid_openmp_conflict())

  expect_identical(Sys.getenv("KMP_DUPLICATE_LIB_OK"), "")
  expect_identical(Sys.getenv("OMP_NUM_THREADS"), "")
})

# Live -------------------------------------------------------------------------

test_that("embedding_huggingface() works with a real model", {
  skip_on_cran()
  # Starting Python is what fails on macOS, so this has to come before the first
  # reticulate call.
  withr::local_envvar(KMP_DUPLICATE_LIB_OK = NA, OMP_NUM_THREADS = NA)
  suppressMessages(huggingface_avoid_openmp_conflict())
  skip_if_not(
    reticulate::py_module_available("sentence_transformers"),
    "sentence_transformers isn't available"
  )

  clear_sentence_embedding_cache()
  clear_huggingface_model_cache()

  eng <- embedding_huggingface(
    "sentence-transformers/all-MiniLM-L6-v2",
    revision = "1110a243fdf4706b3f48f1d95db1a4f5529b4d41",
    device = "cpu"
  )

  baked <- recipe(~text, data = tibble(text = "The cat sat on the mat.")) |>
    step_sentence_embeddings(text, engine = eng) |>
    prep() |>
    bake(new_data = NULL)

  expect_identical(ncol(baked), 384L)
  # Values from sentence-transformers 6.1.0 for this model and revision
  expect_equal(
    unlist(baked[1, 1:4], use.names = FALSE),
    c(0.13024, -0.01577, -0.03672, 0.05799),
    tolerance = 1e-3
  )
  expect_equal(sum(unlist(baked[1, ])^2), 1, tolerance = 1e-4)
})
