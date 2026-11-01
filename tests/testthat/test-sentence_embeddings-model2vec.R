skip_if_no_model2vec <- function() {
  skip_if_not_installed("wordpiece")
  skip_if_not_installed("safetensors")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
}

# A model with 2 dimensions where "the" and "cat" point along the axes.
write_toy_model2vec <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  vectors <- matrix(
    c(
      9, 9, # [UNK], must be ignored
      1, 0, # the
      0, 1, # cat
      3, 4 # ##s
    ),
    ncol = 2,
    byrow = TRUE
  )
  tensors <- list(embeddings = torch::torch_tensor(vectors))
  safetensors::safe_save_file(tensors, file.path(dir, "model.safetensors"))
  writeLines(c("[UNK]", "the", "cat", "##s"), file.path(dir, "vocab.txt"))
  dir
}

test_that("embedding_model2vec() creates an engine", {
  engine <- embedding_model2vec("some/path")

  expect_s3_class(engine, "embedding_model2vec")
  expect_s3_class(engine, "sentence_embedding_engine")
  expect_identical(engine_label(engine), "model2vec")
  expect_identical(engine_model(engine), "path")
  expect_identical(engine_revision(engine), NA_character_)
  expect_identical(
    engine_required_pkgs(engine),
    c("wordpiece", "safetensors", "torch")
  )
})

test_that("embedding_model2vec() errors on bad arguments", {
  expect_snapshot(embedding_model2vec(1), error = TRUE)
  expect_snapshot(embedding_model2vec(""), error = TRUE)
  expect_snapshot(embedding_model2vec("a", normalize = "yes"), error = TRUE)
  expect_snapshot(embedding_model2vec("a", max_tokens = 0), error = TRUE)
})

test_that("embedding_model2vec() averages and normalizes token vectors", {
  skip_if_no_model2vec()
  withr::defer(clear_model2vec_model_cache())
  dir <- write_toy_model2vec()

  raw <- engine_embed(
    embedding_model2vec(dir, normalize = FALSE),
    c("the cat", "the", "unknownword", "the cats")
  )
  expect_equal(raw[1, ], c(0.5, 0.5))
  expect_equal(raw[2, ], c(1, 0))
  # No known tokens gives zeros
  expect_equal(raw[3, ], c(0, 0))
  # "cats" is "cat" + "##s", so three tokens are averaged
  expect_equal(raw[4, ], c(4 / 3, 5 / 3))

  normalized <- engine_embed(embedding_model2vec(dir), c("the cat", "the", "zzz"))
  expect_equal(sqrt(sum(normalized[1, ]^2)), 1)
  expect_equal(normalized[2, ], c(1, 0))
  expect_equal(normalized[3, ], c(0, 0))
})

test_that("embedding_model2vec() ignores tokens after max_tokens", {
  skip_if_no_model2vec()
  withr::defer(clear_model2vec_model_cache())
  dir <- write_toy_model2vec()

  res <- engine_embed(
    embedding_model2vec(dir, normalize = FALSE, max_tokens = 1),
    "the cat"
  )
  expect_equal(res[1, ], c(1, 0))
})

test_that("embedding_model2vec() works in step_sentence_embeddings()", {
  skip_if_no_model2vec()
  withr::defer(clear_model2vec_model_cache())
  withr::defer(clear_sentence_embedding_cache())
  dir <- write_toy_model2vec()

  rec <- recipe(~text, data = tibble(text = c("the", "cat", NA))) |>
    step_sentence_embeddings(text, engine = embedding_model2vec(dir))
  prepped <- prep(rec)
  res <- bake(prepped, new_data = NULL)

  expect_identical(names(res), c("sentembed_text_1", "sentembed_text_2"))
  expect_equal(res$sentembed_text_1, c(1, 0, NA))
  expect_equal(res$sentembed_text_2, c(0, 1, NA))

  expect_identical(tidy(prepped, number = 1)$model, basename(dir))
  expect_identical(tidy(prepped, number = 1)$dimension, 2L)
})

test_that("embedding_model2vec() errors informatively when the model is missing", {
  skip_if_no_model2vec()
  withr::defer(clear_sentence_embedding_cache())
  empty <- withr::local_tempdir()

  rec <- recipe(~text, data = tibble(text = "the")) |>
    step_sentence_embeddings(text, engine = embedding_model2vec(empty))

  expect_error(prep(rec), "model2vec")
})
