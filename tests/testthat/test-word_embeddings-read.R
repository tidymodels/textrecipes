write_bpemb <- function(lines) {
  path <- withr::local_tempfile(fileext = ".w2v.txt", .local_envir = parent.frame())
  writeLines(lines, path, useBytes = TRUE)
  path
}

test_that("read_embeddings_bpemb() reads the word2vec text format", {
  path <- write_bpemb(c("3 2", "<unk> 0.1 0.2", "▁the 0.3 0.4", "a 0.5 0.6"))

  res <- read_embeddings_bpemb(path)

  expect_identical(
    res,
    tibble::tibble(
      token = c("<unk>", "▁the", "a"),
      d1 = c(0.1, 0.3, 0.5),
      d2 = c(0.2, 0.4, 0.6)
    )
  )
})

test_that("read_embeddings_bpemb() keeps tokens that read.table() would mangle", {
  path <- write_bpemb(
    c("4 1", "\" 1", "# 2", "NA 3", "it's 4")
  )

  res <- read_embeddings_bpemb(path)

  expect_identical(res$token, c("\"", "#", "NA", "it's"))
  expect_identical(res$d1, c(1, 2, 3, 4))
})

test_that("read_embeddings_bpemb() output works with step_word_embeddings()", {
  path <- write_bpemb(c("2 2", "the 1 0", "cat 0 1"))
  embeddings <- read_embeddings_bpemb(path)

  rec <- recipe(~text, data = tibble(text = "the cat")) |>
    step_tokenize(text) |>
    step_word_embeddings(text, embeddings = embeddings)

  expect_no_error(res <- bake(prep(rec), new_data = NULL))
  expect_identical(res$wordembed_text_d1, 1)
  expect_identical(res$wordembed_text_d2, 1)
})

scrub_tempfile <- function(x) {
  gsub("'[^']*\\.w2v\\.txt'", "'<file>'", x)
}

test_that("read_embeddings_bpemb() errors on bad input", {
  expect_snapshot(read_embeddings_bpemb(1), error = TRUE)
  expect_snapshot(read_embeddings_bpemb("not-a-file.w2v.txt"), error = TRUE)

  expect_snapshot(
    read_embeddings_bpemb(write_bpemb(c("1 2 3", "a 1 2"))),
    error = TRUE,
    transform = scrub_tempfile
  )
  expect_snapshot(
    read_embeddings_bpemb(write_bpemb(c("5 2", "a 1 2"))),
    error = TRUE,
    transform = scrub_tempfile
  )
  expect_snapshot(
    read_embeddings_bpemb(write_bpemb(c("1 3", "a 1 2"))),
    error = TRUE,
    transform = scrub_tempfile
  )
})

write_model2vec <- function(vectors, tokens, tensor_name = "embeddings") {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  tensors <- list(torch::torch_tensor(vectors))
  names(tensors) <- tensor_name
  safetensors::safe_save_file(tensors, file.path(dir, "model.safetensors"))
  writeLines(tokens, file.path(dir, "vocab.txt"))
  dir
}

test_that("read_embeddings_model2vec() reads a model folder", {
  skip_if_not_installed("safetensors")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())

  vectors <- matrix(c(0.5, 1, 1.5, 2, 2.5, 3), nrow = 3)
  dir <- write_model2vec(vectors, c("[UNK]", "the", "##s"))

  res <- read_embeddings_model2vec(dir)

  expect_identical(
    res,
    tibble::tibble(
      token = c("[UNK]", "the", "##s"),
      d1 = c(0.5, 1, 1.5),
      d2 = c(2, 2.5, 3)
    )
  )
})

test_that("read_embeddings_model2vec() errors on bad input", {
  skip_if_not_installed("safetensors")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())

  expect_snapshot(read_embeddings_model2vec(1), error = TRUE)

  empty <- withr::local_tempdir()
  expect_snapshot(
    read_embeddings_model2vec(empty),
    error = TRUE,
    transform = function(x) gsub(empty, "<dir>", x, fixed = TRUE)
  )

  vectors <- matrix(1:4, nrow = 2)
  expect_snapshot(
    read_embeddings_model2vec(write_model2vec(vectors, c("a", "b", "c"))),
    error = TRUE
  )
  expect_snapshot(
    read_embeddings_model2vec(
      write_model2vec(vectors, c("a", "b"), tensor_name = "weights")
    ),
    error = TRUE
  )
})
