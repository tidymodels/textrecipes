test_that("download_embeddings_model2vec() downloads both files into path", {
  urls <- character()
  local_mocked_bindings(
    download_file = function(url, destfile) {
      urls <<- c(urls, url)
      writeLines(basename(url), destfile)
    }
  )
  path <- withr::local_tempdir()
  folder <- file.path(path, "nested", "model")

  res <- download_embeddings_model2vec("https://example.com/model/", folder)

  expect_identical(res, folder)
  expect_identical(
    urls,
    c(
      "https://example.com/model/model.safetensors",
      "https://example.com/model/vocab.txt"
    )
  )
  expect_identical(
    sort(list.files(folder)),
    c("model.safetensors", "vocab.txt")
  )
})

test_that("download_embeddings_model2vec() returns the path invisibly", {
  local_mocked_bindings(
    download_file = function(url, destfile) writeLines("", destfile)
  )

  expect_invisible(
    download_embeddings_model2vec("https://example.com", withr::local_tempdir())
  )
})

test_that("download_embeddings_model2vec() defaults to a new temporary folder", {
  local_mocked_bindings(
    download_file = function(url, destfile) writeLines("", destfile)
  )

  res <- download_embeddings_model2vec("https://example.com")

  expect_true(dir.exists(res))
  expect_identical(
    normalizePath(dirname(res)),
    normalizePath(tempdir())
  )
  unlink(res, recursive = TRUE)
})

test_that("download_embeddings_model2vec() output works with read_embeddings_model2vec()", {
  skip_if_not_installed("safetensors")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())

  source <- withr::local_tempdir()
  safetensors::safe_save_file(
    list(embeddings = torch::torch_tensor(matrix(c(1, 2, 3, 4), nrow = 2))),
    file.path(source, "model.safetensors")
  )
  writeLines(c("a", "b"), file.path(source, "vocab.txt"))

  local_mocked_bindings(
    download_file = function(url, destfile) {
      file.copy(file.path(source, basename(url)), destfile)
    }
  )

  res <- download_embeddings_model2vec("https://example.com") |>
    read_embeddings_model2vec()

  expect_identical(res$token, c("a", "b"))
})

test_that("download_embeddings_model2vec() errors informatively", {
  expect_snapshot(download_embeddings_model2vec(1), error = TRUE)
  expect_snapshot(
    download_embeddings_model2vec("https://example.com", path = 1),
    error = TRUE
  )

  local_mocked_bindings(
    download_file = function(url, destfile) stop("cannot open URL")
  )
  expect_snapshot(
    download_embeddings_model2vec("https://example.com", withr::local_tempdir()),
    error = TRUE
  )
})
