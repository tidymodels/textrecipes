skip_if_not_installed("httr2")

# A fake Ollama server. `models` is what /api/tags lists, and the embedding of a
# text is c(nchar(text), 1, 2) so results are easy to check. All requests are
# recorded so tests can inspect what was sent.
fake_ollama <- function(models = "all-minilm:latest", embed = NULL) {
  # The session cache outlives a test, and would hide requests from the mock.
  clear_sentence_embedding_cache()

  state <- new.env()
  state$requests <- list()

  embed <- embed %||%
    function(req, input) {
      lapply(input, function(s) list(nchar(s), 1, 2))
    }

  mock <- function(req) {
    state$requests <- c(state$requests, list(req))
    path <- httr2::url_parse(req$url)$path

    if (path == "/api/tags") {
      return(httr2::response_json(
        body = list(models = lapply(models, function(m) list(name = m)))
      ))
    }

    if (path == "/api/embed") {
      input <- unlist(req$body$data$input)
      return(httr2::response_json(body = list(embeddings = embed(req, input))))
    }

    httr2::response(status_code = 404L)
  }

  list(mock = mock, state = state)
}

embed_requests <- function(fake) {
  Filter(
    function(req) endsWith(req$url, "/api/embed"),
    fake$state$requests
  )
}

test_data <- tibble(text = c("a", "bb", "ccc", "dddd"))

rec_base <- recipe(~., data = test_data)

test_that("embedding_ollama() works through step_sentence_embeddings()", {
  fake <- fake_ollama()
  engine <- embedding_ollama("all-minilm")

  baked <- httr2::with_mocked_responses(fake$mock, {
    rec_base |>
      step_sentence_embeddings(text, engine = engine) |>
      prep() |>
      bake(new_data = NULL)
  })

  expect_named(
    baked,
    c("sentembed_text_1", "sentembed_text_2", "sentembed_text_3")
  )
  expect_equal(baked$sentembed_text_1, c(1, 2, 3, 4))
  expect_equal(baked$sentembed_text_2, rep(1, 4))
  expect_equal(baked$sentembed_text_3, rep(2, 4))
})

test_that("requests are formed as the Ollama API expects", {
  fake <- fake_ollama()
  engine <- embedding_ollama("all-minilm", host = "http://example.test:1234/")

  httr2::with_mocked_responses(fake$mock, {
    rec_base |>
      step_sentence_embeddings(text, engine = engine) |>
      prep()
  })

  req <- embed_requests(fake)[[1]]
  expect_identical(req$url, "http://example.test:1234/api/embed")
  expect_identical(req$body$data$model, "all-minilm")
  expect_identical(unlist(req$body$data$input), test_data$text)
})

test_that("the engine can be used from tidy()", {
  fake <- fake_ollama()
  engine <- embedding_ollama("all-minilm")

  rec <- httr2::with_mocked_responses(fake$mock, {
    rec_base |>
      step_sentence_embeddings(text, engine = engine, id = "test") |>
      prep()
  })

  expect_identical(
    tidy(rec, number = 1),
    tibble(
      terms = "text",
      engine = "ollama",
      model = "all-minilm",
      revision = NA_character_,
      dimension = 3L,
      id = "test"
    )
  )
})

test_that("required_pkgs() includes httr2", {
  rec <- rec_base |>
    step_sentence_embeddings(text, engine = embedding_ollama("all-minilm"))

  expect_identical(
    required_pkgs(rec$steps[[1]]),
    c("textrecipes", "httr2")
  )
})

# Options ----------------------------------------------------------------------

test_that("text_prefix is added before embedding", {
  fake <- fake_ollama()
  engine <- embedding_ollama("all-minilm", text_prefix = "query: ")

  baked <- httr2::with_mocked_responses(fake$mock, {
    rec_base |>
      step_sentence_embeddings(text, engine = engine) |>
      prep() |>
      bake(new_data = NULL)
  })

  sent <- unlist(lapply(embed_requests(fake), function(r) r$body$data$input))
  expect_identical(sent, paste0("query: ", test_data$text))
  expect_equal(baked$sentembed_text_1, nchar(test_data$text) + 7)
})

test_that("normalize scales vectors to unit length", {
  fake <- fake_ollama()
  engine <- embedding_ollama("all-minilm", normalize = TRUE)

  baked <- httr2::with_mocked_responses(fake$mock, {
    rec_base |>
      step_sentence_embeddings(text, engine = engine) |>
      prep() |>
      bake(new_data = NULL)
  })

  lengths <- sqrt(rowSums(as.matrix(baked)^2))
  expect_equal(lengths, rep(1, 4))
})

test_that("normalize leaves zero vectors alone", {
  expect_equal(
    normalize_rows(rbind(c(0, 0), c(3, 4))),
    rbind(c(0, 0), c(0.6, 0.8))
  )
})

test_that("batch_size splits requests", {
  fake <- fake_ollama()
  engine <- embedding_ollama("all-minilm", batch_size = 3)

  baked <- httr2::with_mocked_responses(fake$mock, {
    rec_base |>
      step_sentence_embeddings(text, engine = engine) |>
      prep() |>
      bake(new_data = NULL)
  })

  sizes <- vapply(
    embed_requests(fake),
    function(r) length(r$body$data$input),
    integer(1)
  )
  expect_identical(sizes, c(3L, 1L))
  expect_equal(baked$sentembed_text_1, c(1, 2, 3, 4))
})

test_that("the session cache respects settings that change the embeddings", {
  fake <- fake_ollama()

  httr2::with_mocked_responses(fake$mock, {
    rec_base |>
      step_sentence_embeddings(text, engine = embedding_ollama("all-minilm")) |>
      prep()
    n_after_first <- length(embed_requests(fake))

    # Same settings: batch_size doesn't change the embeddings, so it's cached
    rec_base |>
      step_sentence_embeddings(
        text,
        engine = embedding_ollama("all-minilm", batch_size = 2)
      ) |>
      prep()
    expect_identical(length(embed_requests(fake)), n_after_first)

    # A prefix does change them
    rec_base |>
      step_sentence_embeddings(
        text,
        engine = embedding_ollama("all-minilm", text_prefix = "x")
      ) |>
      prep()
    expect_gt(length(embed_requests(fake)), n_after_first)
  })
})

# Guards -----------------------------------------------------------------------

test_that("model names without a tag match :latest", {
  expect_true(ollama_model_available("all-minilm", "all-minilm:latest"))
  expect_true(ollama_model_available("all-minilm:latest", "all-minilm:latest"))
  expect_true(ollama_model_available("all-minilm:22m", "all-minilm:22m"))
  expect_false(ollama_model_available("all-minilm:22m", "all-minilm:latest"))
  expect_false(ollama_model_available("nomic-embed-text", "all-minilm:latest"))
})

test_that("a missing model gives an error that says how to pull it", {
  fake <- fake_ollama(models = "other-model:latest")
  engine <- embedding_ollama("all-minilm")

  expect_snapshot(
    error = TRUE,
    httr2::with_mocked_responses(
      fake$mock,
      prep(step_sentence_embeddings(rec_base, text, engine = engine))
    )
  )

  # Nothing was embedded
  expect_length(embed_requests(fake), 0L)
})

test_that("an unreachable server gives an informative error", {
  clear_sentence_embedding_cache()
  engine <- embedding_ollama("all-minilm")
  unreachable <- function(req) {
    rlang::abort("Couldn't connect to server", class = "httr2_failure")
  }

  expect_snapshot(
    error = TRUE,
    httr2::with_mocked_responses(
      unreachable,
      prep(step_sentence_embeddings(rec_base, text, engine = engine))
    )
  )
})

test_that("the server is only contacted for text that isn't cached", {
  fake <- fake_ollama()
  engine <- embedding_ollama("all-minilm")

  rec <- httr2::with_mocked_responses(fake$mock, {
    rec_base |>
      step_sentence_embeddings(text, engine = engine) |>
      prep()
  })

  n <- length(fake$state$requests)

  # All text is cached, so baking works without a server
  unreachable <- function(req) {
    rlang::abort("Couldn't connect to server", class = "httr2_failure")
  }
  expect_no_error(
    httr2::with_mocked_responses(unreachable, bake(rec, new_data = NULL))
  )
  expect_identical(length(fake$state$requests), n)
})

test_that("HTTP errors stop bake() with the server's message", {
  fake <- fake_ollama()
  failing <- function(req) {
    if (endsWith(req$url, "/api/embed")) {
      return(httr2::response_json(
        status_code = 400L,
        body = list(error = "input exceeds the context length")
      ))
    }
    fake$mock(req)
  }

  engine <- embedding_ollama("all-minilm")

  expect_snapshot(
    error = TRUE,
    httr2::with_mocked_responses(
      failing,
      prep(step_sentence_embeddings(rec_base, text, engine = engine))
    )
  )
})

test_that("only transient errors are retried", {
  # httr2 skips its retry loop when responses are mocked, so this checks that
  # the policy is set up as intended instead of observing a retry.
  req <- ollama_request(embedding_ollama("all-minilm"), "/api/embed")
  is_transient <- req$policies$retry_is_transient

  expect_gt(req$policies$retry_max_tries, 1)
  for (status in c(429L, 502L, 503L, 504L)) {
    expect_true(is_transient(httr2::response(status_code = status)))
  }
  for (status in c(400L, 401L, 404L, 500L)) {
    expect_false(is_transient(httr2::response(status_code = status)))
  }
})

test_that("a response with the wrong number of embeddings is an error", {
  fake <- fake_ollama(embed = function(req, input) list(list(1, 2, 3)))
  engine <- embedding_ollama("all-minilm")

  expect_snapshot(
    error = TRUE,
    httr2::with_mocked_responses(
      fake$mock,
      prep(step_sentence_embeddings(rec_base, text, engine = engine))
    )
  )
})

test_that("a changed dimension is caught when baking", {
  fake <- fake_ollama()
  engine <- embedding_ollama("all-minilm")

  rec <- httr2::with_mocked_responses(fake$mock, {
    rec_base |>
      step_sentence_embeddings(text, engine = engine) |>
      prep()
  })

  changed <- fake_ollama(
    embed = function(req, input) lapply(input, function(s) list(1, 2, 3, 4, 5))
  )

  expect_snapshot(
    error = TRUE,
    httr2::with_mocked_responses(
      changed$mock,
      bake(rec, new_data = tibble(text = "unseen text"))
    )
  )
})

# Engine -----------------------------------------------------------------------

test_that("embedding_ollama() validates its arguments", {
  expect_snapshot(error = TRUE, {
    embedding_ollama()
    embedding_ollama(1)
    embedding_ollama("")
    embedding_ollama("all-minilm", host = "")
    embedding_ollama("all-minilm", text_prefix = 1)
    embedding_ollama("all-minilm", normalize = "yes")
    embedding_ollama("all-minilm", batch_size = 0)
  })
})

test_that("embedding_ollama() prints", {
  expect_snapshot(embedding_ollama("all-minilm"))
})

test_that("embedding_ollama() strips trailing slashes from host", {
  expect_identical(
    embedding_ollama("m", host = "http://example.test///")$host,
    "http://example.test"
  )
})

test_that("printing of the step shows the engine", {
  rec <- rec_base |>
    step_sentence_embeddings(text, engine = embedding_ollama("all-minilm"))

  expect_snapshot(print(rec))
})

# Live server ------------------------------------------------------------------

test_that("works against a real Ollama server", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  engine <- embedding_ollama("all-minilm")
  reachable <- tryCatch(
    {
      engine_check(engine)
      TRUE
    },
    error = function(cnd) FALSE
  )
  skip_if_not(reachable, "No Ollama server with `all-minilm` available")

  baked <- recipe(~., data = tibble(text = c("The cat sat.", "A dog ran."))) |>
    step_sentence_embeddings(text, engine = engine) |>
    prep() |>
    bake(new_data = NULL)

  expect_identical(nrow(baked), 2L)
  expect_identical(ncol(baked), 384L)
  expect_false(anyNA(baked))
})
