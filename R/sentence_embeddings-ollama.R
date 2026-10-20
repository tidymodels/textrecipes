#' Ollama Embedding Engine
#'
#' `embedding_ollama()` creates an engine for [step_sentence_embeddings()] that
#' gets embeddings from a running [Ollama](https://ollama.com) server. No Python
#' environment or API key is needed, the engine sends HTTP requests to the
#' server, which is usually running on your own machine.
#'
#' @param model A single string giving the name of the Ollama model, such as
#'   `"all-minilm"`. The model must already be available on the server, which
#'   you can do with `ollama pull <model>`. There is no default, so the model
#'   is always visible in your code.
#' @param host A single string giving the address of the Ollama server. Defaults
#'   to `"http://localhost:11434"`.
#' @param text_prefix A single string added to the start of every text before it
#'   is embedded, or `NULL` for no prefix. Some models are trained with
#'   prefixes, for example `"query: "` or `"passage: "`. Defaults to `NULL`.
#' @param normalize A single logical. If `TRUE`, each embedding is scaled to
#'   have unit length. Defaults to `FALSE`, which returns the vectors as the
#'   server returns them.
#' @param batch_size A single positive whole number giving the maximum number of
#'   texts sent in one request. Defaults to `64`.
#'
#' @details
#'
#' Embeddings are requested from the `/api/embed` endpoint. Requests that fail
#' with a transient error (too many requests, or the server being unavailable)
#' are retried a few times. Any other failure stops `bake()` with an error, so
#' partial results are never returned. Texts longer than the context length of
#' the model are an error rather than being silently truncated.
#'
#' Before any text is embedded, the engine checks that the server can be
#' reached and that `model` is available on it, and gives an error that says
#' how to fix it if not. The text is sent to the server at `host`. For a server
#' you don't control, keep in mind that this is where your data goes.
#'
#' The engine stores the model name and the server address, not the model. A
#' prepped recipe therefore needs the same model to be available on the server
#' when it is baked, and Ollama model names, such as `"all-minilm"`, can point to
#' different model versions over time. The engine doesn't pin a version.
#'
#' Requires the httr2 package.
#'
#' # Setting up Ollama
#'
#' The engine needs an Ollama server that is running and has the model.
#'
#' 1. Install Ollama from <https://ollama.com/download>. See the
#'    [Ollama documentation](https://docs.ollama.com) for your system.
#' 2. Make sure the server is running. The desktop apps start it when you open
#'    them. Otherwise run `ollama serve` in a terminal and leave it running.
#' 3. Pull the model once, for example `ollama pull all-minilm`. Models are
#'    listed at <https://ollama.com/library>.
#'
#' You can check which models are on the server with `ollama list`.
#'
#' @return An object of class `sentence_embedding_engine`.
#'
#' @seealso [step_sentence_embeddings()]
#' @family Embedding Engines
#'
#' @examplesIf FALSE
#' # Needs a running Ollama server with the model pulled:
#' #   ollama pull all-minilm
#' library(recipes)
#'
#' sample_data <- tibble(
#'   text = c("The cat sat.", "A dog ran.", "Stocks fell sharply today.")
#' )
#'
#' rec <- recipe(~text, data = sample_data) |>
#'   step_sentence_embeddings(
#'     text,
#'     engine = embedding_ollama("all-minilm")
#'   )
#'
#' rec |>
#'   prep() |>
#'   bake(new_data = NULL)
#' @export
embedding_ollama <- function(
  model,
  host = "http://localhost:11434",
  text_prefix = NULL,
  normalize = FALSE,
  batch_size = 64
) {
  check_string(model, arg = "model")
  check_string(host, arg = "host")
  check_string(text_prefix, allow_null = TRUE, arg = "text_prefix")
  check_bool(normalize, arg = "normalize")
  check_number_whole(batch_size, min = 1, arg = "batch_size")

  if (!nzchar(model)) {
    cli::cli_abort("{.arg model} must not be an empty string.")
  }
  if (!nzchar(host)) {
    cli::cli_abort("{.arg host} must not be an empty string.")
  }

  new_embedding_engine(
    "embedding_ollama",
    model = model,
    host = sub("/+$", "", host),
    text_prefix = text_prefix,
    normalize = normalize,
    batch_size = as.integer(batch_size)
  )
}

#' @export
engine_label.embedding_ollama <- function(engine) {
  "ollama"
}

#' @export
engine_model.embedding_ollama <- function(engine) {
  engine$model
}

#' @export
engine_revision.embedding_ollama <- function(engine) {
  NA_character_
}

#' @export
engine_required_pkgs.embedding_ollama <- function(engine) {
  "httr2"
}

# Everything that changes the embeddings goes into the key. `batch_size` doesn't.
#' @export
engine_cache_key.embedding_ollama <- function(engine) {
  rlang::hash(list(
    "ollama",
    engine$host,
    engine$model,
    engine$text_prefix,
    engine$normalize
  ))
}

#' @export
engine_check.embedding_ollama <- function(engine) {
  rlang::check_installed("httr2", reason = "to use `embedding_ollama()`.")

  available <- ollama_models(engine)

  if (!ollama_model_available(engine$model, available)) {
    cli::cli_abort(
      c(
        "The model {.val {engine$model}} isn't available on the Ollama server.",
        "i" = "Pull it with {.code ollama pull {engine$model}}.",
        "i" = "Server: {.url {engine$host}}"
      ),
      class = "ollama_model_not_found"
    )
  }

  invisible(engine)
}

#' @export
engine_embed.embedding_ollama <- function(engine, x) {
  if (anyNA(x)) {
    cli::cli_abort("{.arg x} can't contain missing values.")
  }

  if (length(x) == 0L) {
    return(matrix(NA_real_, nrow = 0L, ncol = 0L))
  }

  if (!is.null(engine$text_prefix)) {
    x <- paste0(engine$text_prefix, x)
  }

  batches <- split(x, ceiling(seq_along(x) / engine$batch_size))

  res <- lapply(batches, function(batch) ollama_embed_batch(engine, batch))
  res <- do.call(rbind, unname(res))

  if (engine$normalize) {
    res <- normalize_rows(res)
  }

  res
}

# Requests ---------------------------------------------------------------------

ollama_request <- function(engine, path) {
  req <- httr2::request(paste0(engine$host, path))
  req <- httr2::req_user_agent(
    req,
    "textrecipes (https://textrecipes.tidymodels.org)"
  )
  req <- httr2::req_timeout(req, 120)
  req <- httr2::req_retry(
    req,
    max_tries = 3,
    # 500 is left out: it is what Ollama returns for a model that fails to load
    # or runs out of memory, and retrying won't change that.
    is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(429L, 502L, 503L, 504L)
    }
  )
  httr2::req_error(
    req,
    body = function(resp) {
      body <- tryCatch(httr2::resp_body_json(resp), error = function(cnd) NULL)
      body$error
    }
  )
}

# Names of the models on the server. Also checks that it can be reached.
ollama_models <- function(engine) {
  req <- ollama_request(engine, "/api/tags")

  call <- rlang::current_env()
  abort_unreachable <- function(cnd) {
    cli::cli_abort(
      c(
        "Can't reach the Ollama server at {.url {engine$host}}.",
        "i" = "Is it running? Start it with {.code ollama serve}.",
        "i" = "To use a server at another address, set {.arg host}."
      ),
      parent = cnd,
      call = call,
      class = "ollama_unreachable"
    )
  }

  resp <- tryCatch(
    httr2::req_perform(req),
    httr2_failure = abort_unreachable,
    httr2_http = abort_unreachable
  )

  body <- tryCatch(httr2::resp_body_json(resp), error = abort_unreachable)
  if (!is.list(body$models)) {
    abort_unreachable(NULL)
  }

  vapply(body$models, function(m) m$name %||% m$model, character(1))
}

# Ollama treats a name without a tag as `:latest`. A `:` before the last `/` is
# a registry port, not a tag.
ollama_model_available <- function(model, available) {
  if (!grepl(":", sub("^.*/", "", model), fixed = TRUE)) {
    model <- paste0(model, ":latest")
  }
  model %in% available
}

ollama_embed_batch <- function(engine, batch) {
  req <- ollama_request(engine, "/api/embed")
  req <- httr2::req_body_json(
    req,
    # Error on texts longer than the context instead of silently cutting them.
    list(model = engine$model, input = as.list(batch), truncate = FALSE)
  )

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp)

  embeddings <- body$embeddings
  if (!is.list(embeddings) || length(embeddings) != length(batch)) {
    cli::cli_abort(
      c(
        "The Ollama server returned an unexpected response.",
        "i" = "It was sent {length(batch)} text{?s} and returned
               {length(embeddings)} embedding{?s}."
      ),
      class = "bad_embedding_result"
    )
  }

  embeddings <- lapply(embeddings, function(e) as.numeric(unlist(e)))
  lengths <- lengths(embeddings)
  if (any(lengths == 0L) || length(unique(lengths)) != 1L) {
    cli::cli_abort(
      c(
        "The Ollama server returned an unexpected response.",
        "i" = "The embeddings are empty or differ in length."
      ),
      class = "bad_embedding_result"
    )
  }

  res <- do.call(rbind, embeddings)
  if (anyNA(res)) {
    cli::cli_abort(
      c(
        "The Ollama server returned an unexpected response.",
        "i" = "The embeddings contain missing values."
      ),
      class = "bad_embedding_result"
    )
  }

  res
}

normalize_rows <- function(x) {
  norms <- sqrt(rowSums(x^2))
  # Leave all-zero vectors as they are instead of dividing by zero.
  norms[norms == 0] <- 1
  x / norms
}
