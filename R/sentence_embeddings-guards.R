# Guards for step_sentence_embeddings(). Each helper checks one thing and aborts
# with an error that names the engine, so a failure points at the cause instead
# of surfacing somewhere deep inside a batch.

check_embedding_backend <- function(engine, call = caller_env()) {
  tryCatch(
    engine_check(engine),
    error = function(cnd) {
      cli::cli_abort(
        "The {.field {engine_label(engine)}} engine can't be used.",
        parent = cnd,
        call = call
      )
    }
  )
  invisible(engine)
}

# The column is checked in `prep()` with `check_type()`. This is the same rule
# for the `bake()` side, where `new_data` can differ from the training data.
check_embedding_input <- function(x, col_name, call = caller_env()) {
  if (!is.character(x) && !is.factor(x)) {
    cli::cli_abort(
      c(
        "{.field {col_name}} must be a character or factor vector.",
        "i" = "It is {.cls {class(x)[[1]]}}."
      ),
      class = "bad_embedding_input",
      call = call
    )
  }
  invisible(x)
}

check_embedding_result <- function(
  res,
  n,
  engine,
  call = caller_env()
) {
  label <- engine_label(engine)

  if (!is.numeric(res) || length(dim(res)) != 2L) {
    cli::cli_abort(
      c(
        "The {.field {label}} engine must return a numeric matrix.",
        "i" = "It returned {.obj_type_friendly {res}}."
      ),
      class = "bad_embedding_result",
      call = call
    )
  }

  if (nrow(res) != n) {
    cli::cli_abort(
      c(
        "The {.field {label}} engine must return one row per string.",
        "i" = "It was given {n} string{?s} and returned {nrow(res)} row{?s}."
      ),
      class = "bad_embedding_result",
      call = call
    )
  }

  if (ncol(res) < 1L) {
    cli::cli_abort(
      "The {.field {label}} engine returned a matrix with 0 columns.",
      class = "bad_embedding_result",
      call = call
    )
  }

  invisible(res)
}

check_embedding_dimension <- function(
  res,
  dimension,
  engine,
  call = caller_env()
) {
  if (ncol(res) != dimension) {
    cli::cli_abort(
      c(
        "The embedding dimension changed since the recipe was prepped.",
        "i" = "The {.field {engine_label(engine)}} engine returned
               {ncol(res)} dimension{?s} but {dimension} {?was/were} expected.",
        "i" = "The model behind the engine may have changed."
      ),
      class = "embedding_dimension_changed",
      call = call
    )
  }
  invisible(res)
}
