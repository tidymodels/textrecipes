#' Positional One-Hot encoding of Tokens
#'
#' `step_sequence_onehot()` creates a *specification* of a recipe step that will
#' take a string and do one hot encoding for each character by position.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param sequence_length A numeric, number of characters to keep before
#'   discarding. Defaults to 100.
#' @param padding 'pre' or 'post', pad either before or after each sequence.
#'   defaults to 'pre'.
#' @param truncating 'pre' or 'post', remove values from sequences larger than
#'   sequence_length either in the beginning or in the end of the sequence.
#'   Defaults too 'pre'.
#' @param vocabulary A character vector, characters to be mapped to integers.
#'   Characters not in the vocabulary will be encoded as 0. Defaults to
#'   `letters`.
#' @param prefix A prefix for generated column names, defaults to "seq1hot".
#' @template args-keep_original_cols
#' @template args-skip
#' @template args-id
#'
#' @source
#' \url{https://papers.nips.cc/paper/5782-character-level-convolutional-networks-for-text-classification.pdf}
#'
#'
#' @template returns
#'
#' @details
#'
#' The string will be capped by the sequence_length argument, strings shorter
#' then sequence_length will be padded with empty characters. The encoding will
#' assign an integer to each character in the vocabulary, and will encode
#' accordingly. Characters not in the vocabulary will be encoded as 0.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `vocabulary`, `token`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{vocabulary}{integer, index}
#'   \item{token}{character, text corresponding to the index}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @family Steps for Numeric Variables From Characters
#'
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~medium, data = tate_text) |>
#'   step_tokenize(medium) |>
#'   step_tokenfilter(medium) |>
#'   step_sequence_onehot(medium)
#'
#' tate_obj <- tate_rec |>
#'   prep()
#'
#' bake(tate_obj, new_data = NULL)
#'
#' tidy(tate_rec, number = 3)
#' tidy(tate_obj, number = 3)
#' @export
step_sequence_onehot <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    columns = NULL,
    sequence_length = 100,
    padding = "pre",
    truncating = "pre",
    vocabulary = NULL,
    prefix = "seq1hot",
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("sequence_onehot")
  ) {
    rlang::arg_match0(padding, c("pre", "post"))
    rlang::arg_match0(truncating, c("pre", "post"))

    add_step(
      recipe,
      step_sequence_onehot_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        sequence_length = sequence_length,
        padding = padding,
        truncating = truncating,
        vocabulary = vocabulary,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_sequence_onehot_new <-
  function(
    terms,
    role,
    trained,
    columns,
    sequence_length,
    padding,
    truncating,
    vocabulary,
    prefix,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "sequence_onehot",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      sequence_length = sequence_length,
      padding = padding,
      truncating = truncating,
      vocabulary = vocabulary,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_sequence_onehot <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_number_whole(x$sequence_length, min = 0, arg = "sequence_length")
  check_string(x$prefix, arg = "prefix")

  check_type(training[, col_names], types = "tokenlist")

  token_list <- list()

  for (col_name in col_names) {
    token_list[[col_name]] <- x$vocabulary %||%
      sort(get_unique_tokens(training[[col_name]]))
  }

  step_sequence_onehot_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    sequence_length = x$sequence_length,
    padding = x$padding,
    truncating = x$truncating,
    vocabulary = token_list,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_sequence_onehot <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  if (is.null(names(object$vocabulary))) {
    # Backwards compatibility with 1.0.3 (#230)
    names(object$vocabulary) <- col_names
  }

  for (col_name in col_names) {
    out_text <- string2encoded_matrix(
      x = new_data[[col_name]],
      vocabulary = object$vocabulary[[col_name]],
      sequence_length = object$sequence_length,
      padding = object$padding,
      truncating = object$truncating
    )

    colnames(out_text) <- paste(
      sep = "_",
      object$prefix,
      col_name,
      seq_len(ncol(out_text))
    )

    out_text <- as_tibble(out_text)

    out_text <- recipes::check_name(out_text, new_data, object, names(out_text))

    new_data <- vec_cbind(new_data, out_text)
  }

  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

#' @export
print.step_sequence_onehot <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Sequence 1 hot encoding for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_sequence_onehot
#' @usage NULL
#' @export
tidy.step_sequence_onehot <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$columns) == 0) {
      res <- tibble(
        terms = character(),
        vocabulary = character(),
        token = integer()
      )
    } else {
      res <- tibble(
        terms = rep(x$columns, each = lengths(x$vocabulary)),
        vocabulary = unlist(lapply(x$vocabulary, seq_along)),
        token = unlist(x$vocabulary)
      )
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      vocabulary = NA_character_,
      token = NA_integer_
    )
  }
  res$id <- x$id
  res
}

char_key <- function(x) {
  out <- seq_along(x)
  names(out) <- x
  out
}

string2encoded_matrix <- function(
  x,
  vocabulary,
  sequence_length,
  padding,
  truncating
) {
  vocabulary <- char_key(vocabulary)
  x <- get_tokens(x)

  res <- matrix(NA_integer_, nrow = length(x), ncol = sequence_length)

  for (i in seq_along(x)) {
    len_x <- length(x[[i]])

    values <- x[[i]]

    if (len_x == 0) next

    if (len_x == sequence_length) {
      res[i, ] <- values
    }

    if (len_x < sequence_length) {
      if (padding == "post") {
        res[i, seq_len(len_x)] <- values
      } else {
        res[i, seq(sequence_length - len_x + 1, sequence_length)] <- values
      }
    } else {
      if (truncating == "post") {
        res[i, ] <- values[seq_len(sequence_length)]
      } else {
        res[i, ] <- values[seq(len_x - sequence_length + 1, len_x)]
      }
    }
  }
  res <- matrix(
    vocabulary[match(res, names(vocabulary))],
    nrow = length(x),
    ncol = sequence_length
  )
  res[is.na(res)] <- 0L
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_sequence_onehot <- function(x, ...) {
  c("textrecipes")
}
