#' Create Token Object
#'
#' A [tokenlist] object is a thin wrapper around a list of character vectors,
#' with a few attributes.
#'
#' @param tokens List of character vectors
#' @param lemma List of character vectors, must be same size and shape as `x`.
#' @param pos List of character vectors, must be same size and shape as `x`.
#'
#' @return a [tokenlist] object.
#'
#' @examplesIf rlang::is_installed("modeldata")
#' abc <- list(letters, LETTERS)
#' tokenlist(abc)
#'
#' unclass(tokenlist(abc))
#'
#' tibble(text = tokenlist(abc))
#'
#' library(tokenizers)
#' library(modeldata)
#' data(tate_text)
#' tokens <- tokenize_words(as.character(tate_text$medium))
#'
#' tokenlist(tokens)
#' @export
tokenlist <- function(tokens = list(), lemma = NULL, pos = NULL) {
  tokens <- vec_cast(tokens, list())
  if (!is.null(lemma)) {
    lemma <- vec_cast(lemma, list())
  }
  if (!is.null(pos)) {
    pos <- vec_cast(pos, list())
  }
  unique_tokens <- unique(unlist(tokens))

  new_tokenlist(
    tokens = tokens,
    lemma = lemma,
    pos = pos,
    unique_tokens = unique_tokens %||% character()
  )
}

new_tokenlist <- function(
  tokens = list(),
  lemma = NULL,
  pos = NULL,
  unique_tokens = character()
) {
  vec_assert(tokens, list())
  if (!(is.null(lemma) | is.list(lemma))) {
    cli::cli_abort("{.arg lemma} must be NULL or a list.")
  }
  if (!(is.null(pos) | is.list(pos))) {
    cli::cli_abort("{.arg pos} must be NULL or a list.")
  }
  vec_assert(unique_tokens, character())

  if (length(tokens) == 0) {
    return(
      vctrs::new_rcrd(
        fields = list(tokens = tokens),
        unique_tokens = unique_tokens,
        class = "textrecipes_tokenlist"
      )
    )
  }

  vctrs::new_rcrd(
    fields = purrr::compact(
      list(
        tokens = tokens,
        lemma = lemma,
        pos = pos
      )
    ),
    unique_tokens = unique_tokens,
    class = "textrecipes_tokenlist"
  )
}

is_tokenlist <- function(x) {
  inherits(x, "textrecipes_tokenlist")
}

maybe_get_lemma <- function(x) {
  if ("lemma" %in% vctrs::fields(x)) {
    vctrs::field(x, "lemma")
  } else {
    NULL
  }
}

maybe_get_pos <- function(x) {
  if ("pos" %in% vctrs::fields(x)) {
    vctrs::field(x, "pos")
  } else {
    NULL
  }
}

get_tokens <- function(x) {
  vctrs::field(x, "tokens")
}

get_unique_tokens <- function(x) {
  attr(x, "unique_tokens")
}

#' @export
vec_restore.textrecipes_tokenlist <- function(x, to, ..., i = NULL) {
  tokens <- get_tokens(x)
  unique_tokens <- unique(unlist(tokens))

  new_tokenlist(
    tokens,
    maybe_get_lemma(x),
    maybe_get_pos(x),
    unique_tokens %||% character()
  )
}

# Printing --------------------------------------------------------------------
#' @export
format.textrecipes_tokenlist <- function(x, ...) {
  out <- formatC(lengths(vctrs::field(x, "tokens")))
  paste0("[", out, " tokens]")
}

#' @export
vec_ptype_abbr.textrecipes_tokenlist <- function(x, ...) {
  "tknlist"
}

#' @export
obj_print_footer.textrecipes_tokenlist <- function(x, ...) {
  cat(
    "# Unique Tokens: ",
    format(length(attr(x, "unique_tokens"))),
    "\n",
    sep = ""
  )
}

# Special functions -----------------------------------------------------------

# Takes a vector of character vectors and keeps (for keep = TRUE) the words
# or removes (for keep = FALSE) the words
tokenlist_filter <- function(x, dict, keep = FALSE) {
  if (!is_tokenlist(x)) {
    cli::cli_abort("Input must be a tokenlist.")
  }

  if (!keep) {
    dict <- setdiff(attr(x, "unique_tokens"), dict)
  }

  tokens <- get_tokens(x)
  seq_x <- seq_along(tokens)
  i <- rep(seq_x, lengths(tokens))
  j <- match(unlist(tokens), dict)

  keep_id <- !is.na(j)
  split_id <- factor(i[keep_id], seq_x)

  out <- split(dict[j[keep_id]], split_id)
  names(out) <- NULL

  lemma <- maybe_get_lemma(x)
  if (!is.null(lemma)) {
    lemma <- split(unlist(lemma)[keep_id], split_id)
    names(lemma) <- NULL
  } else {
    lemma <- NULL
  }

  pos <- maybe_get_pos(x)
  if (!is.null(pos)) {
    pos <- split(unlist(pos)[keep_id], split_id)
    names(pos) <- NULL
  } else {
    pos <- NULL
  }

  new_tokenlist(out, lemma = lemma, pos = pos, unique_tokens = dict)
}

tokenlist_filter_function <- function(x, fn) {
  if (!is_tokenlist(x)) {
    cli::cli_abort("Input must be a {.cls tokenlist}.")
  }

  tokens <- get_tokens(x)

  keeps <- lapply(tokens, fn)

  out <- purrr::map2(tokens, keeps, \(.x, .y) .x[.y])

  lemma <- maybe_get_lemma(x)
  if (!is.null(lemma)) {
    lemma <- purrr::map2(lemma, keeps, \(.x, .y) .x[.y])
    names(lemma) <- NULL
  } else {
    lemma <- NULL
  }

  pos <- maybe_get_pos(x)
  if (!is.null(pos)) {
    pos <- purrr::map2(pos, keeps, \(.x, .y) .x[.y])
    names(pos) <- NULL
  } else {
    pos <- NULL
  }

  tokenlist(out, lemma = lemma, pos = pos)
}

tokenlist_apply <- function(x, fun, arguments = NULL) {
  if (!is_tokenlist(x)) {
    cli::cli_abort("Input must be {.cls tokenlist} object.")
  }

  tokens <- get_tokens(x)
  apply_expr <- expr(lapply(tokens, fun))

  if (length(arguments) > 0) {
    apply_expr <- rlang::call_modify(apply_expr, !!!arguments)
  }

  tokenlist(eval(apply_expr))
}

# Takes a [tokenlist] and calculate the token count matrix
tokenlist_to_dtm <- function(x, dict) {
  if (!is_tokenlist(x)) {
    cli::cli_abort("Input must be a tokenlist.")
  }

  tokens <- get_tokens(x)
  i <- rep(seq_along(tokens), lengths(tokens))
  j <- match(unlist(tokens), dict)

  out <- Matrix::sparseMatrix(
    i = i[!is.na(j)],
    j = j[!is.na(j)],
    dims = c(length(tokens), length(dict)),
    x = 1
  )

  out@Dimnames[[2]] <- dict
  out
}

tokenlist_lemma <- function(x) {
  if (!is_tokenlist(x)) {
    cli::cli_abort("Input must be a tokenlist.")
  }

  if (is.null(maybe_get_lemma(x))) {
    cli::cli_abort("The {.code lemma} attribute is not available.")
  }

  tokenlist(maybe_get_lemma(x), pos = maybe_get_pos(x))
}

tokenlist_pos_filter <- function(x, pos_tags) {
  if (!is_tokenlist(x)) {
    cli::cli_abort("Input must be a tokenlist.")
  }

  if (is.null(maybe_get_pos(x))) {
    cli::cli_abort("{.arg pos} attribute not available.")
  }

  tokens <- get_tokens(x)
  seq_x <- seq_along(tokens)
  i <- rep(seq_x, lengths(tokens))
  j <- match(unlist(maybe_get_pos(x)), pos_tags)

  keep_id <- !is.na(j)
  split_id <- factor(i[keep_id], seq_x)

  out <- split(unlist(get_tokens(x))[keep_id], split_id)
  names(out) <- NULL

  pos <- split(unlist(maybe_get_pos(x))[keep_id], split_id)
  names(pos) <- NULL

  lemma <- maybe_get_lemma(x)
  if (!is.null(lemma)) {
    lemma <- split(unlist(lemma)[keep_id], split_id)
    names(lemma) <- NULL
  } else {
    lemma <- NULL
  }

  tokenlist(out, lemma = lemma, pos = pos)
}

tokenlist_ngram <- function(x, n, n_min, delim) {
  if (!is_tokenlist(x)) {
    cli::cli_abort("Input must be a tokenlist.")
  }

  tokenlist(ngram(get_tokens(x), n, n_min, delim))
}

tokenlist_embedding <- function(x, emb, aggregation, aggregation_default) {
  tokens <- get_tokens(x)
  n_doc <- length(tokens)
  emb_names <- names(emb)[-1]
  n_dim <- length(emb_names)

  # Map each token occurrence to its document and its row in `emb`.
  doc_id <- rep.int(seq_len(n_doc), lengths(tokens))
  token_index <- match(unlist(tokens, use.names = FALSE), emb[[1]])

  keep <- !is.na(token_index)
  doc_id <- doc_id[keep]
  token_index <- token_index[keep]

  res <- matrix(
    aggregation_default,
    nrow = n_doc,
    ncol = n_dim,
    dimnames = list(NULL, emb_names)
  )

  if (length(token_index) > 0L) {
    emb_mat <- as.matrix(emb[, -1])
    vals <- emb_mat[token_index, , drop = FALSE]

    agg <- aggregate_embedding(vals, doc_id, aggregation)
    res[as.integer(rownames(agg)), ] <- agg
  }

  tibble::as_tibble(res)
}

# Aggregate the rows of `vals` (a numeric matrix) by `group`, returning a matrix
# with one row per present group and the group label as the row name.
aggregate_embedding <- function(vals, group, aggregation) {
  if (aggregation %in% c("sum", "mean")) {
    out <- rowsum(vals, group, reorder = FALSE)
    if (aggregation == "mean") {
      counts <- tabulate(match(group, as.integer(rownames(out))))
      out <- out / counts
    }
    return(out)
  }

  fun <- switch(aggregation, min = min, max = max)
  group <- factor(group)
  # Loop over the (relatively few) embedding columns, aggregating each with a
  # single vectorised `tapply`, rather than looping over the many groups.
  out <- vapply(
    seq_len(ncol(vals)),
    function(col) tapply(vals[, col], group, fun),
    numeric(nlevels(group))
  )
  rownames(out) <- levels(group)
  out
}
