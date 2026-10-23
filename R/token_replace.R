#' Replace Tokens in a Token Variable
#'
#' `step_token_replace()` creates a *specification* of a recipe step that will
#' replace tokens in a [`token`][tokenlist()] variable according to a table of
#' patterns and replacements.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param pattern A character vector of tokens to look for. Must be the same
#'   length as `replacement`, or `replacement` must be length 1.
#' @param replacement A character vector of the tokens to replace the matches
#'   with.
#' @param fixed Logical. If `TRUE`, the default, a token is replaced when it is
#'   exactly equal to an element of `pattern`. If `FALSE`, `pattern` is treated
#'   as a vector of regular expressions, and a token is replaced when it matches
#'   one. Regular expressions are case-sensitive, and backreferences in
#'   `replacement` are not supported: `replacement` is always used literally.
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' The whole token is replaced, not just the part that matched. If a token
#' matches more than one element of `pattern`, the first match is used. Since
#' the number of tokens is unchanged, the lemma and part-of-speech attributes
#' of the tokens are kept as is.
#'
#' This is useful for normalizing spelling variants, mapping tokens to a
#' placeholder such as `"<number>"`, or applying a small dictionary of
#' corrections.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned
#' with columns `terms`, `pattern`, `replacement`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{pattern}{character, the patterns to look for}
#'   \item{replacement}{character, the replacements}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @seealso [step_tokenize()] to turn characters into [`tokens`][tokenlist()]
#' @family Steps for Token Modification
#'
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~., data = tate_text) |>
#'   step_tokenize(medium) |>
#'   step_token_replace(
#'     medium,
#'     pattern = c("paper", "canvas"),
#'     replacement = c("PAPER", "CANVAS")
#'   )
#'
#' tate_obj <- tate_rec |>
#'   prep()
#'
#' bake(tate_obj, new_data = NULL) |>
#'   slice(1:2) |>
#'   pull(medium)
#'
#' tidy(tate_rec, number = 2)
#' tidy(tate_obj, number = 2)
#'
#' # Using regular expressions
#' tate_rec <- recipe(~., data = tate_text) |>
#'   step_tokenize(medium) |>
#'   step_token_replace(
#'     medium,
#'     pattern = "^[0-9]+$",
#'     replacement = "<number>",
#'     fixed = FALSE
#'   )
#' @export
step_token_replace <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    columns = NULL,
    pattern,
    replacement,
    fixed = TRUE,
    skip = FALSE,
    id = rand_id("token_replace")
  ) {
    check_character(pattern, allow_na = FALSE)
    check_character(replacement, allow_na = FALSE)
    check_bool(fixed)
    if (length(pattern) == 0) {
      cli::cli_abort("{.arg pattern} must have at least one element.")
    }
    if (any(pattern == "")) {
      cli::cli_abort("{.arg pattern} must not contain empty strings.")
    }
    if (!fixed) {
      check_regex(pattern)
    }
    if (!length(replacement) %in% c(1L, length(pattern))) {
      cli::cli_abort(
        "{.arg replacement} must be length 1 or the same length as
        {.arg pattern} ({length(pattern)}), not length {length(replacement)}."
      )
    }

    add_step(
      recipe,
      step_token_replace_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        pattern = pattern,
        replacement = rep_len(replacement, length(pattern)),
        fixed = fixed,
        skip = skip,
        id = id
      )
    )
  }

step_token_replace_new <-
  function(
    terms,
    role,
    trained,
    columns,
    pattern,
    replacement,
    fixed,
    skip,
    id
  ) {
    step(
      subclass = "token_replace",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      pattern = pattern,
      replacement = replacement,
      fixed = fixed,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_token_replace <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names], types = "tokenlist")

  step_token_replace_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    pattern = x$pattern,
    replacement = x$replacement,
    fixed = x$fixed,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_token_replace <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  new_data <- recipes_map_cols(new_data, col_names, function(x) {
    replace_fun <- make_token_replacer(
      get_unique_tokens(x),
      object$pattern,
      object$replacement,
      object$fixed
    )
    tokenlist_apply(x, replace_fun)
  })
  new_data
}

# Matches the patterns against the unique tokens once, and returns a function
# that replaces the tokens of a single document using that lookup. Each token is
# replaced with the replacement of the first pattern it matches.
make_token_replacer <- function(unique_tokens, pattern, replacement, fixed) {
  if (fixed) {
    idx <- match(unique_tokens, pattern)
  } else {
    idx <- rep(NA_integer_, length(unique_tokens))
    for (i in rev(seq_along(pattern))) {
      idx[grepl(pattern[[i]], unique_tokens)] <- i
    }
  }

  hit <- !is.na(idx)
  from <- unique_tokens[hit]
  to <- replacement[idx[hit]]

  function(tokens) {
    j <- match(tokens, from)
    found <- !is.na(j)
    tokens[found] <- to[j[found]]
    tokens
  }
}

check_regex <- function(
  pattern,
  arg = caller_arg(pattern),
  call = caller_env()
) {
  for (p in pattern) {
    tryCatch(
      grepl(p, ""),
      error = function(e) {
        cli::cli_abort(
          "{.arg {arg}} contains an invalid regular expression: {.val {p}}.",
          call = call
        )
      },
      warning = function(w) {
        cli::cli_abort(
          "{.arg {arg}} contains an invalid regular expression: {.val {p}}.",
          call = call
        )
      }
    )
  }
  invisible(pattern)
}

#' @export
print.step_token_replace <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Token replacement for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_token_replace
#' @usage NULL
#' @export
tidy.step_token_replace <- function(x, ...) {
  if (is_trained(x)) {
    terms <- unname(x$columns)
  } else {
    terms <- sel2char(x$terms)
  }

  n_terms <- length(terms)
  res <- tibble(
    terms = rep(terms, each = length(x$pattern)),
    pattern = rep(x$pattern, times = n_terms),
    replacement = rep(x$replacement, times = n_terms)
  )
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_token_replace <- function(x, ...) {
  c("textrecipes")
}
