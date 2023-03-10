# Takes a data.frame (data) and replaces the columns with the names (names) and
# converts them from factor variable to character variables. Keeps characters
# variables unchanged.
factor_to_text <- function(data, names) {
  for (i in seq_along(names)) {
    if (is.factor(data[, names[i], drop = TRUE])) {
      data[, names[i]] <- as.character.factor(data[, names[i], drop = TRUE])
    }
  }
  data
}

check_possible_tokenizers <- function(x, dict, call = caller_env(2)) {
  if (!(x %in% dict)) {
    possible_tokenizers <- glue::glue_collapse(
      dict,
      sep = ", ", last = ", or "
    )
    rlang::abort(
      glue(
        "token should be one of the supported: {possible_tokenizers}"
      ),
      call = call
    )
  }
}

# same as tokenlist_filter but takes an list as input and returns a tibble with
# [`token`][tokenlist()].
word_tbl_filter <- function(x, words, keep) {
  tibble(
    map(x, tokenlist_filter, words, keep)
  )
}

table0 <- function(x) {
  res <- dplyr::count(tibble(tokens = x), tokens)

  purrr::set_names(res$n, res$tokens)
}
