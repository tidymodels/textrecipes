# Takes a data.frame (data) and replaces the columns with the names (names)
# and converts them from factor variable to character variables. Keeps
# characters variables unchanged.
factor_to_text <- function(data, names) {
  for (i in seq_along(names)) {
    if (is.factor(data[, names[i], drop = TRUE])) {
      data[, names[i]] <- as.character.factor(data[, names[i], drop = TRUE])
    }
  }
  data
}

## This function takes the default arguments of `cl` (call object) and
## replaces them with the matching ones in `options` and
## remove any in `removals`
mod_call_args <- function(cl, args, removals = NULL) {
  if (!is.null(removals)) {
    for (i in removals) {
      cl[[i]] <- NULL
    }
  }
  arg_names <- names(args)
  for (i in arg_names) {
    cl[[i]] <- args[[i]]
  }
  cl
}

check_list <- function(dat) {
  all_good <- vapply(dat, is.list, logical(1))

  if (!all(all_good)) {
    rlang::abort("All columns selected for this step should be tokenlists")
  }

  invisible(all_good)
}

# same as tokenlist_filter but takes an list as input and returns a tibble with
# [tokenlist].
word_tbl_filter <- function(x, words, keep) {
  tibble(
    map(x, tokenlist_filter, words, keep)
  )
}
