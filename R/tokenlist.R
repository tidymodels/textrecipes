new_tokenlist <- function(x = list(), tokens = character()) {
  vec_assert(x, list())
  vec_assert(tokens, character())
  new_vctr(x, tokens = tokens, class = "textrecipes_tokenlist")
}

tokenlist <- function(x = list()) {
  x <- vec_cast(x, list())
  new_tokenlist(x, tokens = unique(unlist(x)))
}

is_tokenlist <- function(x) {
  inherits(x, "textrecipes_tokenlist")
}

format.textrecipes_tokenlist <- function(x, ...) {
  out <- formatC(lengths(vec_data(x)))
  paste0("[", out, " tokens]")
}

vec_ptype_abbr.textrecipes_tokenlist <- function(x, ...) {
  "tokens"
}

obj_print_footer.textrecipes_tokenlist <- function(x, ...) {
  cat("# Unique Tokens: ", format(length(attr(x, "tokens"))), "\n", sep = "")
}

# Takes a vector of character vectors and keeps (for keep = TRUE) the words
# or removes (for keep = FALSE) the words
tokenlist_filter <- function(x, dict, keep = FALSE) {
  if (!is_tokenlist(x)) {
    rlang::abort("Input must be a tokenlist.")
  }
  
  if (!keep) {
    dict <- setdiff(attr(x, "tokens"), dict)
  }
  
  seq_x <- seq_along(x)
  i <- rep(seq_x, lengths(x))
  j <- match(unlist(x), dict)
  
  out <- split(dict[j[!is.na(j)]], factor(i[!is.na(j)], seq_x))
  names(out) <- NULL
  new_tokenlist(out, dict)
}
