new_tokenlist <- function(x = list(), lemma = NULL, tokens = character()) {
  vec_assert(x, list())
  if (!(is.null(lemma) | is.list(lemma))) {
    rlang::abort("`lemma` must be NULL or a list.")
  }
  vec_assert(tokens, character())
  new_vctr(x, lemma = lemma, tokens = tokens, class = "textrecipes_tokenlist")
}

tokenlist <- function(x = list(), lemma = NULL) {
  x <- vec_cast(x, list())
  if (!is.null(lemma)) {
    lemma <- vec_cast(lemma, list())
  }
  new_tokenlist(x, lemma = lemma, tokens = unique(unlist(x)))
}

is_tokenlist <- function(x) {
  inherits(x, "textrecipes_tokenlist")
}

#' @export
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

vec_restore.textrecipes_tokenlist <- function(x, to, ..., i = NULL) {
  new_tokenlist(x, unique(unlist(x)))
}

`[.textrecipes_tokenlist`<- function(x, i) {
  if (is.null(attr(x, "lemma"))) {
    lemma <- NULL
  } else {
    lemma <- attr(x, "lemma")[i]
  }
  tokenlist(vec_data(x)[i], lemma = lemma)
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

  keep_id <- !is.na(j)
  split_id <- factor(i[keep_id], seq_x)
  
  out <- split(dict[j[keep_id]], split_id)
  names(out) <- NULL
  
  if (!is.null(attr(x, "lemma"))) {
    lemma <- split(unlist(attr(x, "lemma"))[keep_id], split_id)
    names(lemma) <- NULL
  } else {
    lemma <- NULL
  }
  
  new_tokenlist(out, lemma = lemma, tokens = dict)
}

tokenlist_apply <- function(x, fun) {
  if (!is_tokenlist(x)) {
    rlang::abort("Input must be a tokenlist.")
  }
  
  tokenlist(lapply(x, fun))
}

# Takes a list of tokens and calculate the token count matrix
tokenlist_to_dtm <- function(x, dict) {
  if (!is_tokenlist(x)) {
    rlang::abort("Input must be a tokenlist.")
  }
  i <- rep(seq_along(x), lengths(x))
  j <- match(unlist(x), dict)
  
  out <- sparseMatrix(i = i[!is.na(j)],  
                      j = j[!is.na(j)], 
                      dims = c(length(x), length(dict)),
                      x = 1)
  
  out@Dimnames[[2]] <- dict
  out
}
