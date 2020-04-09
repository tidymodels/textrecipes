#' Create tokenlist object
#' 
#' A [tokenlist] object is a thin wrapper around a list of character vectors, 
#' with a few attributes.
#'
#' @param x List of character vectors
#' @param lemma List of character vectors, must be same size and shape as `x`.
#' @param pos List of character vectors, must be same size and shape as `x`.
#'
#' @return a [tokenlist] object.
#' @export
#'
#' @examples
#' abc <- list(letters, LETTERS)
#' tokenlist(abc)
#' 
#' unclass(tokenlist(abc))
#' 
#' tibble(text = tokenlist(abc))
#' 
#' library(tokenizers)
#' library(modeldata)
#' data(okc_text)
#' tokens <- tokenize_words(okc_text$essay0)
#' 
#' tokenlist(tokens)
tokenlist <- function(x = list(), lemma = NULL, pos = NULL) {
  x <- vec_cast(x, list())
  if (!is.null(lemma)) {
    lemma <- vec_cast(lemma, list())
  }
  if (!is.null(pos)) {
    pos <- vec_cast(pos, list())
  }
  new_tokenlist(x, lemma = lemma, pos = pos, tokens = unique(unlist(x)))
}

new_tokenlist <- function(x = list(), lemma = NULL, pos, tokens = character()) {
  vec_assert(x, list())
  if (!(is.null(lemma) | is.list(lemma))) {
    rlang::abort("`lemma` must be NULL or a list.")
  }
  if (!(is.null(pos) | is.list(pos))) {
    rlang::abort("`pos` must be NULL or a list.")
  }
  vec_assert(tokens, character())
  new_vctr(x, lemma = lemma, pos = pos, tokens = tokens, 
           class = "textrecipes_tokenlist")
}

is_tokenlist <- function(x) {
  inherits(x, "textrecipes_tokenlist")
}

#' @export
format.textrecipes_tokenlist <- function(x, ...) {
  out <- formatC(lengths(vec_data(x)))
  paste0("[", out, " tokens]")
}

#' @export
vec_ptype_abbr.textrecipes_tokenlist <- function(x, ...) {
  "tknlist"
}

#' @export
obj_print_footer.textrecipes_tokenlist <- function(x, ...) {
  cat("# Unique Tokens: ", format(length(attr(x, "tokens"))), "\n", sep = "")
}

#' @export
`[.textrecipes_tokenlist`<- function(x, i) {
  if (is.null(attr(x, "lemma"))) {
    lemma <- NULL
  } else {
    lemma <- attr(x, "lemma")[i]
  }
  if (is.null(attr(x, "pos"))) {
    pos <- NULL
  } else {
    pos <- attr(x, "pos")[i]
  }
  tokenlist(vec_data(x)[i], lemma = lemma, pos = pos)
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
  
  if (!is.null(attr(x, "pos"))) {
    pos <- split(unlist(attr(x, "pos"))[keep_id], split_id)
    names(pos) <- NULL
  } else {
    pos <- NULL
  }
  
  new_tokenlist(out, lemma = lemma, pos = pos, tokens = dict)
}

tokenlist_apply <- function(x, fun, arguments = NULL) {
  if (!is_tokenlist(x)) {
    rlang::abort("Input must be a tokenlist.")
  }
  
  apply_expr <- expr(lapply(x, fun))
  
  if (length(arguments) > 0)
    apply_expr <- mod_call_args(apply_expr, args = arguments)
  
  tokenlist(eval(apply_expr))
}

# Takes a [tokenlist] and calculate the token count matrix
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

tokenlist_lemma <- function(x) {
  tokenlist(attr(x, "lemma"))
}

tokenlist_pos_filter <- function(x, pos_tags) {
  if (!is_tokenlist(x)) {
    rlang::abort("Input must be a tokenlist.")
  }
  
  if (is.null(attr(x, "pos"))) {
    rlang::abort("pos attribute not avaliable.")
  }
  
  seq_x <- seq_along(x)
  i <- rep(seq_x, lengths(x))
  j <- match(unlist(attr(x, "pos")), pos_tags)
  
  keep_id <- !is.na(j)
  split_id <- factor(i[keep_id], seq_x)
  
  out <- split(unlist(vec_data(x))[keep_id], split_id)
  names(out) <- NULL
  
  pos <- split(unlist(attr(x, "pos"))[keep_id], split_id)
  names(pos) <- NULL
  
  if (!is.null(attr(x, "lemma"))) {
    lemma <- split(unlist(attr(x, "lemma"))[keep_id], split_id)
    names(lemma) <- NULL
  } else {
    lemma <- NULL
  }
  
  tokenlist(out, lemma = lemma, pos = pos)
}

tokenlist_ngram <- function(x, n, delim) {
  if (!is_tokenlist(x)) {
    rlang::abort("Input must be a tokenlist.")
  }
  
  tokenlist(rcpp_ngram(vec_data(x), n, delim))
}
