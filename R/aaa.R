# Takes a data.frame (data) and replaces the columns with the names (names)
# and converts them from factor variable to character variables. Keeps 
# characters variables unchanged.
factor_to_text <- function(data, names) {
  for (i in seq_along(names)) {
    if(is.factor(data[, names[i], drop = TRUE]))
      data[, names[i]] <- as.character.factor(data[, names[i], drop = TRUE])
  }
  data
}

## This function takes the default arguments of `cl` (call object) and
## replaces them with the matching ones in `options` and
## remove any in `removals`
mod_call_args <- function(cl, args, removals = NULL) {
  if (!is.null(removals))
    for (i in removals)
      cl[[i]] <- NULL
    arg_names <- names(args)
    for (i in arg_names)
      cl[[i]] <- args[[i]]
    cl
}

check_list <- function (dat) {

  all_good <- vapply(dat, is.list, logical(1))
  label <- "numeric"

  if (!all(all_good)) 
    stop("All columns selected for the step should be a list-column", 
         call. = FALSE)
  invisible(all_good)
}

# Takes a vector of character vectors and keeps (for keep = TRUE) the words
# or removes (for keep = FALSE) the words
#' @importFrom purrr keep
#' @importFrom stopwords stopwords
word_list_filter <- function(x, words, keep) {
  
  if(!keep) {
    return(keep(x, !(x %in% words)))
  }
  else {
    return(keep(x, x %in% words))
  }
}
# same as word_list_filter but takes an list as input and returns a tibble with
# list-column.
word_tbl_filter <- function(x, words, keep) {
  tibble(
    map(x, word_list_filter, words, keep)
  )
}

# Takes a list of tokens and calculate the token count matrix
#' @importFrom text2vec itoken create_dtm vocab_vectorizer create_vocabulary
list_to_dtm <- function(x, values) {
  
  it <- itoken(x, progress = FALSE)
  vectorizer <- vocab_vectorizer(create_vocabulary(values))
  create_dtm(it, vectorizer)
}