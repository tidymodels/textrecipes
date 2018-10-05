#'  Term frequency-inverse document frequency of tokens
#'
#' `step_tfidf` creates a *specification* of a recipe step that
#'  will convert a list of tokens into multiple variables containing
#'  the Term frequency-inverse document frequency of tokens.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_tfidf`, this indicates the variables to be encoded
#'  into a list column. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param tf.weight A character determining the weighting scheme for
#'  the term frequency calculations. Must be one of "binary", 
#'  "raw count", "term frequency", "log normalization" or
#'  "double normalization". Defaults to "raw count".
#' @param K A numeric weight used if `tf.weight` is set to
#'  "double normalization". Defaults to 0.5.
#' @param idf.weight A character determining the weighting scheme
#'  for the inverse document frequency calculations. Must be one of
#'  "unary", "idf", "idf smooth" or "idf max".
#'  Defaults to "idf".
#' @param idf.adjustment Numeric added to the denominator of inverse
#'  document frequency calculation to avoid division-by-zero. Defaults
#'  to 1.
#' @param res The words that will be used to calculate the term 
#'  frequency will be stored here once this preprocessing step has 
#'  be trained by [prep.recipe()].
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. See notes below
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake.recipe()]? While all
#'  operations are baked when [recipes::prep.recipe()] is run, some
#'  operations may not be able to be conducted on new data (e.g.
#'  processing the outcome variable(s)). Care should be taken when
#'  using `skip = TRUE` as it may affect the computations for
#'  subsequent operations
#' @param trained A logical to indicate if the recipe has been
#'  baked.
#' @return An updated version of `recipe` with the new step added
#'  to the sequence of existing steps (if any).
#' @examples
#' library(recipes)
#' 
#' data(okc_text)
#' 
#' okc_rec <- recipe(~ ., data = okc_text) %>%
#'   step_tokenize(essay0) %>%
#'   step_tfidf(essay0)
#'   
#' okc_obj <- okc_rec %>%
#'   prep(training = okc_text, retain = TRUE)
#'   
#' bake(okc_obj, okc_text)
#' 
#' tidy(okc_rec, number = 2)
#' tidy(okc_obj, number = 2)
#' @export
#' @details
#' Term frequency-inverse document frequency is the product of two statistics.
#' The term frequency (TF) and the inverse document frequency (IDF). 
#' 
#' Term frequency is a weight of how many times each token appear in each 
#' observation. There are different ways to calculate the weight and this 
#' step can do it in a couple of ways. Setting the argument `tf.weight` to
#' "binary" will result in a set of binary variables denoting if a token
#' is present in the observation. "raw count" will count the times a token
#' is present in the observation. "term frequency" will devide the count
#' with the total number of words in the document to limit the effect 
#' of the document length as longer documents tends to have the word present
#' more times but not necessarily at a higher procentage. "log normalization"
#' takes the log of 1 plus the count, adding 1 is done to avoid taking log of
#' 0. Finally "double normalization" is the raw frequency divided by the raw 
#' frequency of the most occurring term in the document. This is then 
#' multiplied by `K` and `K` is added tot he result. This is again done to 
#' prevent a bias towards longer documents.
#' 
#' Inverse document frequency is a measure of how much information a word
#' gives, in other words, how common or rare is the word across all the 
#' observations. If a word appears in all the observations it might not
#' give us that much insight, but if it only appear in some it might help
#' us differentiate the observations. 
#' 
#' Setting the argument `idf.weight` to "unary" will result in a IDF of 1,
#' thus simplifying this step to `step_tf`. "idf" is calculated by deviding
#' the number of observations with how many observatiuons the token appear 
#' in and taking the log of that. This will result in a devide-by-zero error
#' if a token doesn't appear in the data so an adjustment is done by adding 
#' `idf.adjustment` to the count of appearences. "idf smooth" is done by 
#' taking the log of 1 plus "idf". "idf max" is done in a similar way to
#' "idf" but instead of looking at the number of observations, we look at the
#' maximum number of observations a term appeared.
#' 
#' The new components will have names that begin with `prefix`, then
#' the name of the variable, followed by the tokens all seperated by
#' `-`. The new variables will be created alphabetically according to
#' token.
#' 
#' @seealso [step_hashing()] [step_tf()] [step_tokenize()]
#' @importFrom recipes add_step step terms_select sel2char ellipse_check 
#' @importFrom recipes check_type
step_tfidf <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           tf.weight = "raw count",
           K = 0.5,
           idf.weight = "idf",
           idf.adjustment = 1,
           res = NULL,
           prefix = "tfidf",
           skip = FALSE) {
    
    if(!(tf.weight %in% tf_funs) | length(tf.weight) != 1)
      stop("`tf.weight` should be one of: ",
           paste0("'", tf_funs, "'", collapse = ", "),
           call. = FALSE)
    
    if(!(idf.weight %in% idf_funs) | length(idf.weight) != 1)
      stop("`idf.weight` should be one of: ",
           paste0("'", idf_funs, "'", collapse = ", "),
           call. = FALSE)
    
    if(idf.adjustment < 0 | is.na(idf.adjustment))
      stop("`idf.adjustment` must be a positive number.",
           call. = FALSE)
    
    add_step(
      recipe,
      step_tfidf_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        res = res,
        columns = columns,
        tf.weight = tf.weight,
        K = K,
        idf.weight = idf.weight,
        idf.adjustment = idf.adjustment,
        prefix = prefix,
        skip = skip
      )
    )
  }

idf_funs <- c("unary", "idf", "idf smooth", "idf max")

step_tfidf_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           columns = NULL,
           tf.weight = NULL,
           K = NULL,
           idf.weight = NULL,
           idf.adjustment = NULL,
           res = NULL,
           prefix = "tfidf",
           skip = FALSE) {
    step(
      subclass = "tfidf",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      tf.weight = tf.weight,
      K = K,
      idf.weight = idf.weight,
      idf.adjustment = idf.adjustment,
      res = res,
      prefix = prefix,
      skip = skip
    )
  }

#' @export
prep.step_tfidf <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  check_list(training[, col_names])
  
  token_list <- list()
  
  for (i in seq_along(col_names)) {
    token_list[[i]] <- sort(unique(unlist(training[, col_names[i], drop = TRUE])))
  }
  
  step_tfidf_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    tf.weight = x$tf.weight,
    K = x$K,
    idf.weight = x$idf.weight,
    idf.adjustment = x$idf.adjustment,
    res = token_list,
    prefix = x$prefix,
    skip = x$skip
  )
}

#' @export
#' @importFrom tibble as_tibble tibble
#' @importFrom recipes bake prep
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
bake.step_tfidf <- function(object, newdata, ...) {
  col_names <- object$columns
  # for backward compat
  
  for (i in seq_along(col_names)) {
    
    tfidf_text <- tfidf_function(newdata[, col_names[i], drop = TRUE],
                                 object$res[[i]],
                                 paste0(object$prefix, "-", col_names[i]),
                                 object$tf.weight,
                                 object$K,
                                 object$idf.weight,
                                 object$idf.adjustment)
    
    newdata <- bind_cols(newdata, tfidf_text)
    
    newdata <-
      newdata[, !(colnames(newdata) %in% col_names[i]), drop = FALSE]
  }
  
  as_tibble(newdata)
}

tfidf_function <- function(data, names, labels, tf.weights, K, idf.weight,
                           adjustment) {
  
  counts <- list_to_count_matrix(data, names)
  
  tf <- tf_weight(counts, tf.weights, K)
  
  idf <- idf_weight(counts, idf.weight, adjustment)
  
  tfidf <- t(t(tf) * idf)
  
  colnames(tfidf) <- paste0(labels, "-", names)
  as_tibble(tfidf)
}

idf_weight <- function(x, scheme, adjustment) {
  if(scheme == "unary")
    return(1)
  
  if(scheme == "idf") {
    N <- nrow(x)
    return(log(N / (colSums(x > 0) + adjustment)))
  }
  
  if(scheme == "idf smooth") {
    N <- nrow(x)
    return(log(1 + N / (colSums(x > 0) + adjustment)))
  }
  
  if(scheme == "idf max") {
    nt <- colSums(x > 0)
    return(log(max(nt) / (nt + adjustment)))
  }
}

#' @importFrom recipes printer
#' @export
print.step_tfidf <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Term frequency-inverse document frequency with ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_tfidf
#' @param x A `step_tfidf` object.
#' @importFrom rlang na_chr
#' @export
tidy.step_tfidf <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$terms,
                  value = x$idf.weight,
                  tf = x$tf.weight)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_chr,
                  tf = na_chr)
  }
  res
}