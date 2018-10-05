#' Filter the tokens based on term frequency
#'
#' `step_tokenfilter` creates a *specification* of a recipe step that
#'  will convert a list of tokens into a list where the tokens are filtered
#'  based on frequency.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_tokenfilter`, this indicates the variables to be encoded
#'  into a list column. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param max.tf An integer. Maximal number of times a word can appear
#'  before getting removed.
#' @param min.tf An integer. Minimum number of times a word can appear
#'  before getting removed.
#' @param procentage A logical. Should max.tf and min.tf be interpreded 
#'  as a procentage instead of count.
#' @param max.words An integer. Will only keep the top max.words words
#'  after filtering done by max.tf and min.tf. Defaults to 100.
#' @param res The words that will be keep will be stored here once 
#'  this preprocessing step has be trained by [prep.recipe()].
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake.recipe()]? While all
#'  operations are baked when [recipes::prep.recipe()] is run, some
#'  operations may not be able to be conducted on new data (e.g.
#'  processing the outcome variable(s)). Care should be taken when
#'  using `skip = TRUE` as it may affect the computations for
#'  subsequent operations.
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
#'   step_tokenfilter(essay0) 
#'   
#' okc_obj <- okc_rec %>%
#'   prep(training = okc_text, retain = TRUE)
#' 
#' juice(okc_obj, essay0) %>% 
#'   slice(1:2)
#' 
#' juice(okc_obj) %>% 
#'   slice(2) %>% 
#'   pull(essay0)
#' 
#' tidy(okc_rec, number = 2)
#' tidy(okc_obj, number = 2)
#' @export
#' @details
#' This step allow you to limit the tokens you are looking at by filtering
#' on their occurance in the corpus. You are able to exclude tokens if they
#' appear too many times or too fews times in the data. It can be specified
#' as counts using `max.tf` and `min.tf` or as procentages by setting
#' `procentage` as `TRUE`. In addition one can filter to only use the top
#' `max.words` used tokens.
#' 
#' It is advised to filter before using [step_tf] or [step_tfidf] to limit
#' the number of variables created.
#' 
#' @seealso [step_untokenize()]
#' @importFrom recipes add_step step terms_select sel2char ellipse_check 
#' @importFrom recipes check_type
step_tokenfilter <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           max.tf = Inf,
           min.tf = 0,
           procentage = FALSE,
           max.words = 100,
           res = NULL,
           skip = FALSE) {
    
    if(procentage &&(max.tf > 1 | max.tf < 0 | min.tf > 1 | min.tf < 0))
      stop("`max.tf` and `min.tf` should be in the interval [0, 1].",
           call. = FALSE)
      
    add_step(
      recipe,
      step_tokenfilter_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        max.tf = max.tf,
        min.tf = min.tf,
        procentage = procentage,
        max.words = max.words,
        res = res,
        skip = skip
      )
    )
  }

step_tokenfilter_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           columns = NULL,
           max.tf = NULL,
           min.tf = NULL,
           procentage = NULL,
           max.words = NULL,
           res = NULL,
           skip = FALSE) {
    step(
      subclass = "tokenfilter",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      max.tf = max.tf,
      min.tf = min.tf,
      procentage = procentage,
      max.words = max.words,
      res = res,
      skip = skip
    )
  }

#' @export
prep.step_tokenfilter <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  check_list(training[, col_names])
  
  retain_words <- list()
  
  for (i in seq_along(col_names)) {
    retain_words[[i]] <- tokenfilter_fun(training[, col_names[i], drop = TRUE],
                                        x$max.tf, x$min.tf, x$max.words,
                                        x$procentage)
  }
  
  step_tokenfilter_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    max.tf = x$max.tf,
    min.tf = x$min.tf,
    procentage = x$procentage,
    max.words = x$max.words,
    res = retain_words,
    skip = x$skip
  )
}

#' @export
#' @importFrom tibble as_tibble tibble
#' @importFrom recipes bake prep
#' @importFrom purrr map
bake.step_tokenfilter <- function(object, newdata, ...) {
  col_names <- object$columns
  # for backward compat
  
  for (i in seq_along(col_names)) {
    newdata[, col_names[i]] <- 
      word_tbl_filter(newdata[, col_names[i], drop = TRUE], 
                      object$res[[i]], 
                      TRUE)
  }
  newdata <- factor_to_text(newdata, col_names)
  
  as_tibble(newdata)
}

tokenfilter_fun <- function(data, max_tf, min_tf, max_features, procentage) {
  tf <- table(unlist(data))
  
  if(procentage)
    tf <- tf / sum(tf)
  
  ids <- tf < max_tf & tf > min_tf
  
  if(is.null(max_features)) {
    names(sort(tf[ids], decreasing = TRUE))
  } else {
    names(sort(tf[ids], decreasing = TRUE)[seq_len(max_features)])
  }
}

#' @importFrom recipes printer
#' @export
print.step_tokenfilter <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Text filtering for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_tokenfilter
#' @param x A `step_tokenfilter` object.
#' @importFrom rlang na_int
#' @export
tidy.step_tokenfilter <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$terms,
                  value = x$max.words)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_int)
  }
  res
}