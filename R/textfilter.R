#' Filter the tokens based on term frequency
#'
#' `step_textfilter` creates a *specification* of a recipe step that
#'  will convert a list of its tokenized parts into a list where the 
#'  tokens are filtered based on frequency.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_textfilter`, this indicates the variables to be encoded
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
#'  after filtering done by max.tf and min.tf.
#' @param res The words that will be keep will be stored here once 
#'  this preprocessing step has be trained by [prep.recipe()].
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
#'   step_textfilter(essay0, max.words = 10) %>%
#'   prep(training = okc_text, retain = TRUE)
#' 
#' juice(okc_rec, essay0) %>% 
#'   slice(1:2)
#' 
#' juice(okc_rec) %>% 
#'   slice(2) %>% 
#'   pull(essay0) 
#' @keywords datagen 
#' @concept preprocessing encoding
#' @export
#' @importFrom recipes add_step step terms_select sel2char ellipse_check 
#' @importFrom recipes check_type
step_textfilter <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           max.tf = Inf,
           min.tf = 0,
           procentage = FALSE,
           max.words = NULL,
           res = NULL,
           skip = FALSE
  ) {
    add_step(
      recipe,
      step_textfilter_new(
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

step_textfilter_new <-
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
      subclass = "textfilter",
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
prep.step_textfilter <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  check_list(training[, col_names])
  
  retain_words <- list()
  
  for (i in seq_along(col_names)) {
    retain_words[[i]] <- textfilter_fun(training[, col_names[i], drop = TRUE],
                                        x$max.tf, x$min.tf, x$max.words,
                                        x$procentage)
  }
  
  step_textfilter_new(
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
bake.step_textfilter <- function(object, newdata, ...) {
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

textfilter_fun <- function(data, max_tf, min_tf, max_features, procentage) {
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
print.step_textfilter <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Text filtering for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
}