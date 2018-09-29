#' Term frequency of tokens
#'
#' `step_tf` creates a *specification* of a recipe step that
#'  will convert a list of its tokenized parts into a list with 
#'  its tokenized parts stemmed.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_tf`, this indicates the variables to be encoded
#'  into a list column. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
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
#'   step_textfilter(essay0, max.words = 100) %>%
#'   step_tf(essay0) %>%
#'   prep(training = okc_text, retain = TRUE)
#'   
#' bake(okc_rec, okc_text)
#' @keywords datagen 
#' @concept preprocessing encoding
#' @export
#' @details
#' The new components will have names that begin with `prefix`, then
#' the name of the variable, followed by the tokens all seperated by
#' `-`. The new variables will be created alphabetically according to
#' token.
#' 
#' @importFrom recipes add_step step terms_select sel2char ellipse_check 
#' @importFrom recipes check_type
step_tf <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           res = NULL,
           prefix = "tf",
           skip = FALSE
  ) {
    add_step(
      recipe,
      step_tf_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        res = res,
        columns = columns,
        prefix = prefix,
        skip = skip
      )
    )
  }

step_tf_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           columns = NULL,
           res = NULL,
           prefix = "tf",
           skip = FALSE) {
    step(
      subclass = "tf",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      res = res,
      prefix = prefix,
      skip = skip
    )
  }

#' @export
prep.step_tf <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  check_list(training[, col_names])
  
  token_list <- list()
  
  for (i in seq_along(col_names)) {
    token_list[[i]] <- sort(unique(unlist(training[, col_names[i], drop = TRUE])))
  }
  
  step_tf_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
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
bake.step_tf <- function(object, newdata, ...) {
  col_names <- object$columns
  # for backward compat
  
  for (i in seq_along(col_names)) {
    
    tf_text <- tf_function(newdata[, col_names[i], drop = TRUE],
                           object$res[[i]],
                           paste0(object$prefix, "-", col_names[i]))
    
    newdata <- bind_cols(newdata, tf_text)
    
    newdata <-
      newdata[, !(colnames(newdata) %in% col_names[i]), drop = FALSE]
  }
  
  as_tibble(newdata)
}

tf_function <- function(data, names, labels) {
  n_words <- length(names)
  
  counts <- purrr::map(data, ~ tabulate(factor(.x, names), n_words)) %>%
    unlist() %>%
    matrix(ncol = n_words, byrow = TRUE) %>%
    as_tibble()
  
  colnames(counts) <- paste0(labels, "-", names)
  counts
}
