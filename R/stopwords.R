#' Filtering of stopwords from a list-column variable
#'
#' `step_stopwords` creates a *specification* of a recipe step that
#'  will filter a list of its tokenized parts for stopwords(keep or remove).
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_stopwords`, this indicates the variables to be encoded
#'  into a list column. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param options A list of options passed to the stemmer
#' @param language A character to indicate the langauge of stopwords 
#'  by ISO 639-1 coding scheme.
#' @param keep A logical. Specifies whether to keep the stopwords or discard them.
#' @param stopword_source A character to indicate the stopwords source as listed 
#'  in `stopwords::stopwords_getsources`
#' @param custom_stopword_source A character vector to indicate a custom list of words 
#'  that cater to the users specific problem.
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
#'   step_stopwords(essay0) %>%
#'   prep(training = okc_text, retain = TRUE)
#' 
#' juice(okc_rec, essay0) %>% 
#'   slice(1:2)
#' 
#' juice(okc_rec) %>% 
#'   slice(2) %>% 
#'   pull(essay0) 
#'   
#' # With a custom stopwords list
#' 
#' okc_rec <- recipe(~ ., data = okc_text) %>%
#'   step_tokenize(essay0) %>%
#'   step_stopwords(essay0, custom_stopword_source = c("twice", "upon")) %>%
#'   prep(traimomg = okc_text, retain = TRUE)
#'   
#' juice(okc_rec) %>%
#'   slice(2) %>%
#'   pull(essay0) 
#' @keywords datagen 
#' @concept preprocessing encoding
#' @export
#' @importFrom recipes add_step step terms_select sel2char ellipse_check 
#' @importFrom recipes check_type
step_stopwords <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           options = list(),
           language = "en",
           keep = FALSE,
           stopword_source = "snowball",
           custom_stopword_source = NULL,
           skip = FALSE
  ) {
    add_step(
      recipe,
      step_stopwords_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        options = options,
        language = language,
        keep = keep,
        stopword_source = stopword_source,
        custom_stopword_source = custom_stopword_source,
        skip = skip
      )
    )
  }

step_stopwords_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           columns = NULL,
           options = NULL,
           language = NULL,
           keep = NULL,
           stopword_source = NULL,
           custom_stopword_source = NULL,
           skip = FALSE) {
    step(
      subclass = "stopwords",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      options = options,
      language = language,
      keep = keep,
      stopword_source = stopword_source,
      custom_stopword_source = custom_stopword_source,
      skip = skip
    )
  }

#' @export
prep.step_stopwords <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  check_list(training[, col_names])
  
  step_stopwords_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    options = x$options,
    language = x$language,
    keep = x$keep,
    stopword_source = x$stopword_source,
    custom_stopword_source = x$custom_stopword_source,
    skip = x$skip
  )
}

#' @export
#' @importFrom tibble as_tibble tibble
#' @importFrom recipes bake prep
#' @importFrom purrr map
bake.step_stopwords <- function(object, newdata, ...) {
  col_names <- object$columns
  
  stopword_list <- null_switch(object$custom_stopword_source, 
                               stopwords(language = object$language, 
                                         source = object$stopword_source))
  
  for (i in seq_along(col_names)) {
    newdata[, col_names[i]] <- 
      word_tbl_filter(newdata[, col_names[i], drop = TRUE], 
                      stopword_list, 
                      object$keep)
  }
  newdata <- factor_to_text(newdata, col_names)
  
  as_tibble(newdata)
}

#' @importFrom recipes printer
print.step_stopwords <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Stop word removal for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
}