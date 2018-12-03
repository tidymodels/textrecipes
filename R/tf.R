#' Term frequency of tokens
#'
#' `step_tf` creates a *specification* of a recipe step that
#'  will convert a list of tokens into multiple variables containing
#'  the token counts.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_tf`, this indicates the variables to be encoded
#'  into a list column. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new columns created by the original variables will be 
#'  used as predictors in a model.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param weight_scheme A character determining the weighting scheme for
#'  the term frequency calculations. Must be one of "binary", 
#'  "raw count", "term frequency", "log normalization" or
#'  "double normalization". Defaults to "raw count".
#' @param weight A numeric weight used if `weight_scheme` is set to
#'  "double normalization". Defaults to 0.5.
#' @param vocabulary A character vector of strings to be considered.
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
#'  subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#' @param trained A logical to indicate if the recipe has been
#'  baked.
#' @return An updated version of `recipe` with the new step added
#'  to the sequence of existing steps (if any).
#' @examples
#' \donttest{
#' library(recipes)
#' 
#' data(okc_text)
#' 
#' okc_rec <- recipe(~ ., data = okc_text) %>%
#'   step_tokenize(essay0) %>%
#'   step_tf(essay0)
#'   
#' okc_obj <- okc_rec %>%
#'   prep(training = okc_text, retain = TRUE)
#'   
#' bake(okc_obj, okc_text)
#' 
#' tidy(okc_rec, number = 2)
#' tidy(okc_obj, number = 2)
#' }
#' @export
#' @details
#' Term frequency is a weight of how many times each token appear in each 
#' observation. There are different ways to calculate the weight and this 
#' step can do it in a couple of ways. Setting the argument `weight_scheme` to
#' "binary" will result in a set of binary variables denoting if a token
#' is present in the observation. "raw count" will count the times a token
#' is present in the observation. "term frequency" will devide the count
#' with the total number of words in the document to limit the effect 
#' of the document length as longer documents tends to have the word present
#' more times but not necessarily at a higher procentage. "log normalization"
#' takes the log of 1 plus the count, adding 1 is done to avoid taking log of
#' 0. Finally "double normalization" is the raw frequency divided by the raw 
#' frequency of the most occurring term in the document. This is then 
#' multiplied by `weight` and `weight` is added tot he result. This is again done to 
#' prevent a bias towards longer documents.
#' 
#' The new components will have names that begin with `prefix`, then
#' the name of the variable, followed by the tokens all seperated by
#' `-`. The new variables will be created alphabetically according to
#' token.
#' 
#' @seealso [step_hashing()] [step_tfidf()] [step_tokenize()]
#' @importFrom recipes add_step step terms_select sel2char ellipse_check 
#' @importFrom recipes check_type rand_id
step_tf <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           columns = NULL,
           weight_scheme = "raw count",
           weight = 0.5,
           vocabulary = NULL,
           res = NULL,
           prefix = "tf",
           skip = FALSE,
           id = rand_id("tf")
  ) {
    
    if(!(weight_scheme %in% tf_funs) | length(weight_scheme) != 1)
      stop("`weight_scheme` should be one of: ",
           paste0("'", tf_funs, "'", collapse = ", "),
           call. = FALSE)
    
    add_step(
      recipe,
      step_tf_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        res = res,
        columns = columns,
        weight_scheme = weight_scheme,
        weight = weight,
        vocabulary = vocabulary,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

tf_funs <- c("binary", "raw count", "term frequency", "log normalization",
             "double normalization")

step_tf_new <-
  function(terms, role, trained, columns, weight_scheme, weight, vocabulary,
           res, prefix, skip, id) {
    step(
      subclass = "tf",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      weight_scheme = weight_scheme,
      weight = weight,
      vocabulary = vocabulary,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_tf <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  check_list(training[, col_names])
  
  token_list <- list()
  
  for (i in seq_along(col_names)) {
    token_list[[i]] <- x$vocabulary %||% 
      sort(unique(unlist(training[, col_names[i], drop = TRUE])))
  }
  
  step_tf_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    weight_scheme = x$weight_scheme,
    weight = x$weight,
    vocabulary = x$vocabulary,
    res = token_list,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
#' @importFrom tibble as_tibble tibble
#' @importFrom recipes bake prep
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
bake.step_tf <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat
  
  for (i in seq_along(col_names)) {
    
    tf_text <- tf_function(new_data[, col_names[i], drop = TRUE],
                           object$res[[i]],
                           paste0(object$prefix, "_", col_names[i]),
                           object$weight_scheme,
                           object$weight)
    
    new_data <- bind_cols(new_data, tf_text)
    
    new_data <-
      new_data[, !(colnames(new_data) %in% col_names[i]), drop = FALSE]
  }
  
  as_tibble(new_data)
}

tf_function <- function(data, names, labels, weights, weight) {
  
  counts <- as.matrix(list_to_dtm(data, names))
  
  tf <- tf_weight(counts, weights, weight)
  colnames(tf) <- paste0(labels, "_", names)
  as_tibble(tf)
}

tf_weight <- function(x, scheme, weight) {
  if(scheme == "binary")
    return(x > 0)
  
  if(scheme == "raw count")
    return(x)
  
  if(scheme == "term frequency")
    return(x / rowSums(x))
  
  if(scheme == "log normalization")
    return(log(1 + x))
  
  if(scheme == "double normalization") {
    max_ftd <- apply(x, 1, max)
    return(weight + weight * x / max_ftd)
  }
}

#' @importFrom recipes printer
#' @export
print.step_tf <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Term frequency with ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_tf
#' @param x A `step_tf` object.
#' @importFrom rlang na_chr na_int
#' @export
tidy.step_tf <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$terms,
                  value = x$weight_scheme)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_chr)
  }
  res$id <- x$id
  res
}