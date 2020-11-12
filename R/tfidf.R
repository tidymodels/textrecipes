#'  Term frequency-inverse document frequency of tokens
#'
#' `step_tfidf` creates a *specification* of a recipe step that
#'  will convert a [tokenlist] into multiple variables containing
#'  the term frequency-inverse document frequency of tokens.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_tfidf`, this indicates the variables to be encoded
#'  into a [tokenlist]. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new columns created by the original variables will be
#'  used as predictors in a model.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param vocabulary A character vector of strings to be considered.
#' @param res The words that will be used to calculate the term
#'  frequency will be stored here once this preprocessing step has
#'  be trained by [prep.recipe()].
#' @param smooth_idf TRUE smooth IDF weights by adding one to document
#'  frequencies, as if an extra document was seen containing every term
#'  in the collection exactly once. This prevents division by zero.
#' @param norm A character, defines the type of normalization to apply to
#'  term vectors. "l1" by default, i.e., scale by the number of words in the
#'  document. Must be one of c("l1", "l2", "none").
#' @param sublinear_tf A logical, apply sublinear term-frequency scaling, i.e.,
#'  replace the term frequency with 1 + log(TF). Defaults to FALSE.
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. See notes below.
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
#' library(modeldata)
#' data(okc_text)
#'
#' okc_rec <- recipe(~., data = okc_text) %>%
#'   step_tokenize(essay0) %>%
#'   step_tfidf(essay0)
#'
#' okc_obj <- okc_rec %>%
#'   prep()
#'
#' bake(okc_obj, okc_text)
#'
#' tidy(okc_rec, number = 2)
#' tidy(okc_obj, number = 2)
#' }
#' @export
#' @details
#' It is strongly advised to use [step_tokenfilter] before using [step_tfidf] to
#' limit the number of variables created; otherwise you may run into memory
#' issues. A good strategy is to start with a low token count and increase
#' depending on how much RAM you want to use.
#'
#' Term frequency-inverse document frequency is the product of two statistics:
#' the term frequency (TF) and the inverse document frequency (IDF).
#'
#' Term frequency measures how many times each token appears in each
#' observation.
#'
#' Inverse document frequency is a measure of how informative a word
#' is, e.g., how common or rare the word is across all the
#' observations. If a word appears in all the observations it might not
#' give that much insight, but if it only appears in some it might help
#' differentiate between observations.
#'
#' The IDF is defined as follows: idf = log(1 + (# documents in the corpus) /
#' (# documents where the term appears))
#'
#' The new components will have names that begin with `prefix`, then
#' the name of the variable, followed by the tokens all separated by
#' `-`. The new variables will be created alphabetically according to
#' token.
#'
#' @seealso [step_tokenize()] to turn character into tokenlist.
#' @family tokenlist to numeric steps
step_tfidf <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           columns = NULL,
           vocabulary = NULL,
           res = NULL,
           smooth_idf = TRUE,
           norm = "l1",
           sublinear_tf = FALSE,
           prefix = "tfidf",
           skip = FALSE,
           id = rand_id("tfidf")) {
    add_step(
      recipe,
      step_tfidf_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        vocabulary = vocabulary,
        res = res,
        smooth_idf = smooth_idf,
        norm = norm,
        sublinear_tf = sublinear_tf,
        columns = columns,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

step_tfidf_new <-
  function(terms, role, trained, columns, vocabulary, res, smooth_idf, norm,
           sublinear_tf, prefix, skip, id) {
    step(
      subclass = "tfidf",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      vocabulary = vocabulary,
      res = res,
      smooth_idf = smooth_idf,
      norm = norm,
      sublinear_tf = sublinear_tf,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_tfidf <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  check_list(training[, col_names])

  token_list <- list()

  for (i in seq_along(col_names)) {
    token_list[[i]] <- x$vocabulary %||%
      sort(get_unique_tokens(training[, col_names[i], drop = TRUE]))
  }

  step_tfidf_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    vocabulary = x$vocabulary,
    res = token_list,
    smooth_idf = x$smooth_idf,
    norm = x$norm,
    sublinear_tf = x$sublinear_tf,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_tfidf <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (i in seq_along(col_names)) {
    tfidf_text <- tfidf_function(
      new_data[, col_names[i], drop = TRUE],
      object$res[[i]],
      paste0(object$prefix, "_", col_names[i]),
      object$smooth_idf,
      object$norm,
      object$sublinear_tf
    )

    new_data <-
      new_data[, !(colnames(new_data) %in% col_names[i]), drop = FALSE]

    new_data <- vctrs::vec_cbind(new_data, tfidf_text)
  }
  as_tibble(new_data)
}

#' @export
print.step_tfidf <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Term frequency-inverse document frequency with ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_tfidf
#' @param x A `step_tfidf` object.
#' @export
tidy.step_tfidf <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$terms)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

# Implementation
tfidf_function <- function(data, names, labels, smooth_idf, norm,
                           sublinear_tf) {
  counts <- tokenlist_to_dtm(data, names)

  tfidf <- dtm_to_tfidf(counts, smooth_idf, norm, sublinear_tf)

  colnames(tfidf) <- paste0(labels, "_", names)
  as_tibble(tfidf)
}

dtm_to_tfidf <- function(dtm, smooth_idf, norm, sublinear_tf) {
  dtm <- normalize(dtm, norm)

  if (sublinear_tf) {
    dtm@x <- 1 + log(dtm@x)
  }

  out <- dtm %*% Matrix::Diagonal(x = log(smooth_idf + nrow(dtm) /
    Matrix::colSums(dtm > 0)))
  as.matrix(out)
}

normalize <- function(dtm, norm = c("l1", "l2", "none")) {
  if (norm == "none") {
    return(dtm)
  }

  norm_vec <- switch(norm,
    l1 = 1 / Matrix::rowSums(dtm),
    l2 = 1 / sqrt(Matrix::rowSums(dtm^2))
  )

  # case when sum row elements == 0
  norm_vec[is.infinite(norm_vec)] <- 0

  Matrix::Diagonal(x = norm_vec) %*% dtm
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_tfidf <- function(x, ...) {
  c("textrecipes")
}
