#' Term Frequency-Inverse Document Frequency of Tokens
#'
#' `step_tfidf()` creates a *specification* of a recipe step that will convert a
#' [`token`][tokenlist()] variable into multiple variables containing the term
#' frequency-inverse document frequency of tokens.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param vocabulary A character vector of strings to be considered.
#' @param res The words that will be used to calculate the term frequency will
#'   be stored here once this preprocessing step has be trained by
#'   [prep.recipe()].
#' @param smooth_idf TRUE smooth IDF weights by adding one to document
#'   frequencies, as if an extra document was seen containing every term in the
#'   collection exactly once. This prevents division by zero.
#' @param norm A character, defines the type of normalization to apply to term
#'   vectors. "l1" by default, i.e., scale by the number of words in the
#'   document. Must be one of c("l1", "l2", "none").
#' @param sublinear_tf A logical, apply sublinear term-frequency scaling, i.e.,
#'   replace the term frequency with 1 + log(TF). Defaults to FALSE.
#' @template args-prefix
#' @template args-keep_original_cols
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
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
#' Inverse document frequency is a measure of how informative a word is, e.g.,
#' how common or rare the word is across all the observations. If a word appears
#' in all the observations it might not give that much insight, but if it only
#' appears in some it might help differentiate between observations.
#'
#' The IDF is defined as follows: idf = log(1 + (# documents in the corpus) / (#
#' documents where the term appears))
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the selectors or variables selected), `token` (name of the tokens),
#' `weight` (the calculated IDF weight) is returned.
#'
#' @template details-prefix
#'
#' @template case-weights-not-supported
#'
#' @seealso [step_tokenize()] to turn characters into [`tokens`][tokenlist()]
#' @family Steps for Numeric Variables From Tokens
#'
#' @examples
#' \donttest{
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~., data = tate_text) %>%
#'   step_tokenize(medium) %>%
#'   step_tfidf(medium)
#'
#' tate_obj <- tate_rec %>%
#'   prep()
#'
#' bake(tate_obj, tate_text)
#'
#' tidy(tate_rec, number = 2)
#' tidy(tate_obj, number = 2)
#' }
#'
#' @export
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
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("tfidf")) {
    add_step(
      recipe,
      step_tfidf_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        vocabulary = vocabulary,
        res = res,
        smooth_idf = smooth_idf,
        norm = norm,
        sublinear_tf = sublinear_tf,
        columns = columns,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_tfidf_new <-
  function(terms, role, trained, columns, vocabulary, res, smooth_idf, norm,
           sublinear_tf, prefix, keep_original_cols, skip, id) {
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
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_tfidf <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names], types = "tokenlist")

  idf_weights <- list()

  for (col_name in col_names) {
    tokens <- training[[col_name]]
    vocabulary <- x$vocabulary %||% sort(get_unique_tokens(tokens))
    column_dtm <- tokenlist_to_dtm(tokens, vocabulary)
    idf_weights[[col_name]] <- calc_idf(column_dtm, x$smooth_idf)
  }

  step_tfidf_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    vocabulary = x$vocabulary,
    res = idf_weights,
    smooth_idf = x$smooth_idf,
    norm = x$norm,
    sublinear_tf = x$sublinear_tf,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_tfidf <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  if (is.null(names(object$res))) {
    # Backwards compatibility with 1.0.3 (#230)
    names(object$res) <- col_names
  }
  
  for (col_name in col_names) {
    tfidf_text <- tfidf_function(
      new_data[[col_name]],
      object$res[[col_name]],
      paste0(object$prefix, "_", col_name),
      object$smooth_idf,
      object$norm,
      object$sublinear_tf
    )

    tfidf_text <- check_name(tfidf_text, new_data, object, names(tfidf_text))

    new_data <- vec_cbind(new_data, tfidf_text)
  }
  
  new_data <- remove_original_cols(new_data, object, col_names)
  
  new_data
}

#' @export
print.step_tfidf <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Term frequency-inverse document frequency with "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_tfidf` object.
#' @export
tidy.step_tfidf <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$columns) == 0) {
      res <- tibble(
        terms = character(),
        token = character(),
        weight = double()
      )
    } else {
      res <- purrr::map2_dfr(
        x$columns, x$res,
        ~ tibble(
          terms = .x,
          token = names(.y),
          weight = unname(.y)
        )
      )
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      token = NA_character_,
      weight = na_dbl
    )
  }
  res$id <- x$id
  res
}

# Implementation
tfidf_function <- function(data, weights, labels, smooth_idf, norm,
                           sublinear_tf) {
  # Backwards compatibility with 1592690d36581fc5f4952da3e9b02351b31f1a2e
  if (is.numeric(weights)) {
    dict <- names(weights)
  } else {
    dict <- weights
  }
  counts <- tokenlist_to_dtm(data, dict)

  tfidf <- dtm_to_tfidf(counts, weights, smooth_idf, norm, sublinear_tf)

  colnames(tfidf) <- paste0(labels, "_", dict)
  as_tibble(tfidf)
}

dtm_to_tfidf <- function(dtm, idf_weights, smooth_idf, norm, sublinear_tf) {
  dtm <- normalize(dtm, norm)

  if (sublinear_tf) {
    dtm@x <- 1 + log(dtm@x)
  }
  if (is.character(idf_weights)) {
    rlang::warn(
      c(
        "Please retrain this recipe with version 0.5.1 or higher.",
        "A data leakage bug has been fixed for `step_tfidf()`."
      )
    )
    idf_weights <- log(smooth_idf + nrow(dtm) / Matrix::colSums(dtm > 0))
    out <- dtm %*% Matrix::Diagonal(x = idf_weights)
  } else {
    out <- dtm %*% Matrix::Diagonal(x = idf_weights)
  }
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

calc_idf <- function(dtm, smooth) {
  log(smooth + nrow(dtm) / Matrix::colSums(dtm > 0))
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_tfidf <- function(x, ...) {
  c("textrecipes")
}
