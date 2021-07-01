#' Calculates lda dimension estimates
#'
#' @description
#' `step_lda` creates a *specification* of a recipe step that
#' will return the lda dimension estimates of a text variable.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param lda_models A WarpLDA model object from the text2vec package. If left
#' to NULL, the default, will it train its model based on the training data.
#' Look at the examples for how to fit a WarpLDA model.
#' @param num_topics integer desired number of latent topics.
#' @param prefix A prefix for generated column names, default to "lda".
#' @template args-skip
#' @template args-id
#' 
#' @source \url{https://arxiv.org/abs/1301.3781}
#' 
#' @template returns
#' 
#' @family character to numeric steps
#' 
#' @examples
#' if (requireNamespace("text2vec", quietly = TRUE)) {
#' \donttest{
#' library(recipes)
#' library(modeldata)
#' data(okc_text)
#'
#' okc_rec <- recipe(~ ., data = okc_text) %>%
#'   step_tokenize(essay0) %>%
#'   step_lda(essay0)
#'
#' okc_obj <- okc_rec %>%
#'   prep()
#'
#' bake(okc_obj, new_data = NULL) %>%
#'   slice(1:2)

#' tidy(okc_rec, number = 1)
#' tidy(okc_obj, number = 1)
#'
#' # Changing the number of topics.
#' recipe(~ ., data = okc_text) %>%
#'   step_tokenize(essay0, essay1) %>%
#'   step_lda(essay0, essay1, num_topics = 20) %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
#'   slice(1:2)
#'
#' # Supplying A pre-trained LDA model trained using text2vec
#' library(text2vec)
#' tokens <- word_tokenizer(tolower(okc_text$essay5))
#' it <- itoken(tokens, ids = seq_along(okc_text$essay5))
#' v <- create_vocabulary(it)
#' dtm <- create_dtm(it, vocab_vectorizer(v))
#' lda_model <- LDA$new(n_topics = 15)
#'
#' recipe(~ ., data = okc_text) %>%
#'   step_tokenize(essay0, essay1) %>%
#'   step_lda(essay0, essay1, lda_models = lda_model) %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
#'   slice(1:2)
#' }
#' }
#' 
#' @export
step_lda <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           columns = NULL,
           lda_models = NULL,
           num_topics = 10,
           prefix = "lda",
           skip = FALSE,
           id = rand_id("lda")
  ) {
    
    recipes::recipes_pkg_check(required_pkgs.step_lda())
    
    add_step(
      recipe,
      step_lda_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        lda_models = lda_models,
        num_topics = num_topics,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

step_lda_new <-
  function(terms, role, trained, columns, lda_models, num_topics, prefix,
           skip, id) {
    step(
      subclass = "lda",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      lda_models = lda_models,
      num_topics = num_topics,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_lda <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  check_lda_character(training[, col_names])
  
  check_list(training[, col_names])

  model_list <- list()

  for (i in seq_along(col_names)) {
    tokens <- get_tokens(training[, col_names[i], drop = TRUE])
    
    ddd <- utils::capture.output(
      model_list[[i]] <- x$lda_models %||%
        attr(word_dims(tokens, n = x$num_topics), "dict")
    )
  }

  step_lda_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    lda_models = model_list,
    num_topics = x$num_topics,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_lda <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (i in seq_along(col_names)) {
    tokens <- get_tokens(new_data[, col_names[i], drop = TRUE])
    
    ddd <- utils::capture.output(
      tf_text <- word_dims_newtext(object$lda_models[[i]], tokens)
    )
    
    attr(tf_text, "dict") <- NULL
    colnames(tf_text) <- paste(object$prefix, col_names[i], colnames(tf_text),
                               sep = "_")

    new_data <- vctrs::vec_cbind(new_data, tf_text)

    new_data <-
      new_data[, !(colnames(new_data) %in% col_names[i]), drop = FALSE]
  }

  as_tibble(new_data)
}

#' @export
print.step_lda <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Text feature extraction for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_lda
#' @param x A `step_lda` object.
#' @export
tidy.step_lda <- function(x, ...) {
  if (is_trained(x)) {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  num_topics = x$num_topics)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  num_topics = NA)
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_lda <- function(x, ...) {
  c("textfeatures", "textrecipes")
}

word_dims <- function(tokens, n = 10, n_iter = 20) {
  it <- text2vec::itoken(tokens, ids = seq_along(tokens))
  v <- text2vec::create_vocabulary(it)
  v <- text2vec::prune_vocabulary(
    v, 
    term_count_min = 2, 
    vocab_term_max = n * 50
  )
  dtm <- text2vec::create_dtm(it, text2vec::vocab_vectorizer(v))
  lda_model <- text2vec::LDA$new(n_topics = n)
  d <- lda_model$fit_transform(dtm, n_iter = n_iter)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  names(d) <- paste0("w", seq_len(ncol(d)))
  row.names(d) <- NULL
  attr(d, "dict") <- lda_model
  d
}

word_dims_newtext <- function(lda_model, tokens, n_iter = 20) {
  it <- text2vec::itoken(tokens, ids = seq_along(tokens))
  v <- text2vec::create_vocabulary(it)
  v <- text2vec::prune_vocabulary(v, term_count_min = 5, doc_proportion_max = 0.2)
  dtm <- text2vec::create_dtm(it, text2vec::vocab_vectorizer(v))
  d <- lda_model$fit_transform(dtm, n_iter = n_iter)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  names(d) <- paste0("w", seq_len(ncol(d)))
  row.names(d) <- NULL
  d
}

check_lda_character <- function (dat) {
  
  character_ind <- vapply(dat, is.character, logical(1))
  factor_ind <- vapply(dat, is.factor, logical(1))
  
  all_good <- character_ind | factor_ind
  
  if (any(all_good))
    rlang::abort(
      paste0(
        "All columns selected for this step should be tokenlists.",
        "\n",
        "See https://github.com/tidymodels/textrecipes#breaking-changes",
        " for more information."
      )
    )
  
  invisible(all_good)
}