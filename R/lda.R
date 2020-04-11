#' Calculates lda dimension estimates
#'
#' @description
#' `step_lda` creates a *specification* of a recipe step that
#' will return the lda dimension estimates of a text variable.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_lda`, this indicates the variables to be encoded
#'  into a [tokenlist]. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new columns created by the original variables will be
#'  used as predictors in a model.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param lda_models A WarpLDA model object from the text2vec package. If left
#' to NULL, the default, will it train its model based on the training data.
#' Look at the examples for how to fit a WarpLDA model.
#' @param num_topics integer desired number of latent topics.
#' @param prefix A prefix for generated column names, default to "lda".
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake.recipe()]? While all
#'  operations are baked when [recipes::prep.recipe()] is run, some
#'  operations may not be able to be conducted on new data (e.g.
#'  processing the outcome variable(s)). Care should be taken when
#'  using `skip = TRUE` as it may affect the computations for
#'  subsequent operations.
#' @param id A character string that is unique to this step to identify it
#' @param trained A logical to indicate if the recipe has been
#'  baked.
#' @return An updated version of `recipe` with the new step added
#'  to the sequence of existing steps (if any).
#' @source \url{https://arxiv.org/abs/1301.3781}
#' @examples
#' if (requireNamespace("text2vec", quietly = TRUE)) {
#' \donttest{
#' library(recipes)
#' library(modeldata)
#' data(okc_text)
#'
#' okc_rec <- recipe(~ ., data = okc_text) %>%
#'   step_lda(essay0)
#'
#' okc_obj <- okc_rec %>%
#'   prep()
#'
#' juice(okc_obj) %>%
#'   slice(1:2)

#' tidy(okc_rec, number = 1)
#' tidy(okc_obj, number = 1)
#'
#' # Changing the number of topics.
#' recipe(~ ., data = okc_text) %>%
#'   step_lda(essay0, essay1, num_topics = 20) %>%
#'   prep() %>%
#'   juice() %>%
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
#'   step_lda(essay0, essay1, lda_models = lda_model) %>%
#'   prep() %>%
#'   juice() %>%
#'   slice(1:2)
#' }
#' }
#' @export
#' 
#' @family character to numeric steps
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
    
    recipes::recipes_pkg_check("textfeatures")
    
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

  training <- factor_to_text(training, col_names)

  check_type(training[, col_names], quant = FALSE)

  model_list <- list()

  for (i in seq_along(col_names)) {
    ddd <- utils::capture.output(
      model_list[[i]] <- x$lda_models %||%
        attr(textfeatures::word_dims(training[, col_names[i], drop = TRUE], 
                                     n = x$num_topics),
             "dict")
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

  new_data <- factor_to_text(new_data, col_names)

  for (i in seq_along(col_names)) {
    ddd <- utils::capture.output(
      tf_text <- textfeatures::word_dims_newtext(object$lda_models[[i]],
                                   new_data[, col_names[i], drop = TRUE])
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
