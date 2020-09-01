#'  Generate the basic set of text features
#'
#' `step_sequence_onehot` creates a *specification* of a recipe step that
#'  will take a string and do one hot encoding for each character by position.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_sequence_onehot`, this indicates the variables to be encoded
#'  into a [tokenlist]. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new columns created by the original variables will be 
#'  used as predictors in a model.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param string_length A numeric, number of characters to keep before 
#'      discarding. Defaults to 100.
#' @param padding 'pre' or 'post', pad either before or after each sequence.
#'  defaults to 'pre'.
#' @param truncating 'pre' or 'post', remove values from sequences larger than 
#'  string_length either in the beginning or in the end of the sequence. 
#'  Defaults too 'pre'.
#' @param vocabulary A character vector, characters to be mapped to integers. 
#'  Characters not in the vocabulary will be encoded as 0. Defaults to 
#'  `letters`.
#' @param prefix A prefix for generated column names, default to "seq1hot".
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
#' @examples
#' library(recipes)
#' library(modeldata)
#' data(okc_text)
#' 
#' okc_rec <- recipe(~ essay0, data = okc_text) %>%
#'   step_tokenize(essay0) %>%
#'   step_tokenfilter(essay0) %>%
#'   step_sequence_onehot(essay0) 
#'   
#' okc_obj <- okc_rec %>%
#'   prep()
#' 
#' juice(okc_obj)
#'   
#' tidy(okc_rec, number = 1)
#' tidy(okc_obj, number = 1)
#' 
#' @export
#' @details 
#' The string will be capped by the string_length argument, strings shorter then 
#' string_length will be padded with empty characters. The encoding will assign 
#' a integer to each character in the vocabulary, and will encode accordingly. 
#' Characters not in the vocabulary will be encoded as 0.
#'
#' @source \url{https://papers.nips.cc/paper/5782-character-level-convolutional-networks-for-text-classification.pdf}
#' 
#' @family character to numeric steps
step_sequence_onehot <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           columns = NULL,
           string_length = 100,
           padding = "pre",
           truncating = "pre",
           vocabulary = NULL,
           prefix = "seq1hot",
           skip = FALSE,
           id = rand_id("sequence_onehot")
  ) {
    
    if (length(padding) != 1 || !(padding %in% c("pre", "post")))
      rlang::abort("`padding` should be one of: 'pre', 'post'")
    
    if (length(truncating) != 1 || !(truncating %in% c("pre", "post")))
      rlang::abort("`truncating` should be one of: 'pre', 'post'")
    
    add_step(
      recipe,
      step_sequence_onehot_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        string_length = string_length,
        padding = padding,
        truncating = truncating,
        vocabulary = vocabulary,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

step_sequence_onehot_new <-
  function(terms, role, trained, columns, string_length, padding, truncating,
           vocabulary, prefix, skip, id) {
    step(
      subclass = "sequence_onehot",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      string_length = string_length,
      padding = padding,
      truncating = truncating,
      vocabulary = vocabulary,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_sequence_onehot <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  check_list(training[, col_names])

  token_list <- list()
  
  for (i in seq_along(col_names)) {
    token_list[[i]] <- x$vocabulary %||%
      sort(get_unique_tokens(training[, col_names[i], drop = TRUE]))
  }

  step_sequence_onehot_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    string_length = x$string_length,
    padding = x$padding,
    truncating = x$truncating,
    vocabulary = token_list,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_sequence_onehot <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (i in seq_along(col_names)) {
    out_text <- string2encoded_matrix(new_data[, col_names[i], drop = TRUE],
                                      vocabulary = object$vocabulary[[i]],
                                      string_length = object$string_length,
                                      padding = object$padding,
                                      truncating = object$truncating)

    colnames(out_text) <- paste(sep = "_",
                                object$prefix,
                                col_names[i],
                                seq_len(ncol(out_text)))

    new_data <-
      new_data[, !(colnames(new_data) %in% col_names[i]), drop = FALSE]

    new_data <- vctrs::vec_cbind(new_data, as_tibble(out_text))
  }
  as_tibble(new_data)
}

#' @export
print.step_sequence_onehot <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Sequence 1 hot encoding for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
}

#' @rdname step_sequence_onehot
#' @param x A `step_sequence_onehot` object.
#' @export
tidy.step_sequence_onehot <- function(x, ...) {
  if (is_trained(x)) {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = rep(term_names, each = lengths(x$vocabulary)),
                  vocabulary = unlist(lapply(x$vocabulary, seq_along)),
                  token = unlist(x$vocabulary))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  vocabulary = NA_character_,
                  token = NA_integer_)
  }
  res$id <- x$id
  res
}

# Implementation
pad_string <- function(x, n, padding, truncating) {
  
  len_x <- length(x)
  
  if (len_x == n) {
    return(x)
  }
  
  if (len_x < n) {
    if (padding == "post") {
      return(c(x, rep(NA, (n - len_x))))
    } else {
      return(c(rep(NA, (n - len_x)), x))
    }
  } else {
    if (truncating == "post") {
      return(x[seq_len(n)])
    } else {
      return(x[seq(len_x - n + 1, len_x)])
    }
  }

}

char_key <- function(x) {
  out <- seq_along(x)
  names(out) <- x
  out
}

string2encoded_matrix <- function(x, vocabulary, string_length, padding,
                                  truncating) {
  vocabulary <- char_key(vocabulary)
  x <- get_tokens(x)
  #x <- map(x, ~.x[seq_len(min(length(.x), string_length))])
  x <- lapply(x, pad_string, 
              n = string_length, 
              padding = padding, 
              truncating = truncating)
  x <- lapply(x, function(x) vocabulary[x])
  df <- do.call(rbind, x)
  df <- unname(df)
  df[is.na(df)] <- 0
  df
}
