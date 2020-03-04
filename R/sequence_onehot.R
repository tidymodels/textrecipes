#'  Generate the basic set of text features
#'
#' `step_sequence_onehot` creates a *specification* of a recipe step that
#'  will take a string and do one hot encoding for each character by position.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_sequence_onehot`, this indicates the variables to be encoded
#'  into a list column. See [recipes::selections()] for more
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
#' @param integer_key A character vector, characters to be mapped to integers. 
#'  Characters not in the integer_key will be encoded as 0. Defaults to 
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
#' 
#' data(okc_text)
#' 
#' okc_rec <- recipe(~ ., data = okc_text) %>%
#'   step_sequence_onehot(essay0) 
#'   
#' okc_obj <- okc_rec %>%
#'   prep(training = okc_text, retain = TRUE)
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
#' a integer to each character in the integer_key, and will encode accordingly. 
#' Characters not in the integer_key will be encoded as 0.
#'
#' @source \url{https://papers.nips.cc/paper/5782-character-level-convolutional-networks-for-text-classification.pdf}
step_sequence_onehot <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           columns = NULL,
           string_length = 100,
           integer_key = letters,
           prefix = "seq1hot",
           skip = FALSE,
           id = rand_id("sequence_onehot")
  ) {
    add_step(
      recipe,
      step_sequence_onehot_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        string_length = string_length,
        integer_key = integer_key,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

step_sequence_onehot_new <-
  function(terms, role, trained, columns, string_length, integer_key, prefix,
           skip, id) {
    step(
      subclass = "sequence_onehot",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      string_length = string_length,
      integer_key = integer_key,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_sequence_onehot <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  training <- factor_to_text(training, col_names)

  check_type(training[, col_names], quant = FALSE)

  encoded_key <- char_key(x$integer_key)

  step_sequence_onehot_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    string_length = x$string_length,
    integer_key = encoded_key,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_sequence_onehot <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  new_data <- factor_to_text(new_data, col_names)

  for (i in seq_along(col_names)) {
    out_text <- string2encoded_matrix(new_data[, col_names[i], drop = TRUE],
                                      integer_key = object$integer_key, string_length = object$string_length)

    colnames(out_text) <- paste(sep = "_",
                                object$prefix,
                                col_names[i],
                                seq_len(ncol(out_text)))

    new_data <- bind_cols(new_data, as_tibble(out_text))

    new_data <-
      new_data[, !(colnames(new_data) %in% col_names[i]), drop = FALSE]
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
    res <- tibble(terms = rep(term_names, each = length(x$integer_key)),
                  integer_key = rep(names(x$integer_key), length(x$terms)))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  integer_key = NA_character_)
  }
  res$id <- x$id
  res
}

pad_string <- function(x, n) {
  len_x <- length(x)
  if (len_x == n) {
    return(x)
  }
  c(x, character(n - len_x))
}

char_key <- function(x) {
  out <- seq_along(x)
  names(out) <- x
  out
}

string2encoded_matrix <- function(x, integer_key, string_length) {
  x <- stringr::str_sub(x, 1, string_length)
  x <- stringr::str_split(x, "")
  x <- lapply(x, pad_string, n = string_length)
  x <- lapply(x, function(x) integer_key[x])
  df <- do.call(rbind, x)
  df[is.na(df)] <- 0
  df
}
