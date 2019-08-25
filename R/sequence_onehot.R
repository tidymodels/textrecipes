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
#' @param length A numeric, number of characters to keep before discarding. 
#'  Defaults to 100.
#' @param key A character vector, characters to be mapped to integers. characters 
#'  not in the key will be encoded as 0. Defaults to `letters`.
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
#' The string will be capped by the length argument, strings shorter then length
#' will be padded with empty characters. The encoding will assign a integer to 
#' each character in the key, and will encode accordingly. Characters not in the
#' key will be encoded as 0.
#'
#' @source \url{https://papers.nips.cc/paper/5782-character-level-convolutional-networks-for-text-classification.pdf}
#' @importFrom recipes add_step step terms_select sel2char ellipse_check 
#' @importFrom recipes check_type rand_id
step_sequence_onehot <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           columns = NULL,
           length = 100,
           key = letters,
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
        length = length,
        key = key,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

step_sequence_onehot_new <-
  function(terms, role, trained, columns, length, key, prefix,
           skip, id) {
    step(
      subclass = "sequence_onehot",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      length = length,
      key = key,
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

  encoded_key <- char_key(x$key)

  step_sequence_onehot_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    length = x$length,
    key = encoded_key,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
#' @importFrom tibble as_tibble
#' @importFrom recipes bake prep
#' @importFrom purrr map_dfc
bake.step_sequence_onehot <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  new_data <- factor_to_text(new_data, col_names)

  for (i in seq_along(col_names)) {
    out_text <- string2encoded_matrix(new_data[, col_names[i], drop = TRUE],
                                      key = object$key, length = object$length)

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

#' @importFrom recipes printer
#' @export
print.step_sequence_onehot <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Sequence 1 hot encoding for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
}

#' @rdname step_sequence_onehot
#' @param x A `step_sequence_onehot` object.
#' @importFrom recipes sel2char
#' @export
tidy.step_sequence_onehot <- function(x, ...) {
  if (is_trained(x)) {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = rep(term_names, each = length(x$key)),
                  key = rep(names(x$key), length(x$terms)))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  key = NA_character_)
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

string2encoded_matrix <- function(x, key, length) {
  x <- stringr::str_sub(x, 1, length)
  x <- stringr::str_split(x, "")
  x <- lapply(x, pad_string, n = length)
  x <- lapply(x, function(x) key[x])
  df <- do.call(rbind, x)
  df[is.na(df)] <- 0
  df
}
