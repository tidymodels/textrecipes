#' Calculate Set of Text Features
#'
#' `step_textfeature` creates a *specification* of a recipe step that will
#' extract a number of numeric features of a text column.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param extract_functions A named list of feature extracting functions.
#'   default to \code{\link[textfeatures]{count_functions}} from the
#'   textfeatures package. See details for more information.
#' @param prefix A prefix for generated column names, default to "textfeature".
#' @template args-keep_original_cols
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' This step will take a character column and returns a number of numeric
#' columns equal to the number of functions in the list passed to the
#' `extract_functions` argument. The default is a list of functions from the
#' textfeatures package.
#'
#' All the functions passed to `extract_functions` must take a character vector
#' as input and return a numeric vector of the same length, otherwise an error
#' will be thrown.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the selectors or variables selected) and `functions` (name of feature
#' functions).
#'
#'@template case-weights-not-supported
#'
#' @family Steps for Numeric Variables From Characters
#'   
#' @examplesIf rlang::is_installed("textfeatures")
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~., data = tate_text) %>%
#'   step_textfeature(medium)
#'
#' tate_obj <- tate_rec %>%
#'   prep()
#'
#' bake(tate_obj, new_data = NULL) %>%
#'   slice(1:2)
#'
#' bake(tate_obj, new_data = NULL) %>%
#'   pull(textfeature_medium_n_words)
#'
#' tidy(tate_rec, number = 1)
#' tidy(tate_obj, number = 1)
#'
#' # Using custom extraction functions
#' nchar_round_10 <- function(x) round(nchar(x) / 10) * 10
#'
#' recipe(~., data = tate_text) %>%
#'   step_textfeature(medium,
#'     extract_functions = list(nchar10 = nchar_round_10)
#'   ) %>%
#'   prep() %>%
#'   bake(new_data = NULL)
#' @export
step_textfeature <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           columns = NULL,
           extract_functions = textfeatures::count_functions,
           prefix = "textfeature",
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("textfeature")) {
    recipes::recipes_pkg_check(required_pkgs.step_textfeature())

    add_step(
      recipe,
      step_textfeature_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        extract_functions = extract_functions,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_textfeature_new <-
  function(terms, role, trained, columns, extract_functions, prefix,
           keep_original_cols, skip, id) {
    step(
      subclass = "textfeature",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      extract_functions = extract_functions,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_textfeature <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  training <- factor_to_text(training, col_names)

  check_type(training[, col_names], quant = FALSE)

  purrr::walk(x$extract_functions, validate_string2num)

  step_textfeature_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    extract_functions = x$extract_functions,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_textfeature <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  new_data <- factor_to_text(new_data, col_names)

  for (i in seq_along(col_names)) {
    tf_text <- map_dfc(
      object$extract_functions,
      ~ .x(new_data[, col_names[i], drop = TRUE])
    )

    colnames(tf_text) <- paste(object$prefix, col_names[i], colnames(tf_text),
      sep = "_"
    )

    new_data <- vctrs::vec_cbind(new_data, tf_text)

    keep_original_cols <- get_keep_original_cols(object)
    if (!keep_original_cols) {
      new_data <- 
        new_data[, !(colnames(new_data) %in% col_names[i]), drop = FALSE]
    }
  }
  new_data
}

#' @export
print.step_textfeature <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Text feature extraction for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_textfeature` object.
#' @export
tidy.step_textfeature <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$columns) == 0) {
      res <- tibble(
        terms = character(),
        functions = character()
      )
    } else {
      res <- tibble(
        terms = rep(unname(x$columns), each = length(x$extract_functions)),
        functions = rep(names(x$extract_functions), length(x$terms))
      )
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      functions = NA_character_
    )
  }
  res$id <- x$id
  res
}

validate_string2num <- function(fun) {
  string <- c("This is a test string", "with", "3 elements")

  out <- fun(string)
  if (!(is.numeric(out) | is.logical(out))) {
    rlang::abort(paste0(deparse(substitute(fun)), " must return a numeric."))
  }

  if (length(string) != length(out)) {
    rlang::abort(paste0(
      deparse(substitute(fun)),
      " must return the same length output as its input."
    ))
  }
}


#' @rdname required_pkgs.step
#' @export
required_pkgs.step_textfeature <- function(x, ...) {
  c("textfeatures", "textrecipes")
}
