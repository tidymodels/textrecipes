#' Untokenization of list-column variables
#'
#' `step_untokenize` creates a *specification* of a recipe step that
#'  will convert a list of its tokenized parts into a character predictor.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_untokenize`, this indicates the variables to be encoded
#'  into a list column. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
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
#'   step_untokenize(essay0) 
#'   
#' okc_obj <- okc_rec %>%
#'   prep(training = okc_text, retain = TRUE)
#' 
#' juice(okc_obj, essay0) %>% 
#'   slice(1:2)
#' 
#' juice(okc_obj) %>% 
#'   slice(2) %>% 
#'   pull(essay0) 
#'   
#' tidy(okc_rec, number = 2)
#' tidy(okc_obj, number = 2)
#' @keywords datagen 
#' @concept preprocessing encoding
#' @export
#' @importFrom recipes add_step step terms_select sel2char ellipse_check 
#' @importFrom recipes check_type
step_untokenize <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           skip = FALSE
  ) {
    add_step(
      recipe,
      step_untokenize_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        skip = skip
      )
    )
  }

step_untokenize_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           columns = NULL,
           skip = FALSE) {
    step(
      subclass = "untokenize",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip
    )
  }

#' @export
prep.step_untokenize <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  check_list(training[, col_names])
  
  step_untokenize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip
  )
}

#' @export
#' @importFrom tibble as_tibble
#' @importFrom recipes bake prep
#' @importFrom purrr map_chr
bake.step_untokenize <- function(object, newdata, ...) {
  col_names <- object$columns
  # for backward compat
  
  for (i in seq_along(col_names)) {
    newdata[, col_names[i]] <- map_chr(newdata[, col_names[i], drop = TRUE], 
                                       paste, collapse = " ")
  }
  
  newdata <- factor_to_text(newdata, col_names)
  
  as_tibble(newdata)
}

#' @importFrom recipes printer
#' @export
print.step_untokenize <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Untokenization for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_untokenize
#' @param x A `step_untokenize` object.
#' @importFrom rlang na_chr na_int
#' @export
tidy.step_untokenize <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$terms,
                  value = "untokenize")
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_chr)
  }
  res
}