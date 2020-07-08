#' text_normalizationming of [tokenlist] variables
#'
#' `step_text_normalization` creates a *specification* of a recipe step that
#'  will perform Unicode Normalization
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables will be transformed. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param normalization_form A single character string determining the Unicode 
#'  Normalization. Must be one of "nfc", "nfd", "nfkd", "nfkc", or 
#'  "nfkc_casefold". Defaults to "nfc". 
#'  See [stringi::stri_trans_nfc()] for more details.
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
#' if (requireNamespace("stringi", quietly = TRUE)) {
#' library(recipes)
#' 
#' sample_data <- tibble(text = c("sch\U00f6n", "scho\U0308n"))
#' 
#' okc_rec <- recipe(~ ., data = sample_data) %>%
#'   step_text_normalization(text)
#'   
#' okc_obj <- okc_rec %>%
#'   prep()
#' 
#' juice(okc_obj, text) %>% 
#'   slice(1:2)
#' 
#' juice(okc_obj) %>% 
#'   slice(2) %>% 
#'   pull(text) 
#'   
#' tidy(okc_rec, number = 1)
#' tidy(okc_obj, number = 1)
#' }
#' @export
#' 
#' @seealso [step_texthash()] for feature hashing.
#' @family character to character steps
step_text_normalization <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           normalization_form = "nfc",
           skip = FALSE,
           id = rand_id("text_normalization")
  ) {
    
    recipes::recipes_pkg_check("stringi")
    
    add_step(
      recipe,
      step_text_normalization_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        normalization_form = normalization_form,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_text_normalization_new <-
  function(terms, role, trained, columns, normalization_form, skip, id) {
    step(
      subclass = "text_normalization",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      normalization_form = normalization_form,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_text_normalization <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  training <- factor_to_text(training, col_names)
  
  check_type(training[, col_names], quant = FALSE)
  
  step_text_normalization_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    normalization_form = x$normalization_form,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_text_normalization <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat
  
  new_data <- factor_to_text(new_data, col_names)
  
  normalization_fun <- switch(
    object$normalization_form,
    nfc = stringi::stri_trans_nfc,
    nfd = stringi::stri_trans_nfd,
    nfkd = stringi::stri_trans_nfkd,
    nfkc = stringi::stri_trans_nfkc,
    nfkc_casefold = stringi::stri_trans_nfkc_casefold,
    rlang::abort(paste("'normalization_form' must be one of",
                       "'nfc', 'nfd', 'nfkd', 'nfkc', or 'nfkc_casefold'",
                       "but was ", object$normalization_form))
    )

  for (i in seq_along(col_names)) {
    new_data[, col_names[i]] <- normalization_fun(
        getElement(new_data, col_names[i])
      )
  }
  new_data <- factor_to_text(new_data, col_names)
  as_tibble(new_data)
}

#' @export
print.step_text_normalization <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("text_normalizationming for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_text_normalization
#' @param x A `step_text_normalization` object.
#' @export
tidy.step_text_normalization <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$terms,
                  normalization_form = x$normalization_form)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_chr)
  }
  res$id <- x$id
  res
}
