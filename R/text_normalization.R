#' Normalization of [tokenlist] variables
#'
#' `step_text_normalization` creates a *specification* of a recipe step that
#'  will perform Unicode Normalization
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param normalization_form A single character string determining the Unicode
#'  Normalization. Must be one of "nfc", "nfd", "nfkd", "nfkc", or
#'  "nfkc_casefold". Defaults to "nfc".
#'  See [stringi::stri_trans_nfc()] for more details.
#' @template args-skip
#' @template args-id
#' 
#' @template returns
#' 
#' @seealso [step_texthash()] for feature hashing.
#' @family character to character steps
#' 
#' @examples
#' if (requireNamespace("stringi", quietly = TRUE)) {
#'   library(recipes)
#'
#'   sample_data <- tibble(text = c("sch\U00f6n", "scho\U0308n"))
#'
#'   rec <- recipe(~., data = sample_data) %>%
#'     step_text_normalization(text)
#'
#'   prepped <- rec %>%
#'     prep()
#'
#'   bake(prepped, new_data = NULL, text) %>%
#'     slice(1:2)
#'
#'   bake(prepped, new_data = NULL) %>%
#'     slice(2) %>%
#'     pull(text)
#'
#'   tidy(rec, number = 1)
#'   tidy(prepped, number = 1)
#' }
#' 
#' @export
step_text_normalization <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           normalization_form = "nfc",
           skip = FALSE,
           id = rand_id("text_normalization")) {
    recipes::recipes_pkg_check(required_pkgs.step_text_normalization())

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
    rlang::abort(paste(
      "'normalization_form' must be one of",
      "'nfc', 'nfd', 'nfkd', 'nfkc', or 'nfkc_casefold'",
      "but was ", object$normalization_form
    ))
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
    res <- tibble(
      terms = x$terms,
      normalization_form = x$normalization_form
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_chr
    )
  }
  res$id <- x$id
  res
}


#' @rdname required_pkgs.step
#' @export
required_pkgs.step_text_normalization <- function(x, ...) {
  c("stringi", "textrecipes")
}
