#' Normalization of Character Variables
#'
#' `step_text_normalization()` creates a *specification* of a recipe step that
#' will perform Unicode Normalization on character variables.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param normalization_form A single character string determining the Unicode
#'   Normalization. Must be one of "nfc", "nfd", "nfkd", "nfkc", or
#'   "nfkc_casefold". Defaults to "nfc". See [stringi::stri_trans_nfc()] for
#'   more details.
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `normalization_form`, and `id`:
#' 
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{normalization_form}{character, type of normalization}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @seealso [step_texthash()] for feature hashing.
#' @family Steps for Text Normalization
#'
#' @examplesIf rlang::is_installed("stringi")
#' library(recipes)
#'
#' sample_data <- tibble(text = c("sch\U00f6n", "scho\U0308n"))
#'
#' rec <- recipe(~., data = sample_data) %>%
#'   step_text_normalization(text)
#'
#' prepped <- rec %>%
#'   prep()
#'
#' bake(prepped, new_data = NULL, text) %>%
#'   slice(1:2)
#'
#' bake(prepped, new_data = NULL) %>%
#'   slice(2) %>%
#'   pull(text)
#'
#' tidy(rec, number = 1)
#' tidy(prepped, number = 1)
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
        terms = enquos(...),
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
  col_names <- recipes_eval_select(x$terms, training, info)

  rlang::arg_match0(
    x$normalization_form, 
    c("nfc", "nfd", "nfkd", "nfkc", "nfkc_casefold"),
    arg_nm = "normalization_form"
  )

  training <- factor_to_text(training, col_names)

  check_type(training[, col_names], types = c("string", "factor", "ordered"))

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
  check_new_data(col_names, object, new_data)

  new_data <- factor_to_text(new_data, col_names)

  normalization_fun <- switch(object$normalization_form,
    nfc = stringi::stri_trans_nfc,
    nfd = stringi::stri_trans_nfd,
    nfkd = stringi::stri_trans_nfkd,
    nfkc = stringi::stri_trans_nfkc,
    nfkc_casefold = stringi::stri_trans_nfkc_casefold,
    cli::cli_abort(
      "{.arg normalization_form} must be one of {.val nfc}, {.val nfd}, 
      {.val nfkd}, {.val nfkc}, or {.val nfkc_casefold} but was 
      {.val {object$normalization_form}}."
    )
  )
  
  for (col_name in col_names) {
    new_data[[col_name]] <- normalization_fun(new_data[[col_name]])
    new_data[[col_name]] <- factor(new_data[[col_name]])
  }
  
  new_data
}

#' @export
print.step_text_normalization <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Text Normalization for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_text_normalization
#' @usage NULL
#' @export
tidy.step_text_normalization <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = unname(x$columns),
      normalization_form = x$normalization_form
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      normalization_form = na_chr
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
