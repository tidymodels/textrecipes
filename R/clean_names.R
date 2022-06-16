#' Clean Variable Names
#'
#' `step_clean_names` creates a *specification* of a recipe step that will clean
#' variable names so the names consist only of letters, numbers, and the
#' underscore.
#
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @param clean A named character vector to clean variable names. This is `NULL`
#'   until computed by [recipes::prep.recipe()].
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#' 
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the new clean variable names) and `value` (the original variable names).
#' 
#' @template case-weights-not-supported
#' 
#' @seealso [step_clean_levels()], [recipes::step_factor2string()],
#'  [recipes::step_string2factor()], [recipes::step_regex()],
#'  [recipes::step_unknown()], [recipes::step_novel()], [recipes::step_other()]
#' @family Steps for Text Cleaning
#'
#' @examples
#' library(recipes)
#' data(airquality)
#'
#' air_tr <- tibble(airquality[1:100, ])
#' air_te <- tibble(airquality[101:153, ])
#'
#' rec <- recipe(~., data = air_tr)
#'
#' if (requireNamespace("janitor", quietly = TRUE)) {
#'   rec <- rec %>%
#'     step_clean_names(all_predictors())
#'   rec <- prep(rec, training = air_tr)
#'   tidy(rec, number = 1)
#'
#'   bake(rec, air_tr)
#'   bake(rec, air_te)
#' }
#' @export
step_clean_names <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           clean = NULL,
           skip = FALSE,
           id = rand_id("clean_names")) {
    add_step(
      recipe,
      step_clean_names_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        clean = clean,
        skip = skip,
        id = id
      )
    )
  }

step_clean_names_new <-
  function(terms, role, trained, clean, skip, id) {
    step(
      subclass = "clean_names",
      terms = terms,
      role = role,
      trained = trained,
      clean = clean,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_clean_names <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  if (length(col_names) > 0) {
    cleaned <- janitor::make_clean_names(col_names)
    clean <- rlang::set_names(cleaned, col_names)
  } else {
    clean <- NULL
  }

  step_clean_names_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    clean = clean,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_clean_names <- function(object, new_data, ...) {
  if (!is.null(object$clean)) {
    colnames(new_data) <- dplyr::recode(colnames(new_data), !!!object$clean)
  }

  new_data
}


#' @export
print.step_clean_names <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Cleaning variable names for "
    print_step(names(x$clean), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_clean_names` object.
#' @export
tidy.step_clean_names <- function(x, ...) {
  if (is_trained(x)) {
    if (is.null(x$clean)) {
      res <- tibble(terms = character())
    } else {
      res <- tibble::tibble(terms = unname(x$clean), value = names(x$clean))
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}
