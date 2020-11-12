#' Clean variable names
#'
#' `step_clean_names` creates a *specification* of a recipe step that will
#'  clean variable names so the names consist only of letters, numbers, and the
#'  underscore.
#'
#' @inheritParams step_untokenize
#' @param ... One or more selector functions to choose which
#'  variables' names will be cleaned. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param clean A named character vector to clean variable names. This is `NULL`
#'  until computed by [recipes::prep.recipe()].
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the `tidy` method, a
#'  tibble with columns `terms` (the new clean variable names) and `value`
#'  (the original variable names).
#' @export
#'
#' @seealso [step_clean_levels()], [recipes::step_factor2string()],
#'  [recipes::step_string2factor()], [recipes::step_regex()],
#'  [recipes::step_unknown()], [recipes::step_novel()], [recipes::step_other()]
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
        terms = ellipse_check(...),
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
  col_names <- terms_select(x$terms, info = info)

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

  as_tibble(new_data)
}


#' @export
print.step_clean_names <-
  function(x, width = max(20, options()$width - 30), ...) {
    if (x$trained) {
      cleaned <- names(x$clean)
      if (length(cleaned) > 0) {
        cat("Cleaning variable names for ", sep = "")
        printer(cleaned, x$terms, x$trained, width = width)
      } else {
        cat("No variable names were cleaned\n")
      }
    } else {
      cat("Cleaning variable names for ", sep = "")
      printer(names(x$objects), x$terms, x$trained, width = width)
    }
    invisible(x)
  }

#' @rdname step_clean_names
#' @param x A `step_clean_names` object.
#' @export
tidy.step_clean_names <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble::tibble(terms = unname(x$clean), value = names(x$clean))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}
