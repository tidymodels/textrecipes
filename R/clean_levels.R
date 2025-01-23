#' Clean Categorical Levels
#'
#' `step_clean_levels()` creates a *specification* of a recipe step that will
#' clean nominal data (character or factor) so the levels consist only of
#' letters, numbers, and the underscore.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @param clean A named character vector to clean and recode categorical levels.
#'   This is `NULL` until computed by [recipes::prep.recipe()]. Note that if the
#'   original variable is a character vector, it will be converted to a factor.
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' The new levels are cleaned and then reset with [dplyr::recode_factor()]. When
#' data to be processed contains novel levels (i.e., not contained in the
#' training set), they are converted to missing.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `orginal`, `value`, and `id`:
#' 
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{original}{character, the original levels}
#'   \item{value}{character, the cleaned levels}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @seealso [step_clean_names()], [recipes::step_factor2string()],
#'   [recipes::step_string2factor()], [recipes::step_regex()],
#'   [recipes::step_unknown()], [recipes::step_novel()], [recipes::step_other()]
#' @family Steps for Text Cleaning
#'
#' @examplesIf rlang::is_installed(c("modeldata", "janitor"))
#' library(recipes)
#' library(modeldata)
#' data(Smithsonian)
#'
#' smith_tr <- Smithsonian[1:15, ]
#' smith_te <- Smithsonian[16:20, ]
#'
#' rec <- recipe(~., data = smith_tr)
#'
#' rec <- rec %>%
#'   step_clean_levels(name)
#' rec <- prep(rec, training = smith_tr)
#'
#' cleaned <- bake(rec, smith_tr)
#'
#' tidy(rec, number = 1)
#'
#' # novel levels are replaced with missing
#' bake(rec, smith_te)
#' @export
step_clean_levels <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           clean = NULL,
           skip = FALSE,
           id = rand_id("clean_levels")) {
    add_step(
      recipe,
      step_clean_levels_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        clean = clean,
        skip = skip,
        id = id
      )
    )
  }

step_clean_levels_new <-
  function(terms, role, trained, clean, skip, id) {
    step(
      subclass = "clean_levels",
      terms = terms,
      role = role,
      trained = trained,
      clean = clean,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_clean_levels <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names], types = c("string", "factor", "ordered"))

  if (length(col_names) > 0) {
    orig <- purrr::map(training[, col_names], levels)
    cleaned <- purrr::map(orig, janitor::make_clean_names)
    clean <- purrr::map2(cleaned, orig, rlang::set_names)
  } else {
    clean <- NULL
  }

  step_clean_levels_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    clean = clean,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_clean_levels <- function(object, new_data, ...) {
  col_names <- names(object$clean)
  check_new_data(names(object$clean), object, new_data)

  if (is.null(names(object$clean))) {
    # Backwards compatibility with 1.0.3 (#230)
    names(object$clean) <- col_names
  }
  
  for (col_name in col_names) {
    if (is.factor(new_data[[col_name]])) {
      new_data[[col_name]] <- dplyr::recode_factor(
        new_data[[col_name]], !!!object$clean[[col_name]]
      )      
    } else {
      new_data[[col_name]] <- janitor::make_clean_names(new_data[[col_name]])

    }
}

  new_data
}

#' @export
print.step_clean_levels <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Cleaning factor levels for "
    print_step(names(x$clean), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_clean_levels
#' @usage NULL
#' @export
tidy.step_clean_levels <- function(x, ...) {
  if (is_trained(x)) {
    if (is.null(x$clean)) {
      res <- tibble(terms = character())
    } else {
      res <- purrr::map_dfr(
        x$clean,
        tibble::enframe,
        name = "original",
        .id = "terms"
      )
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_clean_levels <- function(x, ...) {
  c("textrecipes", "janitor")
}
