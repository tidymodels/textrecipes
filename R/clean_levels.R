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
#' The levels are cleaned with [janitor::make_clean_names()], which is the
#' function responsible for the cleaning, and then reset with
#' [dplyr::recode_factor()]. When data to be processed contains novel levels
#' (i.e., not contained in the training set), they are converted to missing.
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
#' rec <- rec |>
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
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    clean = NULL,
    skip = FALSE,
    id = rand_id("clean_levels")
  ) {
    add_step(
      recipe,
      step_clean_levels_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        clean = clean,
        columns = NULL,
        skip = skip,
        id = id
      )
    )
  }

step_clean_levels_new <-
  function(terms, role, trained, clean, columns, skip, id) {
    step(
      subclass = "clean_levels",
      terms = terms,
      role = role,
      trained = trained,
      clean = clean,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_clean_levels <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names], types = c("string", "factor", "ordered"))

  if (length(col_names) > 0) {
    orig <- purrr::map(col_names, function(col_name) {
      col <- training[[col_name]]
      if (is.factor(col)) {
        levels(col)
      } else {
        unique(as.character(col))
      }
    })
    names(orig) <- col_names
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
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_clean_levels <- function(object, new_data, ...) {
  # `columns` is the authoritative source of the trained column names. Older
  # trained objects (created before the `columns` field was added) don't have
  # it, so fall back to `names(object$clean)` for those.
  col_names <- object$columns
  if (is.null(col_names)) {
    col_names <- names(object$clean)
  }
  check_new_data(col_names, object, new_data)

  clean <- object$clean
  if (!is.null(clean) && is.null(names(clean))) {
    # Backwards compatibility with 1.0.3 (#230)
    names(clean) <- col_names
  }

  recipes_map_cols(new_data, col_names, function(x, i, col_name) {
    dict <- clean[[col_name]]
    cleaned_values <- unname(dict[as.character(x)])

    if (is.factor(x)) {
      factor(cleaned_values, levels = unique(unname(dict)))
    } else {
      cleaned_values
    }
  })
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
