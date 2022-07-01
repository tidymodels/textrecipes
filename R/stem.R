#' Stemming of Token Variables
#'
#' `step_stem` creates a *specification* of a recipe step that will convert a
#' [`token`][tokenlist()] variable to have its stemmed version.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param options A list of options passed to the stemmer function.
#' @param custom_stemmer A custom stemming function. If none is provided it will
#'   default to "SnowballC".
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' Words tend to have different forms depending on context, such as organize,
#' organizes, and organizing. In many situations it is beneficial to have these
#' words condensed into one to allow for a smaller pool of words. Stemming is
#' the act of chopping off the end of words using a set of heuristics.
#'
#' Note that the stemming will only be done at the end of the word and will
#' therefore not work reliably on ngrams or sentences.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the selectors or variables selected) and `is_custom_stemmer` (indicate if
#' custom stemmer was used).
#' 
#' @template case-weights-not-supported
#'
#' @seealso [step_tokenize()] to turn characters into [`tokens`][tokenlist()]
#' @family Steps for Token Modification
#'   
#' @examples
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~., data = tate_text) %>%
#'   step_tokenize(medium) %>%
#'   step_stem(medium)
#'
#' tate_obj <- tate_rec %>%
#'   prep()
#'
#' bake(tate_obj, new_data = NULL, medium) %>%
#'   slice(1:2)
#'
#' bake(tate_obj, new_data = NULL) %>%
#'   slice(2) %>%
#'   pull(medium)
#'
#' tidy(tate_rec, number = 2)
#' tidy(tate_obj, number = 2)
#'
#' # Using custom stemmer. Here a custom stemmer that removes the last letter
#' # if it is a "s".
#' remove_s <- function(x) gsub("s$", "", x)
#'
#' tate_rec <- recipe(~., data = tate_text) %>%
#'   step_tokenize(medium) %>%
#'   step_stem(medium, custom_stemmer = remove_s)
#'
#' tate_obj <- tate_rec %>%
#'   prep()
#'
#' bake(tate_obj, new_data = NULL, medium) %>%
#'   slice(1:2)
#'
#' bake(tate_obj, new_data = NULL) %>%
#'   slice(2) %>%
#'   pull(medium)
#' @export
step_stem <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           options = list(),
           custom_stemmer = NULL,
           skip = FALSE,
           id = rand_id("stem")) {
    add_step(
      recipe,
      step_stem_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        options = options,
        custom_stemmer = custom_stemmer,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_stem_new <-
  function(terms, role, trained, columns, options, custom_stemmer, skip, id) {
    step(
      subclass = "stem",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      options = options,
      custom_stemmer = custom_stemmer,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_stem <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_list(training[, col_names])

  step_stem_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    options = x$options,
    custom_stemmer = x$custom_stemmer,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_stem <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  stem_fun <- object$custom_stemmer %||%
    SnowballC::wordStem

  for (i in seq_along(col_names)) {
    stemmed_tokenlist <- tokenlist_apply(
      new_data[, col_names[i], drop = TRUE],
      stem_fun, object$options
    )

    new_data[, col_names[i]] <- tibble(stemmed_tokenlist)
  }
  new_data <- factor_to_text(new_data, col_names)
  new_data
}

#' @export
print.step_stem <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Stemming for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_stem` object.
#' @export
tidy.step_stem <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = unname(x$columns),
      is_custom_stemmer = !is.null(x$custom_stemmer)
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      is_custom_stemmer = !is.null(x$custom_stemmer)
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_stem <- function(x, ...) {
  c("textrecipes", "SnowballC")
}
