#' Indicator Variables via Feature Hashing
#'
#' `step_dummy_hash` creates a *specification* of a recipe step that will
#' convert factors or character columns into a series of binary (or signed
#' binary) indicator columns.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param signed A logical, indicating whether to use a signed hash-function
#'   (generating values of -1, 0, or 1), to reduce collisions when hashing.
#'   Defaults to TRUE.
#' @param num_terms An integer, the number of variables to output. Defaults to
#'   32.
#' @param collapse A logical; should all of the selected columns be collapsed
#'   into a single column to create a single set of hashed features?
#' @template args-prefix
#' @template args-keep_original_cols
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' Feature hashing, or the hashing trick, is a transformation of a text variable
#' into a new set of numerical variables. This is done by applying a hashing
#' function over the values of the factor levels and using the hash values as
#' feature indices. This allows for a low memory representation of the data and
#' can be very helpful when a qualitative predictor has many levels or is
#' expected to have new levels during prediction. This implementation is done
#' using the MurmurHash3 method.
#'
#' The argument `num_terms` controls the number of indices that the hashing
#' function will map to. This is the tuning parameter for this transformation.
#' Since the hashing function can map two different tokens to the same index,
#' will a higher value of `num_terms` result in a lower chance of collision.
#'
#' @template details-prefix
#'
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the selectors or variables selected), `value` (whether a signed hashing was
#' performed), `num_terms` (number of terms), and `collapse` (where columns
#' collapsed).
#' 
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_dummy_hash"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @references Kilian Weinberger; Anirban Dasgupta; John Langford; Alex Smola;
#'   Josh Attenberg (2009).
#'
#'   Kuhn and Johnson (2019), Chapter 7,
#'   \url{https://bookdown.org/max/FES/encoding-predictors-with-many-categories.html}
#'
#'
#'
#'
#' @seealso [recipes::step_dummy()]
#' @family Steps for Numeric Variables From Characters
#'
#' @examplesIf rlang::is_installed("text2vec")
#' library(recipes)
#' library(modeldata)
#' data(grants)
#'
#' grants_rec <- recipe(~sponsor_code, data = grants_other) %>%
#'   step_dummy_hash(sponsor_code)
#'
#' grants_obj <- grants_rec %>%
#'   prep()
#'
#' bake(grants_obj, grants_test)
#'
#' tidy(grants_rec, number = 1)
#' tidy(grants_obj, number = 1)
#' @export
step_dummy_hash <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           columns = NULL,
           signed = TRUE,
           num_terms = 32L,
           collapse = FALSE,
           prefix = "dummyhash",
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("dummy_hash")) {
    recipes::recipes_pkg_check(required_pkgs.step_dummy_hash())

    add_step(
      recipe,
      step_dummy_hash_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        signed = signed,
        num_terms = num_terms,
        collapse = collapse,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_dummy_hash_new <-
  function(terms, role, trained, columns, signed, collapse, num_terms, prefix,
           keep_original_cols, skip, id) {
    step(
      subclass = "dummy_hash",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      signed = signed,
      num_terms = num_terms,
      collapse = collapse,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_dummy_hash <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names], types = c("string", "factor", "ordered"))

  step_dummy_hash_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    signed = x$signed,
    num_terms = x$num_terms,
    collapse = x$collapse,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_dummy_hash <- function(object, new_data, ...) {
  if (length(object$columns) == 0L) {
    # Empty selection
    return(new_data)
  }

  col_names <- object$columns
  hash_cols <- col_names
  hash_cols <- unname(hash_cols)

  check_new_data(col_names, object, new_data)

  if (object$collapse) {
    new_name <- paste0(col_names, collapse = "_")
    new_data <-
      new_data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        !!new_name :=
          paste0(dplyr::c_across(dplyr::all_of(hash_cols)), collapse = "")
      )
    hash_cols <- new_name
  }

  for (i in seq_along(hash_cols)) {
    tf_text <-
      hashing_function(
        as.character(new_data[[hash_cols[i]]]),
        paste0(
          object$prefix, "_",
          hash_cols[i], "_",
          names0(object$num_terms, "")
        ),
        object$signed,
        object$num_terms
      )

    tf_text <- purrr::map_dfc(tf_text, as.integer)
    keep_original_cols <- get_keep_original_cols(object)
    if (!keep_original_cols) {
      new_data <-
        new_data[, !(colnames(new_data) %in% hash_cols[i]), drop = FALSE]
    }

    tf_text <- check_name(tf_text, new_data, object, names(tf_text))
    
    new_data <- vctrs::vec_cbind(tf_text, new_data)
  }
  if (object$collapse) {
    new_data <- new_data[, !(colnames(new_data) %in% col_names), drop = FALSE]
  }

  new_data
}

#' @export
print.step_dummy_hash <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Feature hashing with "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_dummy_hash` object.
#' @export
tidy.step_dummy_hash <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = unname(x$columns),
      value = x$signed,
      num_terms = x$num_terms,
      collapse = x$collapse
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_lgl,
      num_terms = na_int,
      collapse = na_lgl
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @keywords internal
#' @export
required_pkgs.step_dummy_hash <- function(x, ...) {
  c("text2vec", "textrecipes")
}

#' @rdname tunable_textrecipes
#' @export
tunable.step_dummy_hash <- function(x, ...) {
  tibble::tibble(
    name = c("signed", "num_terms"),
    call_info = list(
      list(pkg = "dials", fun = "signed_hash"),
      list(pkg = "dials", fun = "num_hash", range = c(8, 12))
    ),
    source = "recipe",
    component = "step_dummy_hash",
    component_id = x$id
  )
}
