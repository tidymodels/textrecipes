#' Lemmatization of [tokenlist] variables
#'
#' `step_lemma` creates a *specification* of a recipe step that
#'  will extract the lemmatization of a tokenlist.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @template args-skip
#' @template args-id
#' 
#' @template returns
#' 
#' @details
#' This stem doesn't perform lemmatization by itself, but rather lets you
#' extract the lemma attribute of the tokenlist. To be able to use `step_lemma`
#' you need to use a tokenization method that includes lemmatization. Currently
#' using the `"spacyr"` engine in [step_tokenize()] provides lemmatization and
#' works well with `step_lemma`.
#' 
#' @seealso [step_tokenize()] to turn character into tokenlist.
#' @family tokenlist to tokenlist steps
#' 
#' @examples
#' \dontrun{
#' library(recipes)
#'
#' short_data <- data.frame(text = c(
#'   "This is a short tale,",
#'   "With many cats and ladies."
#' ))
#'
#' okc_rec <- recipe(~text, data = short_data) %>%
#'   step_tokenize(text, engine = "spacyr") %>%
#'   step_lemma(text) %>%
#'   step_tf(text)
#'
#' okc_obj <- prep(okc_rec)
#'
#' bake(okc_obj, new_data = NULL)
#' }
#' 
#' @export
step_lemma <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           skip = FALSE,
           id = rand_id("lemma")) {
    add_step(
      recipe,
      step_lemma_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_lemma_new <-
  function(terms, role, trained, columns, skip, id) {
    step(
      subclass = "lemma",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_lemma <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  check_list(training[, col_names])

  step_lemma_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_lemma <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (i in seq_along(col_names)) {
    variable <- new_data[, col_names[i], drop = TRUE]

    if (is.null(maybe_get_lemma(variable))) {
      rlang::abort(paste0(
        "`", col_names[i],
        "` doesn't have a lemma attribute. ",
        "Make sure the tokenization step includes ",
        "lemmatization."
      ))
    } else {
      lemma_variable <- tokenlist_lemma(variable)
    }

    new_data[, col_names[i]] <- tibble(lemma_variable)
  }
  new_data <- factor_to_text(new_data, col_names)
  as_tibble(new_data)
}

#' @export
print.step_lemma <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Lemmatization for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_lemma
#' @param x A `step_lemma` object.
#' @export
tidy.step_lemma <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = x$terms,
      is_custom_stemmer = is.null(x$custom_stemmer)
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
required_pkgs.step_lemma <- function(x, ...) {
  c("textrecipes")
}
