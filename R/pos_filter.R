#' Part of speech filtering of [tokenlist] variables
#'
#' `step_pos_filter` creates a *specification* of a recipe step that
#'  will filter a [tokenlist] based on part of speech tags.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param keep_tags Character variable of part of speech tags to keep. See
#' details for complete list of tags. Defaults to "NOUN".
#' @template args-skip
#' @template args-id
#' 
#' @template returns
#' 
#' @details
#' Possible part of speech tags for `spacyr` engine are: "ADJ", "ADP", "ADV",
#' "AUX", "CONJ", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON",
#' "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X" and "SPACE". For more
#' information look here \url{https://spacy.io/api/annotation#pos-tagging}.
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
#'   step_pos_filter(text, keep_tags = "NOUN") %>%
#'   step_tf(text)
#'
#' okc_obj <- prep(okc_rec)
#'
#' bake(okc_obj, new_data = NULL)
#' }
#' 
#' @export
step_pos_filter <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           keep_tags = "NOUN",
           skip = FALSE,
           id = rand_id("pos_filter")) {
    add_step(
      recipe,
      step_pos_filter_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        keep_tags = keep_tags,
        skip = skip,
        id = id
      )
    )
  }

step_pos_filter_new <-
  function(terms, role, trained, columns, keep_tags, skip, id) {
    step(
      subclass = "pos_filter",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      keep_tags = keep_tags,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_pos_filter <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  check_list(training[, col_names])

  step_pos_filter_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    keep_tags = x$keep_tags,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_pos_filter <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (i in seq_along(col_names)) {
    variable <- new_data[, col_names[i], drop = TRUE]

    if (is.null(maybe_get_pos(variable))) {
      rlang::abort(paste0(
        "`", col_names[i],
        "` doesn't have a pos attribute. ",
        "Make sure the tokenization step includes ",
        "part of speech tagging."
      ))
    } else {
      pos_filter_variable <- tokenlist_pos_filter(variable, object$keep_tags)
    }

    new_data[, col_names[i]] <- tibble(pos_filter_variable)
  }
  new_data <- factor_to_text(new_data, col_names)
  as_tibble(new_data)
}

#' @export
print.step_pos_filter <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Part of speech filtering for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_pos_filter
#' @param x A `step_pos_filter` object.
#' @export
tidy.step_pos_filter <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$terms)
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
required_pkgs.step_pos_filter <- function(x, ...) {
  c("textrecipes")
}
