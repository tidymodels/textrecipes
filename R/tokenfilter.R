#' Filter the tokens based on term frequency
#'
#' `step_tokenfilter` creates a *specification* of a recipe step that
#'  will convert a [tokenlist] to be filtered based on frequency.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_tokenfilter`, this indicates the variables to be encoded
#'  into a [tokenlist]. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param max_times An integer. Maximal number of times a word can appear
#'  before getting removed.
#' @param min_times An integer. Minimum number of times a word can appear
#'  before getting removed.
#' @param percentage A logical. Should max_times and min_times be interpreded
#'  as a percentage instead of count.
#' @param max_tokens An integer. Will only keep the top max_tokens tokens
#'  after filtering done by max_times and min_times. Defaults to 100.
#' @param res The words that will be keep will be stored here once
#'  this preprocessing step has be trained by [prep.recipe()].
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake.recipe()]? While all
#'  operations are baked when [recipes::prep.recipe()] is run, some
#'  operations may not be able to be conducted on new data (e.g.
#'  processing the outcome variable(s)). Care should be taken when
#'  using `skip = TRUE` as it may affect the computations for
#'  subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#' @param trained A logical to indicate if the recipe has been
#'  baked.
#' @return An updated version of `recipe` with the new step added
#'  to the sequence of existing steps (if any).
#' @examples
#' library(recipes)
#' library(modeldata)
#' data(okc_text)
#'
#' okc_rec <- recipe(~., data = okc_text) %>%
#'   step_tokenize(essay0) %>%
#'   step_tokenfilter(essay0)
#'
#' okc_obj <- okc_rec %>%
#'   prep()
#'
#' bake(okc_obj, new_data = NULL, essay0) %>%
#'   slice(1:2)
#'
#' bake(okc_obj, new_data = NULL) %>%
#'   slice(2) %>%
#'   pull(essay0)
#'
#' tidy(okc_rec, number = 2)
#' tidy(okc_obj, number = 2)
#' @export
#' @details
#' This step allow you to limit the tokens you are looking at by filtering
#' on their occurrence in the corpus. You are able to exclude tokens if they
#' appear too many times or too fews times in the data. It can be specified
#' as counts using `max_times` and `min_times` or as percentages by setting
#' `percentage` as `TRUE`. In addition one can filter to only use the top
#' `max_tokens` used tokens. If `max_tokens` is set to `Inf` then all the tokens
#' will be used. This will generally lead to very large datasets when then
#' tokens are words or trigrams. A good strategy is to start with a low token
#' count and go up according to how much RAM you want to use.
#'
#' It is strongly advised to filter before using [step_tf] or [step_tfidf] to
#' limit the number of variables created.
#'
#' @seealso [step_tokenize()] to turn character into tokenlist.
#' @family tokenlist to tokenlist steps
step_tokenfilter <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           max_times = Inf,
           min_times = 0,
           percentage = FALSE,
           max_tokens = 100,
           res = NULL,
           skip = FALSE,
           id = rand_id("tokenfilter")) {
    if (percentage && (max_times > 1 | max_times < 0 |
      min_times > 1 | min_times < 0)) {
      rlang::abort(
        "`max_times` and `min_times` should be in the interval [0, 1]."
      )
    }

    add_step(
      recipe,
      step_tokenfilter_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        max_times = max_times,
        min_times = min_times,
        percentage = percentage,
        max_tokens = max_tokens,
        res = res,
        skip = skip,
        id = id
      )
    )
  }

step_tokenfilter_new <-
  function(terms, role, trained, columns, max_times, min_times, percentage,
           max_tokens, res, skip, id) {
    step(
      subclass = "tokenfilter",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      max_times = max_times,
      min_times = min_times,
      percentage = percentage,
      max_tokens = max_tokens,
      res = res,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_tokenfilter <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  check_list(training[, col_names])

  retain_words <- list()
  n_words <- list()

  for (i in seq_along(col_names)) {
    retain_words[[i]] <- tokenfilter_fun(
      training[, col_names[i], drop = TRUE],
      x$max_times, x$min_times, x$max_tokens,
      x$percentage
    )

    n_words[[i]] <- length(table(unlist(training[, col_names[i], drop = TRUE])))
  }

  step_tokenfilter_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    max_times = x$max_times,
    min_times = x$min_times,
    percentage = x$percentage,
    max_tokens = n_words,
    res = retain_words,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_tokenfilter <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (i in seq_along(col_names)) {
    filtered_text <- tokenlist_filter(
      new_data[, col_names[i], drop = TRUE],
      object$res[[i]],
      TRUE
    )

    new_data[, col_names[i]] <- tibble(filtered_text)
  }
  new_data <- factor_to_text(new_data, col_names)

  as_tibble(new_data)
}

#' @export
print.step_tokenfilter <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Text filtering for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_tokenfilter
#' @param x A `step_tokenfilter` object.
#' @export
tidy.step_tokenfilter <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = x$terms,
      value = x$max_tokens
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_int
    )
  }
  res$id <- x$id
  res
}

## Implementation
tokenfilter_fun <- function(data, max_times, min_times, max_features,
                            percentage) {
  tf <- table(unlist(get_tokens(data)))

  if (percentage) {
    tf <- tf / sum(tf)
  }

  ids <- tf <= max_times & tf >= min_times

  if (is.infinite(max_features)) {
    names(sort(tf[ids], decreasing = TRUE))
  } else {
    if (max_features > sum(ids)) {
      rlang::warn(paste0(
        "max_features was set to '", max_features,
        "', but only ", sum(ids),
        " was available and selected."
      ))
      max_features <- sum(ids)
    }
    names(sort(tf[ids], decreasing = TRUE)[seq_len(max_features)])
  }
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_tokenfilter <- function(x, ...) {
  c("textrecipes")
}

#' @rdname tunable.step
#' @export
tunable.step_tokenfilter <- function(x, ...) {
  tibble::tibble(
    name = c("max_times", "min_times", "max_tokens"),
    call_info = list(
      list(pkg = "dials", fun = "max_times"),
      list(pkg = "dials", fun = "min_times"),
      list(pkg = "dials", fun = "max_tokens")
    ),
    source = "recipe",
    component = "step_tokenfilter",
    component_id = x$id
  )
}
