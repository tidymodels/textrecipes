#' Filter Tokens Based on Term Frequency
#'
#' `step_tokenfilter()` creates a *specification* of a recipe step that will
#' convert a [`token`][tokenlist()] variable to be filtered based on frequency.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param max_times An integer. Maximal number of times a word can appear before
#'   getting removed.
#' @param min_times An integer. Minimum number of times a word can appear before
#'   getting removed.
#' @param percentage A logical. Should max_times and min_times be interpreted as
#'   a percentage instead of count.
#' @param max_tokens An integer. Will only keep the top max_tokens tokens after
#'   filtering done by max_times and min_times. Defaults to 100.
#' @param filter_fun A function. This function should take a vector of
#'   characters, and return a logical vector of the same length. This function
#'   will be applied to each observation of the data set. Defaults to `NULL`.
#'   All other arguments will be ignored if this argument is used.
#' @param res The words that will be keep will be stored here once this
#'   preprocessing step has be trained by [prep.recipe()].
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' This step allow you to limit the tokens you are looking at by filtering on
#' their occurrence in the corpus. You are able to exclude tokens if they appear
#' too many times or too few times in the data. It can be specified as counts
#' using `max_times` and `min_times` or as percentages by setting `percentage`
#' as `TRUE`. In addition one can filter to only use the top `max_tokens` used
#' tokens. If `max_tokens` is set to `Inf` then all the tokens will be used.
#' This will generally lead to very large data sets when then tokens are words
#' or trigrams. A good strategy is to start with a low token count and go up
#' according to how much RAM you want to use.
#'
#' It is strongly advised to filter before using [step_tf] or [step_tfidf] to
#' limit the number of variables created.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the selectors or variables selected) and `value` (number of unique tokens).
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_tokenfilter"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
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
#'   step_tokenfilter(medium)
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
#' @export
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
           filter_fun = NULL,
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
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        max_times = max_times,
        min_times = min_times,
        percentage = percentage,
        max_tokens = max_tokens,
        filter_fun = filter_fun,
        res = res,
        skip = skip,
        id = id
      )
    )
  }

step_tokenfilter_new <-
  function(terms, role, trained, columns, max_times, min_times, percentage,
           max_tokens, filter_fun, res, skip, id) {
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
      filter_fun = filter_fun,
      res = res,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_tokenfilter <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names], types = "tokenlist")

  retain_words <- list()
  n_words <- integer()

  if (is.null(x$filter_fun)) {
    for (col_name in col_names) {
      retain_words[[col_name]] <- tokenfilter_fun(
        training[[col_name]],
        x$max_times, x$min_times, x$max_tokens,
        x$percentage
      )
      n_words[[col_name]] <- length(unique(unlist(training[[col_name]])))
    }
  } else {
    
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
    filter_fun = x$filter_fun,
    res = retain_words,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_tokenfilter <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  if (is.null(names(object$res)) && is.null(object$filter_fun)) {
    # Backwards compatibility with 1.0.3 (#230)
    names(object$res) <- col_names
  }
  
  for (col_name in col_names) {
    if (is.null(object$filter_fun)) {
      filtered_text <- tokenlist_filter(
        new_data[[col_name]],
        object$res[[col_name]],
        TRUE
      )
    } else {
      filtered_text <- tokenlist_filter_function(
        new_data[[col_name]],
        object$filter_fun
      )
    }

    new_data[[col_name]] <- filtered_text
  }
  new_data <- factor_to_text(new_data, col_names)

  new_data
}

#' @export
print.step_tokenfilter <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Text filtering for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_tokenfilter` object.
#' @export
tidy.step_tokenfilter <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = unname(x$columns),
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
tokenfilter_fun <- function(data, max_times, min_times, max_tokens,
                            percentage) {
  tf <- table0(unlist(get_tokens(data)))

  if (percentage) {
    tf <- tf / sum(tf)
  }

  ids <- tf <= max_times & tf >= min_times

  if (is.infinite(max_tokens)) {
    names(sort(tf[ids], decreasing = TRUE))
  } else {
    if (max_tokens > sum(ids)) {
      rlang::warn(
        glue(
          "max_tokens was set to '{max_tokens}', ",
          "but only {sum(ids)} was available and selected."
        )
      )
      max_tokens <- sum(ids)
    }
    names(sort(tf[ids], decreasing = TRUE)[seq_len(max_tokens)])
  }
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_tokenfilter <- function(x, ...) {
  c("textrecipes")
}

#' @rdname tunable_textrecipes
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
