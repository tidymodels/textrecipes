#' Generate n-grams From Token Variables
#'
#' `step_ngram()` creates a *specification* of a recipe step that will convert a
#' [`token`][tokenlist()] variable into a [`token`][tokenlist()] variable of
#' ngrams.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param num_tokens The number of tokens in the n-gram. This must be an integer
#'   greater than or equal to 1. Defaults to 3.
#' @param min_num_tokens The minimum number of tokens in the n-gram. This must
#'   be an integer greater than or equal to 1 and smaller than `n`. Defaults to
#'   3.
#' @param delim The separator between words in an n-gram. Defaults to "_".
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' The use of this step will leave the ordering of the tokens meaningless. If
#' `min_num_tokens <  num_tokens` then the tokens order in increasing fashion
#' with respect to the number of tokens in the n-gram. If `min_num_tokens = 1`
#' and `num_tokens = 3` then the output contains all the 1-grams followed by all
#' the 2-grams followed by all the 3-grams.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the selectors or variables selected).
#' 
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_ngram"
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
#'   step_ngram(medium)
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
step_ngram <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           num_tokens = 3L,
           min_num_tokens = 3L,
           delim = "_",
           skip = FALSE,
           id = rand_id("ngram")) {
    add_step(
      recipe,
      step_ngram_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        num_tokens = num_tokens,
        min_num_tokens = min_num_tokens,
        delim = delim,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_ngram_new <-
  function(terms, role, trained, columns, num_tokens, min_num_tokens, delim,
           skip, id) {
    step(
      subclass = "ngram",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      num_tokens = num_tokens,
      min_num_tokens = min_num_tokens,
      delim = delim,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_ngram <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names], types = "tokenlist")

  step_ngram_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    num_tokens = x$num_tokens,
    min_num_tokens = x$min_num_tokens,
    delim = x$delim,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_ngram <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    ngrammed_tokenlist <- tokenlist_ngram(
      x = new_data[[col_name]],
      n = object$num_tokens,
      n_min = object$min_num_tokens,
      delim = object$delim
    )

    new_data[[col_name]] <- ngrammed_tokenlist
  }
  new_data <- factor_to_text(new_data, col_names)
  new_data
}

#' @export
print.step_ngram <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "ngramming for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_ngram` object.
#' @export
tidy.step_ngram <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$columns))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_ngram <- function(x, ...) {
  c("textrecipes")
}

#' @rdname tunable_textrecipes
#' @export
tunable.step_ngram <- function(x, ...) {
  tibble::tibble(
    name = c("num_tokens"),
    call_info = list(
      list(pkg = "dials", fun = "num_tokens", range = c(1, 3))
    ),
    source = "recipe",
    component = "step_ngram",
    component_id = x$id
  )
}
