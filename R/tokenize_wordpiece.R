#' Wordpiece Tokenization of Character Variables
#'
#' `step_tokenize_wordpiece()` creates a *specification* of a recipe step that
#' will convert a character predictor into a [`token`][tokenlist()] variable
#' using WordPiece tokenization.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param vocab Character of Character vector of vocabulary tokens. Defaults to
#'   `wordpiece_vocab()`.
#' @param unk_token Token to represent unknown words. Defaults to `"[UNK]"`.
#' @param max_chars Integer, Maximum length of word recognized. Defaults to 100.
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @seealso [step_untokenize()] to untokenize.
#' @family Steps for Tokenization
#'
#' @examplesIf rlang::is_installed(c("modeldata", "wordpiece"))
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~., data = tate_text) |>
#'   step_tokenize_wordpiece(medium)
#'
#' tate_obj <- tate_rec |>
#'   prep()
#'
#' bake(tate_obj, new_data = NULL, medium) |>
#'   slice(1:2)
#'
#' bake(tate_obj, new_data = NULL) |>
#'   slice(2) |>
#'   pull(medium)
#'
#' tidy(tate_rec, number = 1)
#' tidy(tate_obj, number = 1)
#' @export
step_tokenize_wordpiece <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    columns = NULL,
    vocab = wordpiece::wordpiece_vocab(),
    unk_token = "[UNK]",
    max_chars = 100,
    skip = FALSE,
    id = rand_id("tokenize_wordpiece")
  ) {
    recipes::recipes_pkg_check(required_pkgs.step_tokenize_wordpiece())

    add_step(
      recipe,
      step_tokenize_wordpiece_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        vocab = vocab,
        unk_token = unk_token,
        max_chars = max_chars,
        skip = skip,
        id = id
      )
    )
  }

step_tokenize_wordpiece_new <-
  function(
    terms,
    role,
    trained,
    columns,
    vocab,
    unk_token,
    max_chars,
    skip,
    id
  ) {
    step(
      subclass = "tokenize_wordpiece",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      vocab = vocab,
      unk_token = unk_token,
      max_chars = max_chars,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_tokenize_wordpiece <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_string(x$unk_token, arg = "unk_token")
  check_number_whole(x$max_chars, min = 0, arg = "max_chars")

  training <- factor_to_text(training, col_names)

  check_type(training[, col_names], types = c("string", "factor", "ordered"))

  step_tokenize_wordpiece_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    vocab = x$vocab,
    unk_token = x$unk_token,
    max_chars = x$max_chars,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_tokenize_wordpiece <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_data[[col_name]] <- tokenizer_fun(
      x = new_data[[col_name]],
      options = list(
        vocab = object$vocab,
        unk_token = object$unk_token,
        max_chars = object$max_chars
      ),
      token = function(x, ...) {
        res <- wordpiece::wordpiece_tokenize(text = x, ...)
        lapply(res, names)
      }
    )
  }

  new_data
}

#' @export
print.step_tokenize_wordpiece <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "wordpiece Tokenization for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_tokenize_wordpiece
#' @usage NULL
#' @export
tidy.step_tokenize_wordpiece <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = unname(x$columns)
    )
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
required_pkgs.step_tokenize_wordpiece <- function(x, ...) {
  c("wordpiece", "textrecipes")
}
