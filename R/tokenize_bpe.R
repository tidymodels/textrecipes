#' BPE Tokenization of Character Variables
#'
#' `step_tokenize_bpe()` creates a *specification* of a recipe step that will
#' convert a character predictor into a [`token`][tokenlist()] variable using
#' Byte Pair Encoding.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param vocabulary_size Integer, indicating the number of tokens in the final
#'   vocabulary. Defaults to 1000. Highly encouraged to be tuned.
#' @param options A list of options passed to the tokenizer.
#' @param res The fitted [tokenizers.bpe::bpe()] model tokenizer will be stored
#'   here once this preprocessing step has be trained by [prep.recipe()].
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the selectors or variables selected).
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_tokenize_bpe"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @seealso [step_untokenize()] to untokenize.
#' @family Steps for Tokenization
#'
#' @examplesIf rlang::is_installed("tokenizers.bpe")
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~., data = tate_text) %>%
#'   step_tokenize_bpe(medium)
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
#' tidy(tate_rec, number = 1)
#' tidy(tate_obj, number = 1)
#' @export
step_tokenize_bpe <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           vocabulary_size = 1000,
           options = list(),
           res = NULL,
           skip = FALSE,
           id = rand_id("tokenize_bpe")) {
    recipes::recipes_pkg_check(required_pkgs.step_tokenize_bpe())

    add_step(
      recipe,
      step_tokenize_bpe_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        vocabulary_size = vocabulary_size,
        options = options,
        res = res,
        skip = skip,
        id = id
      )
    )
  }

step_tokenize_bpe_new <-
  function(terms, role, trained, columns, options, vocabulary_size, res, skip,
           id) {
    step(
      subclass = "tokenize_bpe",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      vocabulary_size = vocabulary_size,
      options = options,
      res = res,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_tokenize_bpe <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  training <- factor_to_text(training, col_names)

  check_type(training[, col_names], types = c("string", "factor", "ordered"))

  tokenizers <- list()

  bpe_options <- x$options
  if (!is.null(bpe_options$vocab_size)) {
    rlang::abort(
      "Please supply the vocabulary size using the `vocabulary_size` argument."
    )
  }
  bpe_options$vocab_size <- x$vocabulary_size

  for (col_name in col_names) {
    text <- training[[col_name]]

    check_bpe_vocab_size(text, x$vocabulary_size, col_name)

    tokenizers[[col_name]] <- tokenizers_bpe_tokens(text, bpe_options)
  }

  step_tokenize_bpe_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    vocabulary_size = x$vocabulary_size,
    options = x$options,
    res = tokenizers,
    skip = x$skip,
    id = x$id
  )
}

check_bpe_vocab_size <- function(text,
                                 vocabulary_size,
                                 column,
                                 call = caller_env()) {
  text_count <- strsplit(as.character(text), "")
  text_count <- unlist(text_count)
  text_count <- unique(text_count)
  text_count <- length(text_count)

  if (vocabulary_size < text_count) {
    rlang::abort(
      glue(
        "`vocabulary_size` of {vocabulary_size} is too small for column ",
        "`{column}` which has a unique character count of {text_count}",
      ),
      call = call
    )
  }
}

#' @export
bake.step_tokenize_bpe <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  if (is.null(names(object$res))) {
    # Backwards compatibility with 1.0.3 (#230)
    names(object$res) <- col_names
  }
  
  for (col_name in col_names) {
    new_data[[col_name]] <- tokenizer_fun(
      x = new_data[[col_name]],
      options = object$options,
      token = object$res[[col_name]]
    )
  }

  new_data
}

#' @export
print.step_tokenize_bpe <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "BPE Tokenization for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_tokenize_bpe` object.
#' @export
tidy.step_tokenize_bpe <- function(x, ...) {
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
required_pkgs.step_tokenize_bpe <- function(x, ...) {
  c("tokenizers.bpe", "textrecipes")
}

#' @rdname tunable_textrecipes
#' @export
tunable.step_tokenize_bpe <- function(x, ...) {
  tibble::tibble(
    name = c("vocabulary_size"),
    call_info = list(
      list(pkg = "dials", fun = "vocabulary_size", range = c(1000, 32000))
    ),
    source = "recipe",
    component = "step_tokenize_bpe",
    component_id = x$id
  )
}
