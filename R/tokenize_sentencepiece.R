#' Sentencepiece Tokenization of Character Variables
#'
#' `step_tokenize_sentencepiece()` creates a *specification* of a recipe step
#' that will convert a character predictor into a [`token`][tokenlist()]
#' variable using SentencePiece tokenization.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param vocabulary_size An integer, indicating the number of tokens in the
#'   final vocabulary. Defaults to `1000`. Highly encouraged to be tuned.
#'   Ignored when `model` is supplied.
#' @param model A string giving the path to a pretrained sentencepiece model
#'   file, such as one downloaded with [sentencepiece::sentencepiece_download_model()]
#'   or trained with [sentencepiece::sentencepiece()]. When supplied, no model
#'   is trained at `prep()` time and the same model is used for all selected
#'   columns. The model is read when the step is prepped, so the file is not
#'   needed afterwards. Defaults to `NULL`, which trains a new model.
#' @param options A list of options passed to [sentencepiece::sentencepiece()]
#'   when the tokenizer is trained at `prep()` time. These options have no
#'   effect on already-trained models at `bake()` time. Note that the arguments
#'   `x`, `model_dir`, and `vocab_size` should not be passed here, use
#'   `vocabulary_size` instead. Ignored when `model` is supplied. Defaults to
#'   `list()`.
#' @param res The fitted [sentencepiece::sentencepiece()] model tokenizer. This
#'   is `NULL` until the step is trained by [recipes::prep.recipe()] and should
#'   not be set by hand.
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' If you are running into errors, you can investigate the progress of the
#' compiled code by setting `options = list(verbose = TRUE)`. This can reveal if
#' sentencepiece ran correctly or not.
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
#' @examplesIf rlang::is_installed(c("modeldata", "sentencepiece"))
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~., data = tate_text) |>
#'   step_tokenize_sentencepiece(medium)
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
#'
#' # Using a pretrained model
#' model_dir <- tempfile()
#' dir.create(model_dir)
#' file <- tempfile(tmpdir = model_dir)
#' writeLines(tate_text$medium, file)
#' trained <- sentencepiece::sentencepiece(
#'   file,
#'   vocab_size = 100,
#'   model_dir = model_dir
#' )
#'
#' recipe(~., data = tate_text) |>
#'   step_tokenize_sentencepiece(medium, model = trained$model_path) |>
#'   prep() |>
#'   bake(new_data = NULL, medium) |>
#'   slice(1:2)
#' @export
step_tokenize_sentencepiece <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    columns = NULL,
    vocabulary_size = 1000,
    model = NULL,
    options = list(),
    res = NULL,
    skip = FALSE,
    id = rand_id("tokenize_sentencepiece")
  ) {
    recipes::recipes_pkg_check(required_pkgs.step_tokenize_sentencepiece())

    add_step(
      recipe,
      step_tokenize_sentencepiece_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        vocabulary_size = vocabulary_size,
        model = model,
        options = options,
        res = res,
        skip = skip,
        id = id
      )
    )
  }

step_tokenize_sentencepiece_new <-
  function(
    terms,
    role,
    trained,
    columns,
    options,
    vocabulary_size,
    model,
    res,
    skip,
    id
  ) {
    step(
      subclass = "tokenize_sentencepiece",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      vocabulary_size = vocabulary_size,
      model = model,
      options = options,
      res = res,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_tokenize_sentencepiece <- function(x, training, info = NULL, ...) {
  # Recipes made before `model` was added have `x$model` as `NULL`, which
  # is the default, so no backfill is needed.
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names], types = c("string", "factor", "ordered"))

  if (!is.null(x$model)) {
    check_string(x$model, arg = "model")
    if (!file.exists(x$model) || dir.exists(x$model)) {
      cli::cli_abort(
        "The file {.file {x$model}} passed to {.arg model} does not exist."
      )
    }
    check_sentencepiece_model_file(x$model)
    tokenizer <- sentencepiece_model_tokenizer(x$model)
    tokenizers <- rlang::set_names(
      rep(list(tokenizer), length(col_names)),
      col_names
    )
  } else {
    check_number_whole(x$vocabulary_size, min = 0, arg = "vocabulary_size")
    check_options(x$options, exclude = c("x", "model_dir"))

    training <- factor_to_text(training, col_names)

    sentencepiece_options <- x$options
    if (!is.null(sentencepiece_options$vocab_size)) {
      cli::cli_abort(
        "Please supply the vocabulary size using the {.arg vocabulary_size}
      argument."
      )
    }
    sentencepiece_options$vocab_size <- x$vocabulary_size

    tokenizers <- list()
    for (col_name in col_names) {
      text <- training[[col_name]]

      check_sentencepiece_vocab_size(text, x$vocabulary_size, col_name)

      tokenizers[[col_name]] <- tokenizers_sentencepiece_tokens(
        text,
        sentencepiece_options
      )
    }
  }

  step_tokenize_sentencepiece_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    vocabulary_size = x$vocabulary_size,
    model = x$model,
    options = x$options,
    res = tokenizers,
    skip = x$skip,
    id = x$id
  )
}

check_sentencepiece_vocab_size <- function(
  text,
  vocabulary_size,
  column,
  call = caller_env()
) {
  text_count <- strsplit(as.character(text), "")
  text_count <- unlist(text_count)
  text_count <- unique(text_count)
  text_count <- length(text_count)

  if (vocabulary_size < text_count) {
    cli::cli_abort(
      "The {.arg vocabulary_size} of {vocabulary_size} is too small for column {.arg {column}} 
   which has a unique character count of {text_count}.",
      call = call
    )
  }
}

#' @export
bake.step_tokenize_sentencepiece <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  if (is.null(names(object$res))) {
    # Backwards compatibility with 1.0.3 (#230)
    names(object$res) <- col_names
  }

  recipes_map_cols(new_data, col_names, function(x, i, col_name) {
    tokenizer_fun(
      x = x,
      options = object$options,
      token = object$res[[col_name]],
      col_name = col_name
    )
  })
}

#' @export
print.step_tokenize_sentencepiece <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Sentencepiece Tokenization for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_tokenize_sentencepiece
#' @usage NULL
#' @export
tidy.step_tokenize_sentencepiece <- function(x, ...) {
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
required_pkgs.step_tokenize_sentencepiece <- function(x, ...) {
  c("sentencepiece", "textrecipes")
}
