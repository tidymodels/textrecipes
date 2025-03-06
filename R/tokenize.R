#' Tokenization of Character Variables
#'
#' `step_tokenize()` creates a *specification* of a recipe step that will
#' convert a character predictor into a [`token`][tokenlist()] variable.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param training_options A list of options passed to the tokenizer when it is
#'   being trained. Only applicable for engine == "tokenizers.bpe".
#' @param options A list of options passed to the tokenizer.
#' @param token Unit for tokenizing. See details for options. Defaults to
#'   "words".
#' @param engine Package that will be used for tokenization. See details for
#'   options. Defaults to "tokenizers".
#' @param custom_token User supplied tokenizer. Use of this argument will
#'   overwrite the token and engine arguments. Must take a character vector as
#'   input and output a list of character vectors.
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' ```{r, echo=FALSE}
#' options(width = 55)
#' ```
#'
#' Tokenization is the act of splitting a character vector into smaller parts to
#' be further analyzed. This step uses the `tokenizers` package which includes
#' heuristics on how to to split the text into paragraphs tokens, word tokens,
#' among others. `textrecipes` keeps the tokens as a [`token`][tokenlist()]
#' variable and other steps will do their tasks on those [`token`][tokenlist()]
#' variables before transforming them back to numeric variables.
#'
#' Working with `textrecipes` will almost always start by calling
#' `step_tokenize` followed by modifying and filtering steps. This is not always
#' the case as you sometimes want to apply pre-tokenization steps; this can
#' be done with [recipes::step_mutate()].
#'
#' # Engines
#'
#' The choice of `engine` determines the possible choices of `token`.
#'
#' The following is some small example data used in the following examples
#'
#' ```{r}
#' text_tibble <- tibble(
#'   text = c("This is words", "They are nice!")
#' )
#' ```
#'
#' ## tokenizers
#'
#' The tokenizers package is the default `engine` and it comes with the
#' following unit of `token`. All of these options correspond to a function in
#' the tokenizers package.
#'
#' * "words" (default)
#' * "characters"
#' * "character_shingles"
#' * "ngrams"
#' * "skip_ngrams"
#' * "sentences"
#' * "lines"
#' * "paragraphs"
#' * "regex"
#' * "ptb" (Penn Treebank)
#' * "skip_ngrams"
#' * "word_stems"
#'
#' The default tokenizer is `"word"` which splits the text into a series of
#' words. By using `step_tokenize()` without setting any arguments you get word
#' tokens
#'
#' ```{r}
#' recipe(~ text, data = text_tibble) %>%
#'   step_tokenize(text) %>%
#'   show_tokens(text)
#' ```
#'
#' This tokenizer has arguments that change how the tokenization occurs and can
#' accessed using the `options` argument by passing a named list. Here we are
#' telling [tokenizers::tokenize_words] that we don't want to turn the words to
#' lowercase
#'
#' ```{r}
#' recipe(~ text, data = text_tibble) %>%
#'   step_tokenize(text,
#'                 options = list(lowercase = FALSE)) %>%
#'   show_tokens(text)
#' ```
#'
#' We can also stop removing punctuation.
#'
#' ```{r}
#' recipe(~ text, data = text_tibble) %>%
#'   step_tokenize(text,
#'                 options = list(strip_punct = FALSE,
#'                                lowercase = FALSE)) %>%
#'   show_tokens(text)
#' ```
#'
#' The tokenizer can be changed by setting a different `token`. Here we change
#' it to return character tokens.
#'
#' ```{r}
#' recipe(~ text, data = text_tibble) %>%
#'   step_tokenize(text, token = "characters") %>%
#'   show_tokens(text)
#' ```
#'
#' It is worth noting that not all these token methods are appropriate but are
#' included for completeness.
#'
#' ## spacyr
#'
#' * "words"
#'
#' ## tokenizers.bpe
#'
#' The tokeenizers.bpe engine performs Byte Pair Encoding Text Tokenization.
#'
#' * "words"
#'
#' This tokenizer is trained on the training set and will thus need to be passed
#' training arguments. These are passed to the `training_options` argument and
#' the most important one is `vocab_size`. The determines the number of unique
#' tokens the tokenizer will produce. It is generally set to a much higher
#' value, typically in the thousands, but is set to 22 here for demonstration
#' purposes.
#'
#' ```{r, eval=FALSE}
#' recipe(~ text, data = text_tibble) %>%
#'   step_tokenize(
#'     text,
#'     engine = "tokenizers.bpe",
#'     training_options = list(vocab_size = 22)
#'   ) %>%
#'   show_tokens(text)
#' ```
#'
#' ```{r, echo=FALSE}
#' recipe(~ text, data = text_tibble) %>%
#'   step_tokenize(
#'     text,
#'     engine = "tokenizers.bpe",
#'     training_options = list(vocab_size = 22)
#'   ) %>%
#'   show_tokens(text) %>%
#'   lapply(function(x) gsub("▁", "_", x))
#' ```
#'
#' ## udpipe
#'
#' * "words"
#'
#' ## custom_token
#'
#' Sometimes you need to perform tokenization that is not covered by the
#' supported engines. In that case you can use the `custom_token` argument to
#' pass a function in that performs the tokenization you want.
#'
#' Below is an example of a very simple space tokenization. This is a very fast
#' way of tokenizing.
#'
#' ```{r}
#' space_tokenizer <- function(x) {
#'   strsplit(x, " +")
#' }
#'
#' recipe(~ text, data = text_tibble) %>%
#'   step_tokenize(
#'     text,
#'     custom_token = space_tokenizer
#'   ) %>%
#'   show_tokens(text)
#' ```
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{character, unit of tokenization}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_tokenize"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @seealso [step_untokenize()] to untokenize.
#' @family Steps for Tokenization
#'
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~., data = tate_text) %>%
#'   step_tokenize(medium)
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
#'
#' tate_obj_chars <- recipe(~., data = tate_text) %>%
#'   step_tokenize(medium, token = "characters") %>%
#'   prep()
#'
#' bake(tate_obj, new_data = NULL) %>%
#'   slice(2) %>%
#'   pull(medium)
#' @export
step_tokenize <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    columns = NULL,
    training_options = list(),
    options = list(),
    token = "words",
    engine = "tokenizers",
    custom_token = NULL,
    skip = FALSE,
    id = rand_id("tokenize")
  ) {
    add_step(
      recipe,
      step_tokenize_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        training_options = training_options,
        options = options,
        token = token,
        engine = engine,
        custom_token = custom_token,
        skip = skip,
        id = id
      )
    )
  }

step_tokenize_new <-
  function(
    terms,
    role,
    trained,
    columns,
    training_options,
    options,
    token,
    engine,
    custom_token,
    skip,
    id
  ) {
    step(
      subclass = "tokenize",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      training_options = training_options,
      options = options,
      token = token,
      engine = engine,
      custom_token = custom_token,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_tokenize <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_string(x$token, arg = "token")
  check_string(x$engine, arg = "engine")
  check_function(x$custom_token, allow_null = TRUE, arg = "custom_token")

  training <- factor_to_text(training, col_names)

  check_type(training[, col_names], types = c("string", "factor", "ordered"))

  tokenizers <- list()

  for (col_name in col_names) {
    text <- training[[col_name]]

    if (
      x$engine == "tokenizers.bpe" & !is.null(x$training_options$vocab_size)
    ) {
      check_bpe_vocab_size(text, x$training_options$vocab_size, col_name)
    }

    tokenizers[[col_name]] <- x$custom_token %||%
      tokenizer_switch(x$token, x, text)
  }

  step_tokenize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    training_options = x$training_options,
    options = x$options,
    token = x$token,
    engine = x$engine,
    custom_token = tokenizers,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_tokenize <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  if (is.null(names(object$custom_token))) {
    # Backwards compatibility with 1.0.3 (#230)
    names(object$custom_token) <- col_names
  }

  for (col_name in col_names) {
    new_data[[col_name]] <- tokenizer_fun(
      x = new_data[[col_name]],
      options = object$options,
      token = object$custom_token[[col_name]]
    )
  }
  new_data
}

#' @export
print.step_tokenize <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Tokenization for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_tokenize
#' @usage NULL
#' @export
tidy.step_tokenize <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = unname(x$columns),
      value = x$token
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

## Implementation
tokenizer_fun <- function(x, options, token, ...) {
  if (is.factor(x)) {
    x <- as.character.factor(x)
  }
  token_expr <- expr(
    token(
      x = x
    )
  )

  if (length(options) > 0) {
    token_expr <- rlang::call_modify(token_expr, !!!options)
  }

  out <- eval(token_expr)

  if (!is_tokenlist(out)) {
    out <- tokenlist(out)
  }
  out
}

tokenizer_switch <- function(name, object, data, call = caller_env()) {
  if (object$engine == "tokenizers") {
    possible_tokenizers <-
      c(
        "characters",
        "character_shingle",
        "lines",
        "ngrams",
        "paragraphs",
        "ptb",
        "regex",
        "sentences",
        "skip_ngrams",
        "words",
        "word_stems"
      )

    check_possible_tokenizers(name, possible_tokenizers)

    res <- switch(
      name,
      characters = tokenizers::tokenize_characters,
      character_shingle = tokenizers::tokenize_character_shingles,
      lines = tokenizers::tokenize_lines,
      ngrams = tokenizers::tokenize_ngrams,
      paragraphs = tokenizers::tokenize_paragraphs,
      ptb = tokenizers::tokenize_ptb,
      regex = tokenizers::tokenize_regex,
      sentences = tokenizers::tokenize_sentences,
      skip_ngrams = tokenizers::tokenize_skip_ngrams,
      words = tokenizers::tokenize_words,
      word_stems = tokenizers::tokenize_word_stems
    )
    return(res)
  }

  if (object$engine == "spacyr") {
    recipes::recipes_pkg_check(required_pkgs.step_tokenize(object))

    possible_tokenizers <- c("words")

    check_possible_tokenizers(name, possible_tokenizers)

    res <- switch(name, words = spacyr_tokenizer_words)
    return(res)
  }

  if (object$engine == "tokenizers.bpe") {
    recipes::recipes_pkg_check(required_pkgs.step_tokenize(object))

    possible_tokenizers <- c("words")

    check_possible_tokenizers(name, possible_tokenizers)

    res <- switch(
      name,
      words = tokenizers_bpe_tokens(data, object$training_options)
    )
    return(res)
  }

  if (object$engine == "udpipe") {
    recipes::recipes_pkg_check(required_pkgs.step_tokenize(object))

    possible_tokenizers <- c("words")

    check_possible_tokenizers(name, possible_tokenizers)

    res <- switch(name, words = udpipe_words(object$training_options$model))
    return(res)
  }

  cli::cli_abort("The {.arg engine} argument is not valid.", call = call)
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_tokenize <- function(x, ...) {
  if (x$engine == "spacyr") {
    c("spacyr", "textrecipes")
  } else if (x$engine == "tokenizers.bpe") {
    c("tokenizers.bpe", "textrecipes")
  } else if (x$engine == "udpipe") {
    c("udpipe", "textrecipes")
  } else {
    "textrecipes"
  }
}

#' @rdname tunable_textrecipes
#' @export
tunable.step_tokenize <- function(x, ...) {
  tibble::tibble(
    name = c("token"),
    call_info = list(
      list(pkg = "dials", fun = "token")
    ),
    source = "recipe",
    component = "step_tokenize",
    component_id = x$id
  )
}
