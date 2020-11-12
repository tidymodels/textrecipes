#' Tokenization of character variables
#'
#' [step_tokenize()] creates a *specification* of a recipe step that
#'  will convert a character predictor into a [tokenlist].
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For [step_tokenize()], this indicates the variables to be encoded
#'  into a [tokenlist]. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param training_options A list of options passed to the tokenizer when it is
#'  being trained. Only applicable for engine == "tokenizers.bpe".
#' @param options A list of options passed to the tokenizer.
#' @param token Unit for tokenizing. See details for options. Defaults to
#' "words".
#' @param engine Package that will be used for tokenization. See details for
#' options. Defaults to "tokenizers".
#' @param custom_token User supplied tokenizer. Use of this argument
#'  will overwrite the token and engine arguments. Must take a character vector
#'  as input and output a list of character vectors.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake.recipe()]? While all
#'  operations are baked when [recipes::prep.recipe()] is run, some
#'  operations may not be able to be conducted on new data (e.g.
#'  processing the outcome variable(s)). Care should be taken when
#'  using `skip = TRUE` as it may affect the computations for
#'  subsequent operations.
#' @param id A character string that is unique to this step to identify it
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
#'   step_tokenize(essay0)
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
#' tidy(okc_rec, number = 1)
#' tidy(okc_obj, number = 1)
#'
#' okc_obj_chars <- recipe(~., data = okc_text) %>%
#'   step_tokenize(essay0, token = "characters") %>%
#'   prep()
#'
#' bake(okc_obj, new_data = NULL) %>%
#'   slice(2) %>%
#'   pull(essay0)
#' @export
#' @details
#' Tokenization is the act of splitting a character string into smaller parts
#' to be further analysed. This step uses the `tokenizers` package which
#' includes heuristics to split the text into paragraphs tokens, word tokens
#' amoug others. `textrecipes` keeps the tokens in a [tokenlist] and other
#' steps will do their tasks on those [tokenlist]s before transforming them
#' back to numeric.
#'
#' The choice of `engine` determines the possible choices of `token`.
#'
#' If `engine = "tokenizers"`:
#' * "words" (default)
#' * "characters"
#' * "character_shingles"
#' * "ngrams"
#' * "skip_ngrams"
#' * "sentences"
#' * "lines"
#' * "paragraphs"
#' * "regex"
#' * "tweets"
#' * "ptb" (Penn Treebank)
#' * "skip_ngrams"
#' * "word_stems"
#'
#' if `engine = "spacyr"`
#' * "words"
#'
#' Working will `textrecipes` will almost always start by calling
#' `step_tokenize` followed by modifying and filtering steps. This is not always
#' the case as you sometimes want to do apply pre-tokenization steps, this can
#' be done with [recipes::step_mutate()].
#'
#' @seealso [step_untokenize()] to untokenize.
#' @family character to tokenlist steps
step_tokenize <-
  function(recipe,
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
           id = rand_id("tokenize")) {
    add_step(
      recipe,
      step_tokenize_new(
        terms = ellipse_check(...),
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
  function(terms, role, trained, columns, training_options, options, token,
           engine, custom_token, skip, id) {
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
  col_names <- terms_select(x$terms, info = info)

  training <- factor_to_text(training, col_names)

  check_type(training[, col_names], quant = FALSE)

  tokenizers <- list()

  for (i in seq_along(col_names)) {
    tokenizers[[i]] <- x$custom_token %||%
      tokenizer_switch(x$token, x, training[, col_names[[i]], drop = TRUE])
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
  # for backward compat

  for (i in seq_along(col_names)) {
    new_data[, col_names[i]] <- tokenizer_fun(
      data = new_data[, col_names[i]],
      name = col_names[i],
      options = object$options,
      token = object$custom_token[[i]]
    )
  }
  as_tibble(new_data)
}

#' @export
print.step_tokenize <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Tokenization for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_tokenize
#' @param x A `step_tokenize` object.
#' @export
tidy.step_tokenize <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = x$terms,
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
tokenizer_fun <- function(data, name, options, token, ...) {
  check_type(data[, name], quant = FALSE)

  data <- factor_to_text(data, name)

  token_expr <- expr(
    token(
      x = data[, 1, drop = TRUE]
    )
  )

  if (length(options) > 0) {
    token_expr <- mod_call_args(token_expr, args = options)
  }

  token_list <- eval(token_expr)

  if (is_tokenlist(token_list)) {
    out <- tibble::tibble(token_list)
  } else {
    out <- tibble::tibble(tokenlist(token_list))
  }
  names(out) <- name
  out
}

tokenizer_switch <- function(name, object, data) {
  if (object$engine == "tokenizers") {
    possible_tokenizers <-
      c(
        "characters", "character_shingle", "lines", "ngrams",
        "paragraphs", "ptb", "regex", "sentences", "skip_ngrams",
        "tweets", "words", "word_stems"
      )

    if (!(name %in% possible_tokenizers)) {
      rlang::abort(paste0("token should be one of the supported ",
        "'", possible_tokenizers, "'",
        collapse = ", "
      ))
    }

    res <- switch(name,
      characters = tokenizers::tokenize_characters,
      character_shingle = tokenizers::tokenize_character_shingles,
      lines = tokenizers::tokenize_lines,
      ngrams = tokenizers::tokenize_ngrams,
      paragraphs = tokenizers::tokenize_paragraphs,
      ptb = tokenizers::tokenize_ptb,
      regex = tokenizers::tokenize_regex,
      sentences = tokenizers::tokenize_sentences,
      skip_ngrams = tokenizers::tokenize_skip_ngrams,
      tweets = tokenizers::tokenize_tweets,
      words = tokenizers::tokenize_words,
      word_stems = tokenizers::tokenize_word_stems
    )
    return(res)
  }

  if (object$engine == "spacyr") {
    recipes::recipes_pkg_check(required_pkgs.step_tokenize(object))

    possible_tokenizers <- c("words")

    if (!(name %in% possible_tokenizers)) {
      rlang::abort(paste0(
        "token should be one of the supported ",
        "'", 
        possible_tokenizers, 
        "'",
        collapse = ", "
      ))
    }

    res <- switch(name,
      words = spacyr_tokenizer_words
    )
    return(res)
  }

  if (object$engine == "tokenizers.bpe") {
    recipes::recipes_pkg_check(required_pkgs.step_tokenize(object))

    possible_tokenizers <- c("words")

    if (!(name %in% possible_tokenizers)) {
      rlang::abort(paste0(
        "token should be one of the supported ",
        "'",
        possible_tokenizers,
        "'",
        collapse = ", "
      ))
    }

    res <- switch(name,
      words = tokenizers_bpe_words(data, object$training_options)
    )
    return(res)
  }

  if (object$engine == "udpipe") {
    recipes::recipes_pkg_check(required_pkgs.step_tokenize(object))

    possible_tokenizers <- c("words")

    if (!(name %in% possible_tokenizers)) {
      rlang::abort(paste0(
        "token should be one of the supported ",
        "'",
        possible_tokenizers,
        "'",
        collapse = ", "
      ))
    }

    res <- switch(name,
      words = udpipe_words(object$training_options$model)
    )
    return(res)
  }

  rlang::abort("`engine` argument is not valid.")
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

#' @rdname tunable.step
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
