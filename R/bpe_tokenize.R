#' BPE Tokenization of character variables
#'
#' [step_bpe_tokenize()] creates a *specification* of a recipe step that
#'  will convert a character predictor into a [tokenlist] using Byte Pair
#'  Encoding.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
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
#' This tokenizer is trained on the training set and will thus need to be passed
#' training arguments. These are passed to the `training_options` argument and 
#' the most important one is `vocab_size`. The determines the number of unique
#' tokens the tokenizer will produce. It is generally set to a much higher
#' value, typically in the thousands, but is set to 22 here for demonstration
#' purposes.
#'
#' @seealso [step_untokenize()] to untokenize.
#' @family character to tokenlist steps
#' 
#' @examples
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~., data = tate_text) %>%
#'   step_bpe_tokenize(medium)
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
#' @export
step_bpe_tokenize <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           options = list(),
           res = NULL,
           skip = FALSE,
           id = rand_id("bpe_tokenize")) {
    add_step(
      recipe,
      step_bpe_tokenize_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        options = options,
        res = res,
        skip = skip,
        id = id
      )
    )
  }

step_bpe_tokenize_new <-
  function(terms, role, trained, columns, options, res, skip, id) {
    step(
      subclass = "bpe_tokenize",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      options = options,
      res = res,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_bpe_tokenize <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  
  training <- factor_to_text(training, col_names)
  
  check_type(training[, col_names], quant = FALSE)
  
  tokenizers <- list()
  
  for (i in seq_along(col_names)) {
    tokenizers[[i]] <- tokenizers_bpe_words(
      training[, col_names[[i]], drop = TRUE], 
      x$training_options
    )
  }
  
  step_bpe_tokenize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    options = x$options,
    res = tokenizers,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_bpe_tokenize <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat
  
  for (i in seq_along(col_names)) {
    new_data[, col_names[i]] <- tokenizer_fun(
      data = new_data[, col_names[i]],
      name = col_names[i],
      options = object$options,
      token = object$res[[i]]
    )
  }
  
  as_tibble(new_data)
}

#' @export
print.step_bpe_tokenize <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("BPE Tokenization for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_bpe_tokenize` object.
#' @export
tidy.step_bpe_tokenize <- function(x, ...) {
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

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_bpe_tokenize <- function(x, ...) {
  c("tokenizers.bpe", "textrecipes")
}

#' @rdname tunable.step
#' @export
tunable.step_bpe_tokenize <- function(x, ...) {
  #tibble::tibble(
  #  name = c("token"),
  #  call_info = list(
  #    list(pkg = "dials", fun = "token")
  #  ),
  #  source = "recipe",
  #  component = "step_bpe_tokenize",
  #  component_id = x$id
  #)
}
