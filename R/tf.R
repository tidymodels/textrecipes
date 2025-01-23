#' Term frequency of Tokens
#'
#' `step_tf()` creates a *specification* of a recipe step that will convert a
#' [`token`][tokenlist()] variable into multiple variables containing the token
#' counts.
#' 
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param weight_scheme A character determining the weighting scheme for the
#'   term frequency calculations. Must be one of "binary", "raw count", "term
#'   frequency", "log normalization" or "double normalization". Defaults to "raw
#'   count".
#' @param weight A numeric weight used if `weight_scheme` is set to "double
#'   normalization". Defaults to 0.5.
#' @param vocabulary A character vector of strings to be considered.
#' @param res The words that will be used to calculate the term frequency will
#'   be stored here once this preprocessing step has be trained by
#'   [recipes::prep.recipe()].
#' @template args-prefix
#' @template args-keep_original_cols
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' It is strongly advised to use [step_tokenfilter] before using [step_tf] to
#' limit the number of variables created, otherwise you might run into memory
#' issues. A good strategy is to start with a low token count and go up
#' according to how much RAM you want to use.
#'
#' Term frequency is a weight of how many times each token appears in each
#' observation. There are different ways to calculate the weight and this step
#' can do it in a couple of ways. Setting the argument `weight_scheme` to
#' "binary" will result in a set of binary variables denoting if a token is
#' present in the observation. "raw count" will count the times a token is
#' present in the observation. "term frequency" will divide the count by the
#' total number of words in the document to limit the effect of the document
#' length as longer documents tends to have the word present more times but not
#' necessarily at a higher percentage. "log normalization" takes the log of 1
#' plus the count, adding 1 is done to avoid taking log of 0. Finally "double
#' normalization" is the raw frequency divided by the raw frequency of the most
#' occurring term in the document. This is then multiplied by `weight` and
#' `weight` is added to the result. This is again done to prevent a bias towards
#' longer documents.
#'
#' @template details-prefix
#'
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value`, and `id`:
#' 
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{character, the weighting scheme}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_tf"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @seealso [step_tokenize()] to turn characters into [`tokens`][tokenlist()]
#' @family Steps for Numeric Variables From Tokens
#'
#' @examplesIf rlang::is_installed("modeldata")
#' \donttest{
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~., data = tate_text) %>%
#'   step_tokenize(medium) %>%
#'   step_tf(medium)
#'
#' tate_obj <- tate_rec %>%
#'   prep()
#'
#' bake(tate_obj, tate_text)
#'
#' tidy(tate_rec, number = 2)
#' tidy(tate_obj, number = 2)
#' }
#'
#' @export
step_tf <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           columns = NULL,
           weight_scheme = "raw count",
           weight = 0.5,
           vocabulary = NULL,
           res = NULL,
           prefix = "tf",
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("tf")) {
    add_step(
      recipe,
      step_tf_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        res = res,
        columns = columns,
        weight_scheme = weight_scheme,
        weight = weight,
        vocabulary = vocabulary,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

tf_funs <- c(
  "binary", "raw count", "term frequency", "log normalization",
  "double normalization"
)

step_tf_new <-
  function(terms, role, trained, columns, weight_scheme, weight, vocabulary,
           res, prefix, keep_original_cols, skip, id) {
    step(
      subclass = "tf",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      weight_scheme = weight_scheme,
      weight = weight,
      vocabulary = vocabulary,
      res = res,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_tf <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  rlang::arg_match0(x$weight_scheme, tf_funs, arg_nm = "weight_scheme")
  check_number_decimal(x$weight, arg = "weight")
  check_character(x$vocabulary, allow_null = TRUE, arg = "vocabulary")
  check_string(x$prefix, arg = "prefix")

  check_type(training[, col_names], types = "tokenlist")

  token_list <- list()

  for (col_name in col_names) {
    token_list[[col_name]] <- x$vocabulary %||%
      sort(get_unique_tokens(training[[col_name]]))
  }

  step_tf_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    weight_scheme = x$weight_scheme,
    weight = x$weight,
    vocabulary = x$vocabulary,
    res = token_list,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_tf <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  if (is.null(names(object$res))) {
    # Backwards compatibility with 1.0.3 (#230)
    names(object$res) <- col_names
  }
  
  for (col_name in col_names) {
    tf_text <- tf_function(
      new_data[[col_name]],
      object$res[[col_name]],
      paste0(object$prefix, "_", col_name),
      object$weight_scheme,
      object$weight
    )

    if (object$weight_scheme %in% c("binary", "raw count")) {
      tf_text <- purrr::map_dfc(tf_text, as.integer)
    }

    tf_text <- recipes::check_name(tf_text, new_data, object, names(tf_text))

    new_data <- vec_cbind(new_data, tf_text)
  }
  
  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

#' @export
print.step_tf <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Term frequency with "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_tf
#' @usage NULL
#' @export
tidy.step_tf <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = unname(x$columns),
      value = x$weight_scheme
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

tf_function <- function(data, names, labels, weights, weight) {
  counts <- as.matrix(tokenlist_to_dtm(data, names))

  tf <- tf_weight(counts, weights, weight)
  colnames(tf) <- paste0(labels, "_", names)
  as_tibble(tf)
}

tf_weight <- function(x, scheme, weight) {
  if (scheme == "binary") {
    return(x > 0)
  }
  if (scheme == "raw count") {
    return(x)
  }
  if (scheme == "term frequency") {
    rowsums_x <- rowSums(x)
    res <- x / rowsums_x
    res[rowsums_x == 0, ] <- 0
    return(res)
  }
  if (scheme == "log normalization") {
    return(log(1 + x))
  }
  if (scheme == "double normalization") {
    max_ftd <- apply(x, 1, max)
    return(weight + weight * x / max_ftd)
  }
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_tf <- function(x, ...) {
  c("textrecipes")
}

#' @rdname tunable_textrecipes
#' @export
tunable.step_tf <- function(x, ...) {
  tibble::tibble(
    name = c("weight_scheme", "weight"),
    call_info = list(
      list(pkg = "dials", fun = "weight_scheme"),
      list(pkg = "dials", fun = "weight")
    ),
    source = "recipe",
    component = "step_tf",
    component_id = x$id
  )
}
