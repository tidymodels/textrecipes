#' Feature Hashing of Tokens
#'
#' `step_texthash()` creates a *specification* of a recipe step that will
#' convert a [`token`][tokenlist()] variable into multiple numeric variables
#' using the hashing trick.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param signed A logical, indicating whether to use a signed hash-function to
#'   reduce collisions when hashing. Defaults to TRUE.
#' @param num_terms An integer, the number of variables to output. Defaults to
#'   1024.
#' @template args-prefix
#' @template args-keep_original_cols
#' @template args-skip
#' @template args-id
#'
#' @template returns
#'
#' @details
#'
#' Feature hashing, or the hashing trick, is a transformation of a text variable
#' into a new set of numerical variables. This is done by applying a hashing
#' function over the tokens and using the hash values as feature indices. This
#' allows for a low memory representation of the text. This implementation is
#' done using the MurmurHash3 method.
#'
#' The argument `num_terms` controls the number of indices that the hashing
#' function will map to. This is the tuning parameter for this transformation.
#' Since the hashing function can map two different tokens to the same index,
#' will a higher value of `num_terms` result in a lower chance of collision.
#'
#' @template details-prefix
#'
#' @details # Tidying
#'
#'   When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#'   (the selectors or variables selected) and `value` (number of terms).
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_texthash"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @references Kilian Weinberger; Anirban Dasgupta; John Langford; Alex Smola;
#'   Josh Attenberg (2009).
#'
#' @seealso [step_tokenize()] to turn characters into [`tokens`][tokenlist()]
#'   [step_text_normalization()] to perform text normalization.
#' @family Steps for Numeric Variables From Tokens
#'
#' @examplesIf all(c("text2vec", "data.table") %in% rownames(installed.packages()))
#' \dontshow{library(data.table)}
#' \dontshow{data.table::setDTthreads(2)}
#' \dontshow{Sys.setenv("OMP_THREAD_LIMIT" = 2)}
#' library(recipes)
#' library(modeldata)
#' data(tate_text)
#'
#' tate_rec <- recipe(~., data = tate_text) %>%
#'   step_tokenize(medium) %>%
#'   step_tokenfilter(medium, max_tokens = 10) %>%
#'   step_texthash(medium)
#'
#' tate_obj <- tate_rec %>%
#'   prep()
#'
#' bake(tate_obj, tate_text)
#'
#' tidy(tate_rec, number = 3)
#' tidy(tate_obj, number = 3)
#' @export
step_texthash <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           columns = NULL,
           signed = TRUE,
           num_terms = 1024L,
           prefix = "texthash",
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("texthash")) {
    recipes::recipes_pkg_check(required_pkgs.step_texthash())

    add_step(
      recipe,
      step_texthash_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        signed = signed,
        num_terms = num_terms,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

hash_funs <- c(
  "md5", "sha1", "crc32", "sha256", "sha512", "xxhash32",
  "xxhash64", "murmur32"
)

step_texthash_new <-
  function(terms, role, trained, columns, signed, num_terms, prefix,
           keep_original_cols, skip, id) {
    step(
      subclass = "texthash",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      signed = signed,
      num_terms = num_terms,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_texthash <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names], types = "tokenlist")

  step_texthash_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    signed = x$signed,
    num_terms = x$num_terms,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_texthash <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    tf_text <- hashing_function(
      get_tokens(new_data[[col_name]]),
      paste0(
        object$prefix, "_",
        col_name, "_",
        names0(object$num_terms, "")
      ),
      object$signed,
      object$num_terms
    )

    tf_text <- purrr::map_dfc(tf_text, as.integer)

    tf_text <- check_name(tf_text, new_data, object, names(tf_text))

    new_data <- vec_cbind(new_data, tf_text)
  }
  
  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

#' @export
print.step_texthash <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Feature hashing with "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_texthash` object.
#' @export
tidy.step_texthash <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = unname(x$columns),
      value = x$signed,
      length = x$num_terms
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_lgl,
      length = na_int
    )
  }
  res$id <- x$id
  res
}

# Implementation
hashing_function <- function(data, labels, signed, n) {
  counts <- list_to_hash(data, n, signed)

  colnames(counts) <- labels
  as_tibble(counts)
}

# Takes a [tokenlist] and calculate the hashed token count matrix
list_to_hash <- function(x, n, signed) {
  it <- text2vec::itoken(x, progress = FALSE)
  vectorizer <- text2vec::hash_vectorizer(hash_size = n, signed_hash = signed)
  as.matrix(text2vec::create_dtm(it, vectorizer))
}

#' S3 methods for tracking which additional packages are needed for steps.
#'
#' Recipe-adjacent packages always list themselves as a required package so that
#' the steps can function properly within parallel processing schemes.
#' @param x A recipe step
#' @return A character vector
#' @rdname required_pkgs.step
#' @keywords internal
#' @export
required_pkgs.step_texthash <- function(x, ...) {
  c("text2vec", "textrecipes")
}

#' @rdname tunable_textrecipes
#' @export
tunable.step_texthash <- function(x, ...) {
  tibble::tibble(
    name = c("signed", "num_terms"),
    call_info = list(
      list(pkg = "dials", fun = "signed_hash"),
      list(pkg = "dials", fun = "num_hash", range = c(8, 12))
    ),
    source = "recipe",
    component = "step_texthash",
    component_id = x$id
  )
}
