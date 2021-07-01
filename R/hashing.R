#' Term frequency of tokens
#'
#' `step_texthash` creates a *specification* of a recipe step that
#'  will convert a [tokenlist] into multiple variables using the
#'  hashing trick.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param signed A logical, indicating whether to use a signed
#' hash-function to reduce collisions when hashing. Defaults to TRUE.
#' @param num_terms An integer, the number of variables to output.
#'  Defaults to 1024.
#' @template args-prefix
#' @template args-skip
#' @template args-id
#' 
#' @template returns
#' 
#' @details
#'  Feature hashing, or the hashing trick, is a transformation of a
#'  text variable into a new set of numerical variables. This is done by
#'  applying a hashing function over the tokens and using the hash values
#'  as feature indices. This allows for a low memory representation of the
#'  text. This implementation is done using the MurmurHash3 method.
#' 
#'  The argument `num_terms` controls the number of indices that the hashing
#'  function will map to. This is the tuning parameter for this
#'  transformation. Since the hashing function can map two different tokens
#'  to the same index, will a higher value of `num_terms` result in a lower
#'  chance of collision.
#'  
#' @template details-prefix
#' 
#' @references Kilian Weinberger; Anirban Dasgupta; John Langford;
#'  Alex Smola; Josh Attenberg (2009).
#'  
#' @seealso [step_tokenize()] to turn character into tokenlist.
#' @family tokenlist to numeric steps
#' 
#' @examples
#' if (requireNamespace("text2vec", quietly = TRUE)) {
#'   library(recipes)
#'   library(modeldata)
#'   data(okc_text)
#'
#'   okc_rec <- recipe(~., data = okc_text) %>%
#'     step_tokenize(essay0) %>%
#'     step_tokenfilter(essay0, max_tokens = 10) %>%
#'     step_texthash(essay0)
#'
#'   okc_obj <- okc_rec %>%
#'     prep()
#'
#'   bake(okc_obj, okc_text)
#'
#'   tidy(okc_rec, number = 2)
#'   tidy(okc_obj, number = 2)
#' }
#' 
#' @export
step_texthash <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           columns = NULL,
           signed = TRUE,
           num_terms = 1024,
           prefix = "hash",
           skip = FALSE,
           id = rand_id("texthash")) {
    recipes::recipes_pkg_check(required_pkgs.step_texthash())

    add_step(
      recipe,
      step_texthash_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        signed = signed,
        num_terms = num_terms,
        prefix = prefix,
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
  function(terms, role, trained, columns, signed, num_terms, prefix, skip,
           id) {
    step(
      subclass = "texthash",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      signed = signed,
      num_terms = num_terms,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_texthash <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  check_list(training[, col_names])

  step_texthash_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    signed = x$signed,
    num_terms = x$num_terms,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_texthash <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (i in seq_along(col_names)) {
    tf_text <- hashing_function(
      get_tokens(new_data[, col_names[i], drop = TRUE]),
      paste0(
        col_names[i], "_",
        names0(object$num_terms, object$prefix)
      ),
      object$signed,
      object$num_terms
    )

    new_data <-
      new_data[, !(colnames(new_data) %in% col_names[i]), drop = FALSE]

    new_data <- vctrs::vec_cbind(tf_text, new_data)
  }

  as_tibble(new_data)
}

#' @export
print.step_texthash <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Feature hashing with ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_texthash
#' @param x A `step_texthash` object.
#' @export
tidy.step_texthash <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = x$terms,
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

#' Find recommended methods for generating parameter values
#'
#' [tunable()] determines which parameters in an object _can_ be tuned along
#' with information about the parameters.
#' @param x A recipe step
#' @param ... Not currently used.
#' @rdname tunable.step
#' @keywords internal
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
