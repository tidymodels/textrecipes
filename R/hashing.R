#' Term frequency of tokens
#'
#' `step_hashing` creates a *specification* of a recipe step that
#'  will convert a list of its tokenized parts into multiple 
#'  variables using the hashing trick.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_hashing`, this indicates the variables to be encoded
#'  into a list column. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param algorithm A character determining the hashing algorithm. 
#'  Must be one of "md5", "sha1", "crc32", "sha256", "sha512",
#'  "xxhash32", "xxhash64" or "murmur32". Defaults to "murmur32".
#' @param num An integer, the number of variables to output.
#'  Defaults to 1024.
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. See notes below
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake.recipe()]? While all
#'  operations are baked when [recipes::prep.recipe()] is run, some
#'  operations may not be able to be conducted on new data (e.g.
#'  processing the outcome variable(s)). Care should be taken when
#'  using `skip = TRUE` as it may affect the computations for
#'  subsequent operations.
#' @param trained A logical to indicate if the recipe has been
#'  baked.
#' @return An updated version of `recipe` with the new step added
#'  to the sequence of existing steps (if any).
#' @examples
#' library(recipes)
#' 
#' data(okc_text)
#' 
#' okc_rec <- recipe(~ ., data = okc_text) %>%
#'   step_tokenize(essay0) %>%
#'   step_textfilter(essay0, max.words = 100) %>%
#'   step_hashing(essay0) %>%
#'   prep(training = okc_text, retain = TRUE)
#'   
#' bake(okc_rec, okc_text)
#' @keywords datagen 
#' @concept preprocessing encoding
#' @export
#' @details
#' The new components will have names that begin with `prefix`, then
#' the name of the variable, followed by the tokens all seperated by
#' `-`. The variable names are padded with zeros. For example,
#' if `num < 10`, their names will be `PC1` - `PC9`.
#' If `num = 101`, the names would be `PC001` - `PC101`.
#' 
#' @importFrom recipes add_step step terms_select sel2char ellipse_check 
#' @importFrom recipes check_type
step_hashing <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           algorithm = "murmur32",
           num = 1024,
           prefix = "hashing",
           skip = FALSE
  ) {
    add_step(
      recipe,
      step_hashing_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        algorithm = algorithm,
        num = num,
        prefix = prefix,
        skip = skip
      )
    )
  }

step_hashing_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           columns = NULL,
           algorithm = NULL,
           num = NULL,
           prefix = "hashing",
           skip = FALSE) {
    step(
      subclass = "hashing",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      algorithm = algorithm,
      num = num,
      prefix = prefix,
      skip = skip
    )
  }

#' @export
prep.step_hashing <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  check_list(training[, col_names])
  
  step_hashing_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    algorithm = x$algorithm,
    num = x$num,
    prefix = x$prefix,
    skip = x$skip
  )
}

#' @export
#' @importFrom tibble as_tibble tibble
#' @importFrom recipes bake prep names0
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
bake.step_hashing <- function(object, newdata, ...) {
  col_names <- object$columns
  # for backward compat
  
  for (i in seq_along(col_names)) {
    
    tf_text <- hashing_function(newdata[, col_names[i], drop = TRUE],
                                paste0(col_names[i], "-",  
                                       names0(object$num, object$prefix)),
                                object$algorithm,
                                object$num)
    
    newdata <- bind_cols(newdata, tf_text)
    
    newdata <-
      newdata[, !(colnames(newdata) %in% col_names[i]), drop = FALSE]
  }
  
  as_tibble(newdata)
}

hashing_function <- function(data, labels, algo, n) {
  
  counts <- char_to_matrix(data, algo, n)

  colnames(counts) <- labels
  as_tibble(counts)
}

#' @importFrom digest digest
#' @importFrom stringr str_sub
char_to_int <- function(x, algo) {
  hashed <- digest(x, algo)
  strtoi(str_sub(hashed, -3, -1), 16)
}

char_to_vec <- function(x, algo, n) {
  purrr::map_int(x, ~ char_to_int(.x, algo) %% as.integer(n) + 1L)
}


char_to_matrix <- function(x, algo, n) {
  list_out <- lapply(x, char_to_vec, algo, n) 

  purrr::map(list_out, ~ tabulate(.x, n)) %>%
    unlist() %>%
    matrix(ncol = n, byrow = TRUE)
}