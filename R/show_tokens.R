#' Show token output of recipe
#'
#' Returns the tokens as a list of character vectors of a recipe. This function
#' can be useful for diagnostics during recipe construction but should not be
#' used in final recipe steps. Note that this function will both prep() and
#' bake() the recipe it is used on.
#'
#' @param rec A recipe object
#' @param var name of variable
#' @param n Number of elements to return.
#'
#' @return A list of character vectors
#' @export
#'
#' @examplesIf rlang::is_installed("modeldata")
#' text_tibble <- tibble(text = c("This is words", "They are nice!"))
#'
#' recipe(~text, data = text_tibble) %>%
#'   step_tokenize(text) %>%
#'   show_tokens(text)
#'
#' library(modeldata)
#' data(tate_text)
#'
#' recipe(~., data = tate_text) %>%
#'   step_tokenize(medium) %>%
#'   show_tokens(medium)
show_tokens <- function(rec, var, n = 6L) {
  check_number_whole(n, min = 0, nrow(rec$template))
  
  res <- rec %>%
    prep() %>%
    bake(new_data = NULL) %>%
    dplyr::pull({{ var }}) %>%
    get_tokens()

  res[seq_len(min(length(res), n))]
}
