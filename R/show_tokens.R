#' Show token output of recipe
#' 
#' Returns the tokens as a list of character vector of a recipe. This function
#' can be useful for diagnostics doing recipe construction but should not be 
#' used in final recipe steps. Note that this function will both prep() and 
#' bake() the recipe it is used on.
#'
#' @param rec A recipe object
#' @param var name of variable 
#'
#' @return A list of character vectors
#' @export
#'
#' @examples
#' text_tibble <- tibble(text = c("This is words", "They are nice!"))
#' 
#' recipe(~ text, data = text_tibble) %>%
#'   step_tokenize(text) %>%
#'   show_tokens(text)
show_tokens <- function(rec, var) {
  rec %>%
    prep() %>%
    bake(new_data = NULL) %>%
    dplyr::pull({{var}}) %>%
    get_tokens()
}