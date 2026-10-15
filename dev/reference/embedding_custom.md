# Embedding Engines for `step_sentence_embeddings()`

These functions create the `engine` object passed to
[`step_sentence_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_sentence_embeddings.md).
An engine describes how character vectors are turned into numeric
embeddings. Engines only hold a reference to the model (a function, or
later a model name and server settings), never the model itself, so a
prepped recipe stays small.

## Usage

``` r
embedding_custom(fun)
```

## Arguments

- fun:

  A function that takes a character vector and returns a numeric matrix
  with one row per element and one column per embedding dimension. A
  formula such as `~ my_embedder(.x)` is converted to a function with
  [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html).

## Value

An object of class `sentence_embedding_engine`.

## Details

`embedding_custom()` is the escape hatch for any backend that doesn't
have a dedicated engine. The function is called with the unique,
non-missing strings that haven't been embedded yet in the current R
session, and must return a numeric matrix with exactly one row per
string.

The function is stored in the recipe. If it captures a live external
object, such as a Python model through reticulate, that object will not
survive [`saveRDS()`](https://rdrr.io/r/base/readRDS.html). Have the
function load its model lazily, from a path or an identifier, instead of
capturing it.

## See also

[`step_sentence_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_sentence_embeddings.md)

Other Embedding Engines:
[`embedding_ollama()`](https://textrecipes.tidymodels.org/dev/reference/embedding_ollama.md)

## Examples

``` r
# A toy embedder that returns the length and number of vowels of each string
toy_embedder <- function(x) {
  cbind(
    nchar = nchar(x),
    vowels = nchar(gsub("[^aeiou]", "", x))
  )
}

embedding_custom(toy_embedder)
#> <custom function>
```
