# Pretrained Word Embeddings of Tokens

`step_word_embeddings()` creates a *specification* of a recipe step that
will convert a
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variable into word-embedding dimensions by aggregating the vectors of
each token from a pre-trained embedding.

## Usage

``` r
step_word_embeddings(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  embeddings,
  aggregation = c("sum", "mean", "min", "max"),
  aggregation_default = 0,
  prefix = "wordembed",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("word_embeddings")
)
```

## Arguments

- recipe:

  A
  [recipes::recipe](https://recipes.tidymodels.org/reference/recipe.html)
  object. The step will be added to the sequence of operations for this
  recipe.

- ...:

  One or more selector functions to choose which variables are affected
  by the step. See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- role:

  For model terms created by this step, what analysis role should they
  be assigned?. By default, the function assumes that the new columns
  created by the original variables will be used as predictors in a
  model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- columns:

  A character string of variable names that will be populated
  (eventually) by the `terms` argument. This is `NULL` until the step is
  trained by
  [`recipes::prep.recipe()`](https://recipes.tidymodels.org/reference/prep.html).

- embeddings:

  A tibble of pre-trained word embeddings, such as those returned by the
  embedding_glove function from the textdata package. The first column
  should contain tokens, and additional columns should contain
  embeddings vectors.

- aggregation:

  A character giving the name of the aggregation function to use. Must
  be one of "sum", "mean", "min", and "max". Defaults to "sum".

- aggregation_default:

  A numeric denoting the default value for case with no words are
  matched in embedding. Defaults to 0.

- prefix:

  A character string that will be the prefix to the resulting new
  variables. See notes below.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `FALSE`.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`recipes::bake.recipe()`](https://recipes.tidymodels.org/reference/bake.html)?
  While all operations are baked when
  [`recipes::prep.recipe()`](https://recipes.tidymodels.org/reference/prep.html)
  is run, some operations may not be able to be conducted on new data
  (e.g. processing the outcome variable(s)). Care should be taken when
  using `skip = FALSE`.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of existing steps (if any).

## Details

Word embeddings map words (or other tokens) into a high-dimensional
feature space. This function maps pre-trained word embeddings onto the
tokens in your data.

The argument `embeddings` provides the pre-trained vectors. Each
dimension present in this tibble becomes a new feature column, with each
column aggregated across each row of your text using the function
supplied in the `aggregation` argument.

The new components will have names that begin with `prefix`, then the
name of the aggregation function, then the name of the variable from the
embeddings tibble (usually something like "d7"). For example, using the
default "wordembedding" prefix, and the GloVe embeddings from the
textdata package (where the column names are `d1`, `d2`, etc), new
columns would be `wordembedding_d1`, `wordembedding_d1`, etc.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `embedding_rows`,
`aggregation`, and `id`:

- terms:

  character, the selectors or variables selected

- embedding_rows:

  integer, number of rows in embedding

- aggregation:

  character,aggregation

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
to turn characters into
[`tokens`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)

Other Steps for Numeric Variables From Tokens:
[`step_lda()`](https://textrecipes.tidymodels.org/dev/reference/step_lda.md),
[`step_texthash()`](https://textrecipes.tidymodels.org/dev/reference/step_texthash.md),
[`step_tf()`](https://textrecipes.tidymodels.org/dev/reference/step_tf.md),
[`step_tfidf()`](https://textrecipes.tidymodels.org/dev/reference/step_tfidf.md)

## Examples

``` r
library(recipes)

embeddings <- tibble(
  tokens = c("the", "cat", "ran"),
  d1 = c(1, 0, 0),
  d2 = c(0, 1, 0),
  d3 = c(0, 0, 1)
)

sample_data <- tibble(
  text = c(
    "The.",
    "The cat.",
    "The cat ran."
  ),
  text_label = c("fragment", "fragment", "sentence")
)

rec <- recipe(text_label ~ ., data = sample_data) |>
  step_tokenize(text) |>
  step_word_embeddings(text, embeddings = embeddings)

obj <- rec |>
  prep()

bake(obj, sample_data)
#> # A tibble: 3 × 4
#>   text_label wordembed_text_d1 wordembed_text_d2 wordembed_text_d3
#>   <fct>                  <dbl>             <dbl>             <dbl>
#> 1 fragment                   1                 0                 0
#> 2 fragment                   1                 1                 0
#> 3 sentence                   1                 1                 1

tidy(rec, number = 2)
#> # A tibble: 1 × 4
#>   terms embeddings_rows aggregation id                   
#>   <chr>           <int> <chr>       <chr>                
#> 1 text                3 sum         word_embeddings_nOylp
tidy(obj, number = 2)
#> # A tibble: 1 × 4
#>   terms embeddings_rows aggregation id                   
#>   <chr>           <int> <chr>       <chr>                
#> 1 text                3 sum         word_embeddings_nOylp
```
