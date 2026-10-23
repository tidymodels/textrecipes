# Sentence Embeddings of Text

`step_sentence_embeddings()` creates a *specification* of a recipe step
that will convert a character variable into dense embedding dimensions
by running each string through an embedding model.

## Usage

``` r
step_sentence_embeddings(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  engine,
  prefix = "sentembed",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("sentence_embeddings")
)
```

## Arguments

- recipe:

  A
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
  object. The step will be added to the sequence of operations for this
  recipe.

- ...:

  One or more selector functions to choose which variables are affected
  by the step. See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- columns:

  A character vector of the selected variable names. This is `NULL`
  until the step is trained by
  [`recipes::prep.recipe()`](https://recipes.tidymodels.org/reference/prep.html),
  and should not be set by hand.

- engine:

  An engine object created by one of the embedding engine functions,
  such as
  [`embedding_custom()`](https://textrecipes.tidymodels.org/dev/reference/embedding_custom.md).
  The engine defines how strings are turned into vectors and holds the
  settings needed to do so.

- prefix:

  A single string used as the prefix of the new columns. See Details for
  how the column names are built. Defaults to `"sentembed"`.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `FALSE`.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)?
  While all operations are baked when
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is run, some operations may not be able to be conducted on new data
  (e.g. processing the outcome variable(s)). Care should be taken when
  using `skip = TRUE`, as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of existing steps (if any).

## Details

Unlike
[`step_word_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_word_embeddings.md),
which looks up and aggregates the vectors of tokens, this step takes a
character (or factor) column directly and returns one dense vector per
row, as produced by the embedding model. It doesn't use
[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md).

The new columns have names that begin with `prefix`, then the name of
the variable being embedded, then the position of the dimension. For
example, using the default prefix and a variable named `text`, the new
columns are `sentembed_text_1`, `sentembed_text_2`, etc.

## What is stored in the recipe

Embedding models are large, so the step never stores the model. It
stores the engine, which is a reference to the model, together with the
number of dimensions seen when the recipe was prepped. This keeps
prepped recipes small, but it also means a prepped recipe can only be
baked where the engine can run. A recipe that is moved to another
machine needs whatever the engine depends on to be available there.

If the engine returns a different number of dimensions when baking than
it did when prepping,
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) errors
instead of returning columns the downstream model wasn't trained on.

## Missing values and caching

Missing values stay missing: a row with `NA` text gets `NA` in all of
its embedding columns, and is never sent to the engine. Empty strings
are sent to the engine as they are.

Embedding is usually the slowest part of a recipe, and
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) embeds
new data every time it is called. Within an R session, embeddings are
cached per engine and string, so repeated strings, and the training data
that is embedded both by
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) and by
[`bake()`](https://recipes.tidymodels.org/reference/bake.html), are only
embedded once. The cache lives outside the recipe and is cleared when
the R session ends.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `engine`, `model`,
`revision`, `dimension`, and `id`:

- terms:

  character, the selectors or variables selected

- engine:

  character, the name of the engine

- model:

  character, the name of the model, `NA` if the engine doesn't have one

- revision:

  character, the version of the model the engine is pinned to, `NA` if
  the engine can't pin one

- dimension:

  integer, the number of embedding dimensions, `NA` until the recipe is
  prepped

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

[`embedding_custom()`](https://textrecipes.tidymodels.org/dev/reference/embedding_custom.md)
for the available engines, and
[`step_word_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_word_embeddings.md)
for embeddings that are aggregated from tokens.

Other Steps for Numeric Variables From Characters:
[`step_dummy_hash()`](https://textrecipes.tidymodels.org/dev/reference/step_dummy_hash.md),
[`step_sequence_onehot()`](https://textrecipes.tidymodels.org/dev/reference/step_sequence_onehot.md),
[`step_textfeature()`](https://textrecipes.tidymodels.org/dev/reference/step_textfeature.md)

## Examples

``` r
library(recipes)

# A toy embedder, replace it with a call to a real embedding model
toy_embedder <- function(x) {
  cbind(
    nchar = nchar(x),
    vowels = nchar(gsub("[^aeiou]", "", x))
  )
}

sample_data <- tibble(
  text = c("The cat.", "The cat ran.", "A longer sentence here."),
  text_label = c("short", "short", "long")
)

rec <- recipe(text_label ~ ., data = sample_data) |>
  step_sentence_embeddings(text, engine = embedding_custom(toy_embedder))

obj <- rec |>
  prep()

bake(obj, new_data = NULL)
#> # A tibble: 3 × 3
#>   text_label sentembed_text_1 sentembed_text_2
#>   <fct>                 <dbl>            <dbl>
#> 1 short                     8                2
#> 2 short                    12                3
#> 3 long                     23                7

tidy(rec, number = 1)
#> # A tibble: 1 × 6
#>   terms engine          model revision dimension id                    
#>   <chr> <chr>           <chr> <chr>        <int> <chr>                 
#> 1 text  custom function NA    NA              NA sentence_embeddings_I…
tidy(obj, number = 1)
#> # A tibble: 1 × 6
#>   terms engine          model revision dimension id                    
#>   <chr> <chr>           <chr> <chr>        <int> <chr>                 
#> 1 text  custom function NA    NA               2 sentence_embeddings_I…
```
