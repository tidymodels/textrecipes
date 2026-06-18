# BPE Tokenization of Character Variables

`step_tokenize_bpe()` creates a *specification* of a recipe step that
will convert a character predictor into a
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variable using Byte Pair Encoding.

## Usage

``` r
step_tokenize_bpe(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  vocabulary_size = 1000,
  options = list(),
  res = NULL,
  skip = FALSE,
  id = rand_id("tokenize_bpe")
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

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- columns:

  A character string of variable names that will be populated
  (eventually) by the `terms` argument. This is `NULL` until the step is
  trained by
  [`recipes::prep.recipe()`](https://recipes.tidymodels.org/reference/prep.html).

- vocabulary_size:

  Integer, indicating the number of tokens in the final vocabulary.
  Defaults to 1000. Highly encouraged to be tuned.

- options:

  A list of options passed to the tokenizer.

- res:

  The fitted
  [`tokenizers.bpe::bpe()`](https://rdrr.io/pkg/tokenizers.bpe/man/bpe.html)
  model tokenizer will be stored here once this preprocessing step has
  be trained by
  [`recipes::prep.recipe()`](https://recipes.tidymodels.org/reference/prep.html).

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

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Tuning Parameters

This step has 1 tuning parameters:

- `vocabulary_size`: \# Unique Tokens in Vocabulary (type: integer,
  default: 1000)

## Case weights

The underlying operation does not allow for case weights.

## See also

[`step_untokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_untokenize.md)
to untokenize.

Other Steps for Tokenization:
[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md),
[`step_tokenize_sentencepiece()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_sentencepiece.md),
[`step_tokenize_wordpiece()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_wordpiece.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize_bpe(medium)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, new_data = NULL, medium) |>
  slice(1:2)
#> # A tibble: 2 × 1
#>       medium
#>    <tknlist>
#> 1 [8 tokens]
#> 2 [3 tokens]

bake(tate_obj, new_data = NULL) |>
  slice(2) |>
  pull(medium)
#> <textrecipes_tokenlist[1]>
#> [1] [3 tokens]
#> # Unique Tokens: 3

tidy(tate_rec, number = 1)
#> # A tibble: 1 × 2
#>   terms  id                
#>   <chr>  <chr>             
#> 1 medium tokenize_bpe_H4eEm
tidy(tate_obj, number = 1)
#> # A tibble: 1 × 2
#>   terms  id                
#>   <chr>  <chr>             
#> 1 medium tokenize_bpe_H4eEm
```
