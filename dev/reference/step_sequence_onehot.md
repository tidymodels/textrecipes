# Positional One-Hot encoding of Tokens

`step_sequence_onehot()` creates a *specification* of a recipe step that
will take a string and do one hot encoding for each character by
position.

## Usage

``` r
step_sequence_onehot(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  sequence_length = 100,
  padding = "pre",
  truncating = "pre",
  vocabulary = NULL,
  prefix = "seq1hot",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("sequence_onehot")
)
```

## Source

<https://papers.nips.cc/paper/5782-character-level-convolutional-networks-for-text-classification.pdf>

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

- sequence_length:

  A numeric, number of characters to keep before discarding. Defaults to
  100.

- padding:

  'pre' or 'post', pad either before or after each sequence. defaults to
  'pre'.

- truncating:

  'pre' or 'post', remove values from sequences larger than
  sequence_length either in the beginning or in the end of the sequence.
  Defaults too 'pre'.

- vocabulary:

  A character vector, characters to be mapped to integers. Characters
  not in the vocabulary will be encoded as 0. Defaults to `letters`.

- prefix:

  A prefix for generated column names, defaults to "seq1hot".

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

The string will be capped by the sequence_length argument, strings
shorter then sequence_length will be padded with empty characters. The
encoding will assign an integer to each character in the vocabulary, and
will encode accordingly. Characters not in the vocabulary will be
encoded as 0.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `vocabulary`,
`token`, and `id`:

- terms:

  character, the selectors or variables selected

- vocabulary:

  integer, index

- token:

  character, text corresponding to the index

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

Other Steps for Numeric Variables From Characters:
[`step_dummy_hash()`](https://textrecipes.tidymodels.org/dev/reference/step_dummy_hash.md),
[`step_textfeature()`](https://textrecipes.tidymodels.org/dev/reference/step_textfeature.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~medium, data = tate_text) |>
  step_tokenize(medium) |>
  step_tokenfilter(medium) |>
  step_sequence_onehot(medium)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, new_data = NULL)
#> # A tibble: 4,284 × 100
#>    seq1hot_medium_1 seq1hot_medium_2 seq1hot_medium_3 seq1hot_medium_4
#>               <int>            <int>            <int>            <int>
#>  1                0                0                0                0
#>  2                0                0                0                0
#>  3                0                0                0                0
#>  4                0                0                0                0
#>  5                0                0                0                0
#>  6                0                0                0                0
#>  7                0                0                0                0
#>  8                0                0                0                0
#>  9                0                0                0                0
#> 10                0                0                0                0
#> # ℹ 4,274 more rows
#> # ℹ 96 more variables: seq1hot_medium_5 <int>, seq1hot_medium_6 <int>,
#> #   seq1hot_medium_7 <int>, seq1hot_medium_8 <int>,
#> #   seq1hot_medium_9 <int>, seq1hot_medium_10 <int>,
#> #   seq1hot_medium_11 <int>, seq1hot_medium_12 <int>,
#> #   seq1hot_medium_13 <int>, seq1hot_medium_14 <int>,
#> #   seq1hot_medium_15 <int>, seq1hot_medium_16 <int>, …

tidy(tate_rec, number = 3)
#> # A tibble: 1 × 4
#>   terms  vocabulary token id                   
#>   <chr>  <chr>      <int> <chr>                
#> 1 medium NA            NA sequence_onehot_dw9eV
tidy(tate_obj, number = 3)
#> # A tibble: 100 × 4
#>    terms  vocabulary token     id                   
#>    <chr>       <int> <chr>     <chr>                
#>  1 medium          1 16        sequence_onehot_dw9eV
#>  2 medium          2 2         sequence_onehot_dw9eV
#>  3 medium          3 3         sequence_onehot_dw9eV
#>  4 medium          4 35        sequence_onehot_dw9eV
#>  5 medium          5 4         sequence_onehot_dw9eV
#>  6 medium          6 5         sequence_onehot_dw9eV
#>  7 medium          7 6         sequence_onehot_dw9eV
#>  8 medium          8 8         sequence_onehot_dw9eV
#>  9 medium          9 acrylic   sequence_onehot_dw9eV
#> 10 medium         10 aluminium sequence_onehot_dw9eV
#> # ℹ 90 more rows
```
