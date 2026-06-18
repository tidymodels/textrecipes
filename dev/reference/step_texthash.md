# Feature Hashing of Tokens

`step_texthash()` creates a *specification* of a recipe step that will
convert a
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variable into multiple numeric variables using the hashing trick.

## Usage

``` r
step_texthash(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  signed = TRUE,
  num_terms = 1024L,
  prefix = "texthash",
  sparse = "auto",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("texthash")
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

- signed:

  A logical, indicating whether to use a signed hash-function to reduce
  collisions when hashing. Defaults to TRUE.

- num_terms:

  An integer, the number of variables to output. Defaults to 1024.

- prefix:

  A character string that will be the prefix to the resulting new
  variables. See notes below.

- sparse:

  A single string. Should the columns produced be sparse vectors. Can
  take the values `"yes"`, `"no"`, and `"auto"`. If `sparse = "auto"`
  then workflows can determine the best option. Defaults to `"auto"`.

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

Feature hashing, or the hashing trick, is a transformation of a text
variable into a new set of numerical variables. This is done by applying
a hashing function over the tokens and using the hash values as feature
indices. This allows for a low memory representation of the text. This
implementation is done using the MurmurHash3 method.

The argument `num_terms` controls the number of indices that the hashing
function will map to. This is the tuning parameter for this
transformation. Since the hashing function can map two different tokens
to the same index, will a higher value of `num_terms` result in a lower
chance of collision.

The new components will have names that begin with `prefix`, then the
name of the variable, followed by the tokens all separated by `-`. The
variable names are padded with zeros. For example if `prefix = "hash"`,
and if `num_terms < 10`, their names will be `hash1` - `hash9`. If
`num_terms = 101`, their names will be `hash001` - `hash101`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, value and `id`:

- terms:

  character, the selectors or variables selected

- value:

  logical, is it signed?

- length:

  integer, number of terms

- id:

  character, id of this step

## Tuning Parameters

This step has 2 tuning parameters:

- `signed`: Signed Hash Value (type: logical, default: TRUE)

- `num_terms`: \# Hash Features (type: integer, default: 1024)

## Sparse data

This step produces sparse columns if `sparse = "yes"` is being set. The
default value `"auto"` won't trigger production fo sparse columns if a
recipe is
[`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)ed,
but allows for a workflow to toggle to `"yes"` or `"no"` depending on
whether the model supports
[recipes::sparse_data](https://recipes.tidymodels.org/reference/sparse_data.html)
and if the model is is expected to run faster with the data.

The mechanism for determining how much sparsity is produced isn't
perfect, and there will be times when you want to manually overwrite by
setting `sparse = "yes"` or `sparse = "no"`.

## Case weights

The underlying operation does not allow for case weights.

## References

Kilian Weinberger; Anirban Dasgupta; John Langford; Alex Smola; Josh
Attenberg (2009).

## See also

[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
to turn characters into
[`tokens`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
[`step_text_normalization()`](https://textrecipes.tidymodels.org/dev/reference/step_text_normalization.md)
to perform text normalization.

Other Steps for Numeric Variables From Tokens:
[`step_lda()`](https://textrecipes.tidymodels.org/dev/reference/step_lda.md),
[`step_tf()`](https://textrecipes.tidymodels.org/dev/reference/step_tf.md),
[`step_tfidf()`](https://textrecipes.tidymodels.org/dev/reference/step_tfidf.md),
[`step_word_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_word_embeddings.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_tokenfilter(medium, max_tokens = 10) |>
  step_texthash(medium)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, tate_text)
#> # A tibble: 4,284 × 1,028
#>        id artist  title  year texthash_medium_0001 texthash_medium_0002
#>     <dbl> <fct>   <fct> <dbl>                <int>                <int>
#>  1  21926 Absalon Prop…  1990                    0                    0
#>  2  20472 Auerba… Mich…  1990                    0                    0
#>  3  20474 Auerba… Geof…  1990                    0                    0
#>  4  20473 Auerba… Jake   1990                    0                    0
#>  5  20513 Auerba… To t…  1990                    0                    0
#>  6  21389 Ayres,… Phaë…  1990                    0                    0
#>  7 121187 Barlow… Unti…  1990                    0                    0
#>  8  19455 Baseli… Gree…  1990                    0                    0
#>  9  20938 Beatti… Pres…  1990                    0                    0
#> 10 105941 Beuys,… Jose…  1990                    0                    0
#> # ℹ 4,274 more rows
#> # ℹ 1,022 more variables: texthash_medium_0003 <int>,
#> #   texthash_medium_0004 <int>, texthash_medium_0005 <int>,
#> #   texthash_medium_0006 <int>, texthash_medium_0007 <int>,
#> #   texthash_medium_0008 <int>, texthash_medium_0009 <int>,
#> #   texthash_medium_0010 <int>, texthash_medium_0011 <int>,
#> #   texthash_medium_0012 <int>, texthash_medium_0013 <int>, …

tidy(tate_rec, number = 3)
#> # A tibble: 1 × 4
#>   terms  value length id            
#>   <chr>  <lgl>  <int> <chr>         
#> 1 medium NA        NA texthash_NcuKD
tidy(tate_obj, number = 3)
#> # A tibble: 1 × 4
#>   terms  value length id            
#>   <chr>  <lgl>  <int> <chr>         
#> 1 medium TRUE    1024 texthash_NcuKD
```
