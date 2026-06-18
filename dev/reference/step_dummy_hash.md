# Indicator Variables via Feature Hashing

`step_dummy_hash()` creates a *specification* of a recipe step that will
convert factors or character columns into a series of binary (or signed
binary) indicator columns.

## Usage

``` r
step_dummy_hash(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  signed = TRUE,
  num_terms = 32L,
  collapse = FALSE,
  prefix = "dummyhash",
  sparse = "auto",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("dummy_hash")
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

  A logical, indicating whether to use a signed hash-function
  (generating values of -1, 0, or 1), to reduce collisions when hashing.
  Defaults to TRUE.

- num_terms:

  An integer, the number of variables to output. Defaults to 32.

- collapse:

  A logical; should all of the selected columns be collapsed into a
  single column to create a single set of hashed features?

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
a hashing function over the values of the factor levels and using the
hash values as feature indices. This allows for a low memory
representation of the data and can be very helpful when a qualitative
predictor has many levels or is expected to have new levels during
prediction. This implementation is done using the MurmurHash3 method.

The argument `num_terms` controls the number of indices that the hashing
function will map to. This is the tuning parameter for this
transformation. Since the hashing function can map two different tokens
to the same index, a higher value of `num_terms` will result in a lower
chance of collision.

The new components will have names that begin with `prefix`, then the
name of the variable, followed by the tokens all separated by `-`. The
variable names are padded with zeros. For example if `prefix = "hash"`,
and if `num_terms < 10`, their names will be `hash1` - `hash9`. If
`num_terms = 101`, their names will be `hash001` - `hash101`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `value`,
`num_terms`, `collapse`, and `id`:

- terms:

  character, the selectors or variables selected

- value:

  logical, whether a signed hashing was performed

- num_terms:

  integer, number of terms

- collapse:

  logical, were the columns collapsed

- id:

  character, id of this step

## Tuning Parameters

This step has 2 tuning parameters:

- `signed`: Signed Hash Value (type: logical, default: TRUE)

- `num_terms`: \# Hash Features (type: integer, default: 32)

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

Kuhn and Johnson (2019), Chapter 7,
<https://bookdown.org/max/FES/encoding-predictors-with-many-categories.html>

## See also

[`recipes::step_dummy()`](https://recipes.tidymodels.org/reference/step_dummy.html)

Other Steps for Numeric Variables From Characters:
[`step_sequence_onehot()`](https://textrecipes.tidymodels.org/dev/reference/step_sequence_onehot.md),
[`step_textfeature()`](https://textrecipes.tidymodels.org/dev/reference/step_textfeature.md)

## Examples

``` r
#> 
#> Attaching package: ‘data.table’
#> The following objects are masked from ‘package:dplyr’:
#> 
#>     between, first, last
#> The following object is masked from ‘package:base’:
#> 
#>     %notin%

library(recipes)
library(modeldata)
data(grants)

grants_rec <- recipe(~sponsor_code, data = grants_other) |>
  step_dummy_hash(sponsor_code)

grants_obj <- grants_rec |>
  prep()

bake(grants_obj, grants_test)
#> # A tibble: 518 × 32
#>    dummyhash_sponsor_co…¹ dummyhash_sponsor_co…² dummyhash_sponsor_co…³
#>                     <int>                  <int>                  <int>
#>  1                      0                      0                      0
#>  2                      0                      0                      0
#>  3                      0                      0                      0
#>  4                      0                      1                      0
#>  5                      0                      0                      0
#>  6                      0                      1                      0
#>  7                      0                      0                      0
#>  8                      0                      0                      0
#>  9                      0                      0                      0
#> 10                      0                      1                      0
#> # ℹ 508 more rows
#> # ℹ abbreviated names: ¹​dummyhash_sponsor_code_01,
#> #   ²​dummyhash_sponsor_code_02, ³​dummyhash_sponsor_code_03
#> # ℹ 29 more variables: dummyhash_sponsor_code_04 <int>,
#> #   dummyhash_sponsor_code_05 <int>, dummyhash_sponsor_code_06 <int>,
#> #   dummyhash_sponsor_code_07 <int>, dummyhash_sponsor_code_08 <int>,
#> #   dummyhash_sponsor_code_09 <int>, …

tidy(grants_rec, number = 1)
#> # A tibble: 1 × 5
#>   terms        value num_terms collapse id              
#>   <chr>        <lgl>     <int> <lgl>    <chr>           
#> 1 sponsor_code NA           NA NA       dummy_hash_9QeuR
tidy(grants_obj, number = 1)
#> # A tibble: 1 × 5
#>   terms        value num_terms collapse id              
#>   <chr>        <lgl>     <int> <lgl>    <chr>           
#> 1 sponsor_code TRUE         32 FALSE    dummy_hash_9QeuR
```
