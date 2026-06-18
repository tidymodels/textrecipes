# Term frequency of Tokens

`sparse = "yes"` doesn't take effect when
`weight_scheme = "double normalization"` as it doesn't produce sparse
data.

## Usage

``` r
step_tf(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  weight_scheme = "raw count",
  weight = 0.5,
  vocabulary = NULL,
  res = NULL,
  prefix = "tf",
  sparse = "auto",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("tf")
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

- weight_scheme:

  A character determining the weighting scheme for the term frequency
  calculations. Must be one of "binary", "raw count", "term frequency",
  "log normalization" or "double normalization". Defaults to "raw
  count".

- weight:

  A numeric weight used if `weight_scheme` is set to "double
  normalization". Defaults to 0.5.

- vocabulary:

  A character vector of strings to be considered.

- res:

  The words that will be used to calculate the term frequency will be
  stored here once this preprocessing step has be trained by
  [`recipes::prep.recipe()`](https://recipes.tidymodels.org/reference/prep.html).

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

`step_tf()` creates a *specification* of a recipe step that will convert
a
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variable into multiple variables containing the token counts.

It is strongly advised to use
[step_tokenfilter](https://textrecipes.tidymodels.org/dev/reference/step_tokenfilter.md)
before using step_tf to limit the number of variables created, otherwise
you might run into memory issues. A good strategy is to start with a low
token count and go up according to how much RAM you want to use.

Term frequency is a weight of how many times each token appears in each
observation. There are different ways to calculate the weight and this
step can do it in a couple of ways. Setting the argument `weight_scheme`
to "binary" will result in a set of binary variables denoting if a token
is present in the observation. "raw count" will count the times a token
is present in the observation. "term frequency" will divide the count by
the total number of words in the document to limit the effect of the
document length as longer documents tends to have the word present more
times but not necessarily at a higher percentage. "log normalization"
takes the log of 1 plus the count, adding 1 is done to avoid taking log
of 0. Finally "double normalization" is the raw frequency divided by the
raw frequency of the most occurring term in the document. This is then
multiplied by `weight` and `weight` is added to the result. This is
again done to prevent a bias towards longer documents.

The new components will have names that begin with `prefix`, then the
name of the variable, followed by the tokens all separated by `-`. The
variable names are padded with zeros. For example if `prefix = "hash"`,
and if `num_terms < 10`, their names will be `hash1` - `hash9`. If
`num_terms = 101`, their names will be `hash001` - `hash101`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `value`, and `id`:

- terms:

  character, the selectors or variables selected

- value:

  character, the weighting scheme

- id:

  character, id of this step

## Tuning Parameters

This step has 2 tuning parameters:

- `weight_scheme`: Term Frequency Weight Method (type: character,
  default: raw count)

- `weight`: Weight (type: double, default: 0.5)

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

## See also

[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
to turn characters into
[`tokens`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)

Other Steps for Numeric Variables From Tokens:
[`step_lda()`](https://textrecipes.tidymodels.org/dev/reference/step_lda.md),
[`step_texthash()`](https://textrecipes.tidymodels.org/dev/reference/step_texthash.md),
[`step_tfidf()`](https://textrecipes.tidymodels.org/dev/reference/step_tfidf.md),
[`step_word_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_word_embeddings.md)

## Examples

``` r
# \donttest{
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_tf(medium)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, tate_text)
#> # A tibble: 4,284 × 956
#>        id artist     title  year tf_medium_1 tf_medium_10 tf_medium_100
#>     <dbl> <fct>      <fct> <dbl>       <int>        <int>         <int>
#>  1  21926 Absalon    Prop…  1990           0            0             0
#>  2  20472 Auerbach,… Mich…  1990           0            0             0
#>  3  20474 Auerbach,… Geof…  1990           0            0             0
#>  4  20473 Auerbach,… Jake   1990           0            0             0
#>  5  20513 Auerbach,… To t…  1990           0            0             0
#>  6  21389 Ayres, OB… Phaë…  1990           0            0             0
#>  7 121187 Barlow, P… Unti…  1990           0            0             0
#>  8  19455 Baselitz,… Gree…  1990           0            0             0
#>  9  20938 Beattie, … Pres…  1990           0            0             0
#> 10 105941 Beuys, Jo… Jose…  1990           0            0             0
#> # ℹ 4,274 more rows
#> # ℹ 949 more variables: tf_medium_11 <int>, tf_medium_12 <int>,
#> #   tf_medium_13 <int>, tf_medium_133 <int>, tf_medium_14 <int>,
#> #   tf_medium_15 <int>, tf_medium_151 <int>, tf_medium_16 <int>,
#> #   tf_medium_160 <int>, tf_medium_16mm <int>, tf_medium_18 <int>,
#> #   tf_medium_19 <int>, tf_medium_2 <int>, tf_medium_20 <int>,
#> #   tf_medium_2000 <int>, tf_medium_201 <int>, tf_medium_21 <int>, …

tidy(tate_rec, number = 2)
#> # A tibble: 1 × 3
#>   terms  value id      
#>   <chr>  <chr> <chr>   
#> 1 medium NA    tf_XZBzg
tidy(tate_obj, number = 2)
#> # A tibble: 1 × 3
#>   terms  value     id      
#>   <chr>  <chr>     <chr>   
#> 1 medium raw count tf_XZBzg
# }
```
