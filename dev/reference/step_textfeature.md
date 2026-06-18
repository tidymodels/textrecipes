# Calculate Set of Text Features

`step_textfeature()` creates a *specification* of a recipe step that
will extract a number of numeric features of a text column.

## Usage

``` r
step_textfeature(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  extract_functions = count_functions,
  prefix = "textfeature",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("textfeature")
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

- extract_functions:

  A named list of feature extracting functions. Defaults to
  `count_functions`. See details for more information.

- prefix:

  A prefix for generated column names, defaults to "textfeature".

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

This step will take a character column and returns a number of numeric
columns equal to the number of functions in the list passed to the
`extract_functions` argument.

All the functions passed to `extract_functions` must take a character
vector as input and return a numeric vector of the same length,
otherwise an error will be thrown.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `functions`, and
`id`:

- terms:

  character, the selectors or variables selected

- functions:

  character, name of feature functions

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

Other Steps for Numeric Variables From Characters:
[`step_dummy_hash()`](https://textrecipes.tidymodels.org/dev/reference/step_dummy_hash.md),
[`step_sequence_onehot()`](https://textrecipes.tidymodels.org/dev/reference/step_sequence_onehot.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_textfeature(medium)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, new_data = NULL) |>
  slice(1:2)
#> # A tibble: 2 × 30
#>      id artist          title               year textfeature_medium_n…¹
#>   <dbl> <fct>           <fct>              <dbl>                  <int>
#> 1 21926 Absalon         Proposals for a H…  1990                      8
#> 2 20472 Auerbach, Frank Michael             1990                      3
#> # ℹ abbreviated name: ¹​textfeature_medium_n_words
#> # ℹ 25 more variables: textfeature_medium_n_uq_words <int>,
#> #   textfeature_medium_n_charS <int>,
#> #   textfeature_medium_n_uq_charS <int>,
#> #   textfeature_medium_n_digits <int>,
#> #   textfeature_medium_n_hashtags <int>,
#> #   textfeature_medium_n_uq_hashtags <int>, …

bake(tate_obj, new_data = NULL) |>
  pull(textfeature_medium_n_words)
#>    [1]  8  3  3  3  4  4  4  3  6  3  3  3  3  3  3  3  6  7 10  4  4
#>   [22]  3  3  3  3  3  3  3  3  5  8  4  4  3  3  3  3  3  3  1  5  4
#>   [43]  1 10  3  3  3  3  3  3  3  3  3  5 10  5  6  7  4  4  4  4  7
#>   [64]  3  3  3  3  4  3  6  3  3  3  5 11  3  3  4 18  3  8  6 10 13
#>   [85]  5  3  4  3  3  3  3  3  3  3  3  4  3  3  3  3  5  3  5  3  3
#>  [106]  3  4  5  6  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#>  [127]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#>  [148]  3  3  3  3  3  3  3  3  3  3  3  3  6  3  6  6  1  5  8  1  4
#>  [169]  6  6  6  4  8  5  3  3  3  3  7  3  3  3  3  6  3  3  6  4  8
#>  [190] 11  4 11  3  3  4  3  6  6  3  3  6  3  6  8  7  5  6  3  6  5
#>  [211]  5  3  5  5  3  3  3  3  3  3  7  7  7  7  7  7  7  7  5  5  3
#>  [232]  3  3  3  3  3  3  3  3  3 19  6  3  3  3  6  3  3  3  3  3  3
#>  [253]  3  3  3  3  3  3  3  3  3  3  4  3  4  3  3  3  3  3  3  3  3
#>  [274]  3  3  3  3  3  3  3  3  3  3  3  5  5  5  5  5  3  6  7  7  6
#>  [295]  6  4  4  4  4  4  4  4  4  6  6 10  3  3  4  4  7  3  3  6  1
#>  [316] 14 14 11  9  6  9  4 11  4 14  3  6  6  4  4  4  4  6 12  4  4
#>  [337]  8  9 11  5  3  3  3  3  3  3  3  3  3  3  3  3  4  5  3  4 13
#>  [358] 11  3  3  3  3  3  3  3  3  1  1  3  3  3  8  6  5  9  6  5  5
#>  [379]  5  5  7  3  3  3  3  3  2  7  7  7  4  4  3  5  5  5  5  5  5
#>  [400]  6  6  6  6  6  5  6  7 10  6  2  8  3  5  4  2 17 18  4  3  3
#>  [421]  3 10  3  4  3  3 11  7  5  7  3  5  7  5  5  7  5  5  5  7  3
#>  [442]  5  5  7  5  8  5  5  5  5  6  8  8  9  6  6  6  6  4  3  6  6
#>  [463]  4  4  1  3  3  3  3  3  3  3  3  3  3  3  1  9  3  3  4  5  6
#>  [484]  6  8 10  3  6  3  3  3  3  3  3  3  3  3  3  3  3  4  3  8 15
#>  [505] 13 18 12 12  3  6  6  6 11 10  3 13  5  4  3  3 10  9 14  4  8
#>  [526]  6  4  3  4  4  4  3 14  8  3  3  3  3  3  8  3  3  3  3  3  3
#>  [547]  3  3  3  3  4  4  3  3  3  3  3  3  3  3  3  3 12  6  3  6  6
#>  [568]  1  4  7  7  3  5  3  7  4  9  3  3  3  3  3  3  3  3  4  4  3
#>  [589]  3  3  3  3  3  3  6  6  6  6  5  8  3  2 10  3  5  4  3  3  3
#>  [610]  6 17  4  1  3  3  3  3  3  3  3  3  3  4  6  8  9  6  6  6  5
#>  [631]  3  4  3  5  7  6  6  6  6  6  5  2  7  6  7  7  3  3  3  3  3
#>  [652]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#>  [673]  3  3  3  3  3  4  7  8  7  6  6  4  5  3  3  3  3  3  3  3  3
#>  [694]  3  3  3  3  3  3  3  3  3  3  3  3  3  4  7  5  5  5  7 10 11
#>  [715]  9  4  4  7  3  3  3  3  3  3  8  6 17 10 15  3  3  3  3  7  3
#>  [736] 12  2  4  6  8 11  4  7 11 13  4  4  4  3  3  3  3  4  6  8  6
#>  [757]  1  5  7  7  7  8 11 11 11  5  5  5  3  3  3  3  3  4  3  3  4
#>  [778]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  4 10  8  8  8
#>  [799]  8  8  8  8  8  8  8  8  8  8  6 14  5  4  3  3  3  3  3  3  1
#>  [820]  9  4  3  6 11 10 11  4  3  6  3  5  8  8  7  6  6  6  6  6  6
#>  [841]  6  7  7  7  9  6  9  8  3  3  3  6  3  3  3  2  9  4  4  5  3
#>  [862]  3  3  3  3  3  3  3  3  3  8  7  3  3  3  3  3  5  5  3  3  3
#>  [883]  5  5  5  5  3  3  3 11  3  3  3  3  3  4  6  3  7  1  1  7  4
#>  [904]  5  4  4  4  5  3  3 13  3  4 10  3  3  3  3  3  5  8  3  6 11
#>  [925]  7  1  4  3  3  5  4 10  4  3  8  3  8  8  4  4 13  3  7  5  6
#>  [946]  3  3  3  4  6  7  3  3  3  3  3  3  3  3  3  3  3  3  6  5  1
#>  [967]  5  3  3  3  4  6  4  6  5  5  7  5  3  3  3  3  3  3  3  3  3
#>  [988]  3  3  3  3  3  3  3  7  2  3  6  4  7  6  5  7  6  2  5  5  5
#> [1009]  5  5  4  9  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [1030]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  4  3
#> [1051]  3  3  3  3  3  3  3  8  5  5  9  9 11 11 11  5  5  5  5  5  5
#> [1072]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  9  5  5  5 11
#> [1093]  5  5  5  5 22  6  6  4  4  5  5  1  4  3  3  3  3  3  3  3  3
#> [1114]  3  3  3  5  3  3  5  3  3  5  3  3  5  3  5  3  3  3  3  3  3
#> [1135]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [1156]  3  5  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  4  1 14  7
#> [1177]  2  9  5 11  3  3  3 12  3  7  3  4  3  6  3  5  4  4  4  4  4
#> [1198]  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4
#> [1219]  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  3
#> [1240]  3  3  3  3  3  3  3  3  3  3  5  6  6  4  8  8  8  8  6  3  4
#> [1261]  9  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [1282]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [1303]  3  3  3  3  3  3  3  4  4  4  4  4  4  4  4 11  4  3  3  3  5
#> [1324]  5  7  4  4  3  3  3  3  3  3  3  5  5  9  6  5  5  5  5  5  5
#> [1345]  5  5  7  6  4  4  7  4  4  7  5  9  9  9  6  6  5  5  6  5  6
#> [1366]  6  6  5  6  4  6  6  6  6  4  5  5  8  7  4  5 15  4  3  3  3
#> [1387]  3  3  3  3  3  5  3  5  5  5  5  4  4  7 16  4  7  5  5  5  5
#> [1408]  5  5  5  5  5  5  3  3  3  3  3  3  8  4  5  5  3  3  3  5  4
#> [1429]  5  4  8  1  4  4  4 13  9  3  3  3  3  3  3  3  6  5  3  5  3
#> [1450]  5  5  3  5  5  5  5  5  5  5  5  5  5 10 10  4  4  4  4  3  3
#> [1471]  3  3  3  3  3  3  3  3  3  3  8  8  6  6  6  6  6  5 13  3  3
#> [1492]  3  3  3  3  3  3  3  3  2  2  3  2  2  2  2 11  5  3  3  3  3
#> [1513]  3  3  3  3  3  3  3  4  4  4  4  4  3  1  7  5  5  3  5  3  5
#> [1534]  5  3  5  5  3  5  5  7  7  4  8  5  5  6  6  6 13  3  3  3  3
#> [1555]  3  3  3  3  6  5  5  8  9  8  8  9  7  8 10  8  6  8  6  6  6
#> [1576]  7  6  6  6  6  6  5  5  5  5  5  5  5  5  7  7  7  7  7  3  3
#> [1597]  3  3  3  3  3  3  3  3  3  3 14 13 10  4  3  8  3  5  4  4  4
#> [1618] 20  7  3  3  3  3  3  3  3  3  6  3  3  3  4  7  3  3  3  6  3
#> [1639]  3  7  3  3  3  3  3  3  3  3  3  3  3  3  5  6  1  3  3  3  3
#> [1660]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  7  4  3  3  3  6  1
#> [1681]  3  3  3  2  2  3  3  3  3  5  4  7  5  7  7  5  6  5  6  5  7
#> [1702]  6  5  3  4  9  4  4  3  4  3  4  6  6  3  6  6  6  3  3  3  3
#> [1723]  3  3  4  4  6  3  6 13  3  3  3  3  3  3  6  4  6  6  6  4  4
#> [1744] 12 10  4  4  4  3  3  3  3  3  3  3  3  3  3  3  3  3  5  4  3
#> [1765]  3  3  3  3  4  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [1786]  3  3  3  3  3  3  3  3  3  9  3  1  1  1  3  3  3  1  3  3  3
#> [1807]  3  3  5  3  3  4 16  6  6  6  6  6  5  6  6  6  6  6  6  6  6
#> [1828]  6  6  6  6 10  3  4  4  6  6  1  3  3  6  3  4  4  4  4  4  4
#> [1849]  4  4  6  3 10  8  6  3 13  7  6  6  6  6  6  6  6  6  6  6  6
#> [1870]  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  4  6  6
#> [1891]  4  4  3  6  6  3  4  4  4  3  3  3  3  3  3  3  3  3  7  3  3
#> [1912]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  5  6  5  5
#> [1933]  3  5  5  5  5  5  3  5  5  6  6  6  6  6  6  6  6  6 10  3  3
#> [1954]  3  3  3  7  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [1975]  3  3  3  3  3  3  3  3  3  4  4  4  4  1  4  6  6  3  9  9  9
#> [1996]  3  3  4  5  6  6  6  6  6  6  6  6  7  6  4  2  2  5  3  3  8
#> [2017]  8  8  8  8  8  8  8  8  8  8  8  3  3  3  3  3  3  3  3  3  3
#> [2038]  3  3  8  9  9  9  3  7  2  2  4  3  4  4  4  4  4  7  7  4  3
#> [2059]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  4
#> [2080]  4  4  4  4  4  4  4  4  4  4  4  4  4  2  5  3  3  3  3  3  3
#> [2101]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [2122]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [2143]  3  3  3  6  6  4  4  4  4  2  3  3  3  3  3  3  3  3  4  4  7
#> [2164]  3  3  3  4 12 10  3  4  7  6 17  7  6  6  6  6  3  3  3  3  3
#> [2185]  3  7  7  7  7  7  7  6  6  6  6  6  7 10 12  3  5  6  6  4  6
#> [2206]  5  4  4  3  3  8  5  3 11 10  9  9 11  8  7  7  7  9  8  3  3
#> [2227] 18  6  6  1  4  8  5  5  5  5  3  3  3  3  3  5  5  3  5  5  3
#> [2248]  3  3  3  3  3  3  3  3  3  3  5  5  5  5  5  5  5  5  5  5  5
#> [2269]  5  5  5  5  5  5  7  7  7  7  7  7  7 15  3  1 21 10  7  7  3
#> [2290]  6  6  4  4  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [2311]  3  3  7  3  4  9  3  3  6  3  4  7  8  2 10  6  6  1  1 11  9
#> [2332]  9  9  9  9  4  7  7  7  7  7  7  7  7  7  7  5  3 14  3  3  5
#> [2353] 10  6  6  7  4 14  7  7 11 11  8  7  3  3  3  3  3  3  3  3  3
#> [2374]  3  3  3  3  6  7  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [2395]  6  5  4  7  3  4  4  6  6  6  7  6  7  7  7  7 12 20  3  5  5
#> [2416]  5  5  5  5  5  5  5  3  5  5  3  3  5  5  3  6  6  6  6  6  6
#> [2437]  6  6  6  6  6  6  6  6  6  6  6  6  6  6  2  7  4  5  3  3  3
#> [2458]  3  3  3  3  3  3  3  4  8  6  6  3  7  6  6 12  3  6  6 17  6
#> [2479]  6  6  6  6  6 11  3  4  4  3  3  3  3  3  3  3  8  6  3  3  3
#> [2500]  3  6  4  6  6  6  6  6  6  6  6  6  6  6  6  6  6  3  3  5  7
#> [2521]  5  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [2542]  9  9  3  9  8  6  3  3  3  3  1  4  3  4  8  8  6  7  5  6  3
#> [2563]  6  7  5  6  6  4 11  3  4  3  3  3  6 12  4  4  3  3  3  3  3
#> [2584]  3  3  6  4  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [2605]  3  3  3 10  8  6  4  4  2  3  6  6 17  7  5  5  3  3  3  3  3
#> [2626]  3  3  3  3  3  3  3  3  5  4  6  6  5  6  6  6  5  3  3  9 10
#> [2647]  5 10  6  3  6  4  4  9  6 17  7  5  5  5  5  5  5  5  5  5  5
#> [2668]  5  5  5  4  3  3 11  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [2689]  3  3  3  3  3  3  3  3  3  2  3  3  3  3  3  3  3  3  3  3  5
#> [2710]  4  4 17  7  4  6  4  9 11  6  3  3  3  8  8  3  8  8  8  8  8
#> [2731]  6  4  1  6  6  6  6  6  6  6  6  6  6  6  3  9  3  3  3  4  3
#> [2752]  6  6  6  6 10 10  9  3  6  6  4  3  7  4  7  5  5  5  7  5  9
#> [2773]  6 13  4  6  6  6  3  3  6  4  6  5  7 18  3  3  3  3  3  3  3
#> [2794]  3  3  3  4  4 19  5  3  3  3  3  3  3  5  3  3  7  7  7  7  7
#> [2815]  6  6  6  6  1  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4
#> [2836]  4  4  4  4  7 17  5 19  4  4  4  4  4  4  4  4  4  4  4  4  4
#> [2857]  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4
#> [2878]  4  4  4  4  4  4  4  4  4 10  5 13 13 13  5 13  6  5  6  6  6
#> [2899]  5  6  5  7  4 12 10 10  4 12  2  6  6  6  6  4  4 11  5  6  1
#> [2920]  5  6  6  2 10  4  4  7  4  7  5  5  5  3  3  3  3  3  3  3  3
#> [2941]  3  4  7  3  3  4  4  4  4  1  5  3  3  6 10  6  6  6  3  3  3
#> [2962]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [2983]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [3004]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [3025]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [3046]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [3067]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [3088]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  6 14  8
#> [3109]  1 13  7  6  6  6  6  4  4  4  5  7  7  5 15  9  5  4  3  3  3
#> [3130]  3  3  4  4  4  4  4  4  1 16 17  6  3  3  5  4  4  3  3  5  4
#> [3151]  3  3 12  5  6  7  7  7  5  6  5  9  6 12  4  6  5  5  5 11  1
#> [3172]  1  5  6  6  3  3  9 10  3  4  3  3  3  3  3  3  3  3  3  3  3
#> [3193]  3  3  3  3  3  3  3  3  3  3  3  3  7  9  3  3  3  3  3  3  3
#> [3214]  3  3  3  3  3  6  8  3  3  3  3  3  3  3  3  3  3  3  3  5  3
#> [3235]  3  3  3  7  6  3  3  3  3  3  3  3  3  3  3  3  3  3  3  5 11
#> [3256]  7  3  3  3  3  3  3  3  3  8  3  4  4  6  6  6  6  6  6  6  8
#> [3277]  3  4  9  4  4  4  4  4  6  1  5  6  6  6  6  6  6  3  7  7  3
#> [3298]  3  3  3  3  3  7  6  8  5  4  4  3  3  3  5  9  9  4  3  9  6
#> [3319]  6  4  3  1  5  5  5  5  5  5  7  1  4 10  3  9 15 15 12  8  5
#> [3340] 19  4  4  7  7  7 20  2  4  5  9  4  5  6  6  6  6  6  6  6  6
#> [3361]  6  6  7  7  4  4  4  6  6  6  6  6  6  6  6  6  3  6  3  5  3
#> [3382]  3 16  5  3  8  3  3  3  3  3  3  3 10  6  7 19  4  6  7  7 11
#> [3403]  9  9  9  8  9  8  4 11  4  3  3  5  4  6  5  6  7 15  7  8  8
#> [3424]  8  8  8  8  8  8  8  4  4  1  3 15 10  4  3  5  4  4  7  4  4
#> [3445] 11  6 10  7  4  6  4  4  4  4  4  4  5  3  4  6 15  7  3  3  4
#> [3466] 10  4  3  3  3  3  4  5  5  5  1  6  3  4  6  7  4  4  6  6  6
#> [3487]  6  5  3 19 13  2 11  6  6  3 10 10  2  3  3  3  5  5  5  5 20
#> [3508]  5  8  9 16  3  3  3  6  3  3  3  3  3  3  8 10  7 20 14  4  4
#> [3529]  4 16  8 10 16  9  4  7  7  7  7  1 11 11  7  3  3  3  4  5  4
#> [3550]  4 15 15 15 15  8  8 13  1  5  8  8  8  8  6  6  6  6  6  6  6
#> [3571]  4  4  4  2  3  6  8  5  6  6  8  9  7  2  2  2  4  3  4  6 10
#> [3592]  6  6  6  6  5 10  7 14 11 11  3  5  8  4 16  8 19  8  8  5  4
#> [3613]  5  5  6  5  5  5  7  6  4  6  6  6  6  6  6  6 11  6  5  5  6
#> [3634]  4  4  6 11  4  6  5  5  5  5  5  5  8  4  4  4  4  4  4  4  4
#> [3655]  6  3  2  5 18 17 18  6  8  8  3  3  4  3  3  4  6  6 17  8  5
#> [3676]  7  6  8 13 10  4  6  6  6  8 10  8  4  4  4  4  4  4  8  8  5
#> [3697]  6  9  9  9  5  8  8  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> [3718]  3 19  5  8 11  4  3  3 11  4  9  9  4  7 11  4  8  4  8  5  6
#> [3739]  6  6  6  6  6  6  6  3  3  7  5  5  5  5  4  1  8  4  4  4  4
#> [3760]  4  4 11  5  4  4  4  4  4  4  4  3 12  8  8  6 11  4 14  3  3
#> [3781] 12  4  6  3 13  3  3  5  8 12  4  4 19  4  4  4  4  4  4  8  8
#> [3802]  4  5  5  6  3  6  6  6  6  5  5  4  6  4  4  7  6  7  4  9  7
#> [3823]  7  7  7  7  7  7  7  6  6  6  6  6  6  6  6  6  6  6  6  6  6
#> [3844]  6  5  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6
#> [3865]  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6
#> [3886]  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6
#> [3907]  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6
#> [3928]  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  4
#> [3949]  4  4  3  3  3  4  1  3  3  3  6  6  6 10  9  3  4  3 11  4  8
#> [3970]  3  3  3  3  3  3  3  5  2  7  8  3  6  6  3  3  8  6  7  4  7
#> [3991]  7  7 15  7  5  7  3  5  3  6  7  4  4  4  4  4  4  4  4  8  7
#> [4012]  7  9  8  5  4  3  6  7  4  9  4  4  3 10  3  4  7  3  3  3  7
#> [4033]  4  5  3 10  5  7  6  3  3  4  4  4  4  4  5  4  4  4  6 10  3
#> [4054]  6 16  3 15  6  7  4  6 19  4 10  6  3  8 11  3  9 12  8 17  4
#> [4075]  5  6  6  6  5  5  5  7  5  1  6 11 14  9  6  6  3  7  6  5  7
#> [4096]  7  3  3  4  6  3  3  3  3  3  3  3  5  1  6 12 11  3  4  8 11
#> [4117]  9  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4
#> [4138]  4  4  4  4  4  4  4 10  7  1  7  4  3  3  3  3  5  7  4  6  6
#> [4159]  6  6  6  6  6  6  6  6  4  3  3  5  3  3  3 16 11  3 17  5  5
#> [4180]  5  4  4  4  4  4 10  4  9  9  9  4  5  4  4  8  9  4  1  4  5
#> [4201]  5  5  6  5  5  4  8 10 12  4  9 15  3  4  4  4  4  3  1  3  8
#> [4222]  8  8  8  8  3  3  3  4  4  6  3  3  3  4  3  3  3  9 14  3  1
#> [4243]  7  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6
#> [4264]  6  6  6  6  6  6  6  6  6  7  6  6  6  6  6  6  6  6  6  6  4

tidy(tate_rec, number = 1)
#> # A tibble: 1 × 3
#>   terms  functions id               
#>   <chr>  <chr>     <chr>            
#> 1 medium NA        textfeature_E5zNw
tidy(tate_obj, number = 1)
#> # A tibble: 26 × 3
#>    terms  functions     id               
#>    <chr>  <chr>         <chr>            
#>  1 medium n_words       textfeature_E5zNw
#>  2 medium n_uq_words    textfeature_E5zNw
#>  3 medium n_charS       textfeature_E5zNw
#>  4 medium n_uq_charS    textfeature_E5zNw
#>  5 medium n_digits      textfeature_E5zNw
#>  6 medium n_hashtags    textfeature_E5zNw
#>  7 medium n_uq_hashtags textfeature_E5zNw
#>  8 medium n_mentions    textfeature_E5zNw
#>  9 medium n_uq_mentions textfeature_E5zNw
#> 10 medium n_commas      textfeature_E5zNw
#> # ℹ 16 more rows

# Using custom extraction functions
nchar_round_10 <- function(x) round(nchar(x) / 10) * 10

recipe(~., data = tate_text) |>
  step_textfeature(medium,
    extract_functions = list(nchar10 = nchar_round_10)
  ) |>
  prep() |>
  bake(new_data = NULL)
#> # A tibble: 4,284 × 5
#>        id artist             title          year textfeature_medium_n…¹
#>     <dbl> <fct>              <fct>         <dbl>                  <dbl>
#>  1  21926 Absalon            Proposals fo…  1990                     60
#>  2  20472 Auerbach, Frank    Michael        1990                     20
#>  3  20474 Auerbach, Frank    Geoffrey       1990                     20
#>  4  20473 Auerbach, Frank    Jake           1990                     20
#>  5  20513 Auerbach, Frank    To the Studi…  1990                     20
#>  6  21389 Ayres, OBE Gillian Phaëthon       1990                     20
#>  7 121187 Barlow, Phyllida   Untitled       1990                     20
#>  8  19455 Baselitz, Georg    Green VIII     1990                     20
#>  9  20938 Beattie, Basil     Present Bound  1990                     30
#> 10 105941 Beuys, Joseph      Joseph Beuys…  1990                     10
#> # ℹ 4,274 more rows
#> # ℹ abbreviated name: ¹​textfeature_medium_nchar10
```
