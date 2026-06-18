# Clean Variable Names

`step_clean_names()` creates a *specification* of a recipe step that
will clean variable names so the names consist only of letters, numbers,
and the underscore.

## Usage

``` r
step_clean_names(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  clean = NULL,
  skip = FALSE,
  id = rand_id("clean_names")
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

- clean:

  A named character vector to clean variable names. This is `NULL` until
  computed by
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
this step, a tibble is returned with columns `terms`, `value`, and `id`:

- terms:

  character, the new clean variable names

- value:

  character, the original variable names

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

[`step_clean_levels()`](https://textrecipes.tidymodels.org/dev/reference/step_clean_levels.md),
[`recipes::step_factor2string()`](https://recipes.tidymodels.org/reference/step_factor2string.html),
[`recipes::step_string2factor()`](https://recipes.tidymodels.org/reference/step_string2factor.html),
[`recipes::step_regex()`](https://recipes.tidymodels.org/reference/step_regex.html),
[`recipes::step_unknown()`](https://recipes.tidymodels.org/reference/step_unknown.html),
[`recipes::step_novel()`](https://recipes.tidymodels.org/reference/step_novel.html),
[`recipes::step_other()`](https://recipes.tidymodels.org/reference/step_other.html)

Other Steps for Text Cleaning:
[`step_clean_levels()`](https://textrecipes.tidymodels.org/dev/reference/step_clean_levels.md)

## Examples

``` r
library(recipes)
data(airquality)

air_tr <- tibble(airquality[1:100, ])
air_te <- tibble(airquality[101:153, ])

rec <- recipe(~., data = air_tr)

rec <- rec |>
  step_clean_names(all_predictors())
rec <- prep(rec, training = air_tr)
tidy(rec, number = 1)
#> # A tibble: 6 × 3
#>   terms   value   id               
#>   <chr>   <chr>   <chr>            
#> 1 ozone   Ozone   clean_names_mb92Q
#> 2 solar_r Solar.R clean_names_mb92Q
#> 3 wind    Wind    clean_names_mb92Q
#> 4 temp    Temp    clean_names_mb92Q
#> 5 month   Month   clean_names_mb92Q
#> 6 day     Day     clean_names_mb92Q

bake(rec, air_tr)
#> # A tibble: 100 × 6
#>    ozone solar_r  wind  temp month   day
#>    <int>   <int> <dbl> <int> <int> <int>
#>  1    41     190   7.4    67     5     1
#>  2    36     118   8      72     5     2
#>  3    12     149  12.6    74     5     3
#>  4    18     313  11.5    62     5     4
#>  5    NA      NA  14.3    56     5     5
#>  6    28      NA  14.9    66     5     6
#>  7    23     299   8.6    65     5     7
#>  8    19      99  13.8    59     5     8
#>  9     8      19  20.1    61     5     9
#> 10    NA     194   8.6    69     5    10
#> # ℹ 90 more rows
bake(rec, air_te)
#> # A tibble: 53 × 6
#>    ozone solar_r  wind  temp month   day
#>    <int>   <int> <dbl> <int> <int> <int>
#>  1   110     207   8      90     8     9
#>  2    NA     222   8.6    92     8    10
#>  3    NA     137  11.5    86     8    11
#>  4    44     192  11.5    86     8    12
#>  5    28     273  11.5    82     8    13
#>  6    65     157   9.7    80     8    14
#>  7    NA      64  11.5    79     8    15
#>  8    22      71  10.3    77     8    16
#>  9    59      51   6.3    79     8    17
#> 10    23     115   7.4    76     8    18
#> # ℹ 43 more rows
```
