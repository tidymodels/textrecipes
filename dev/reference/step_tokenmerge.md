# Combine Multiple Token Variables Into One

`step_tokenmerge()` creates a *specification* of a recipe step that will
take multiple
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variables and combine them into one
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variable.

## Usage

``` r
step_tokenmerge(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  prefix = "tokenmerge",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("tokenmerge")
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

- prefix:

  A prefix for generated column names, defaults to "tokenmerge".

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

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
to turn characters into
[`tokens`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)

Other Steps for Token Modification:
[`step_lemma()`](https://textrecipes.tidymodels.org/dev/reference/step_lemma.md),
[`step_ngram()`](https://textrecipes.tidymodels.org/dev/reference/step_ngram.md),
[`step_pos_filter()`](https://textrecipes.tidymodels.org/dev/reference/step_pos_filter.md),
[`step_stem()`](https://textrecipes.tidymodels.org/dev/reference/step_stem.md),
[`step_stopwords()`](https://textrecipes.tidymodels.org/dev/reference/step_stopwords.md),
[`step_tokenfilter()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenfilter.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium, artist) |>
  step_tokenmerge(medium, artist)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, new_data = NULL)
#> # A tibble: 4,284 × 4
#>        id title                                         year tokenmerge
#>     <dbl> <fct>                                        <dbl>  <tknlist>
#>  1  21926 Proposals for a Habitat                       1990 [9 tokens]
#>  2  20472 Michael                                       1990 [5 tokens]
#>  3  20474 Geoffrey                                      1990 [5 tokens]
#>  4  20473 Jake                                          1990 [5 tokens]
#>  5  20513 To the Studios                                1990 [6 tokens]
#>  6  21389 Phaëthon                                      1990 [7 tokens]
#>  7 121187 Untitled                                      1990 [6 tokens]
#>  8  19455 Green VIII                                    1990 [5 tokens]
#>  9  20938 Present Bound                                 1990 [8 tokens]
#> 10 105941 Joseph Beuys: A Private Collection. A11 Art…  1990 [5 tokens]
#> # ℹ 4,274 more rows

tidy(tate_rec, number = 2)
#> # A tibble: 2 × 2
#>   terms  id              
#>   <chr>  <chr>           
#> 1 medium tokenmerge_Xew9o
#> 2 artist tokenmerge_Xew9o
tidy(tate_obj, number = 2)
#> # A tibble: 2 × 2
#>   terms  id              
#>   <chr>  <chr>           
#> 1 medium tokenmerge_Xew9o
#> 2 artist tokenmerge_Xew9o
```
