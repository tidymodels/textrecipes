# Untokenization of Token Variables

`step_untokenize()` creates a *specification* of a recipe step that will
convert a
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variable into a character predictor.

## Usage

``` r
step_untokenize(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  sep = " ",
  skip = FALSE,
  id = rand_id("untokenize")
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

- sep:

  a character to determine how the tokens should be separated when
  pasted together. Defaults to `" "`.

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

This steps will turn a
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
vector back into a character vector. This step is calling `paste`
internally to put the tokens back together to a character.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `value`, and `id`:

- terms:

  character, the selectors or variables selected

- value:

  character, seperator used for collapsing

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
to turn characters into
[`tokens`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_untokenize(medium)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, new_data = NULL, medium) |>
  slice(1:2)
#> # A tibble: 2 × 1
#>   medium                                             
#>   <fct>                                              
#> 1 video monitor or projection colour and sound stereo
#> 2 etching on paper                                   

bake(tate_obj, new_data = NULL) |>
  slice(2) |>
  pull(medium)
#> [1] etching on paper
#> 1029 Levels: 100 digital prints on paper ink on paper and wall text ...

tidy(tate_rec, number = 2)
#> # A tibble: 1 × 3
#>   terms  value id              
#>   <chr>  <chr> <chr>           
#> 1 medium NA    untokenize_NGEYl
tidy(tate_obj, number = 2)
#> # A tibble: 1 × 3
#>   terms  value id              
#>   <chr>  <chr> <chr>           
#> 1 medium " "   untokenize_NGEYl
```
