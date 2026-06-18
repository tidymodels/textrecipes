# Normalization of Character Variables

`step_text_normalization()` creates a *specification* of a recipe step
that will perform Unicode Normalization on character variables.

## Usage

``` r
step_text_normalization(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  normalization_form = "nfc",
  skip = FALSE,
  id = rand_id("text_normalization")
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

- normalization_form:

  A single character string determining the Unicode Normalization. Must
  be one of "nfc", "nfd", "nfkd", "nfkc", or "nfkc_casefold". Defaults
  to "nfc". See
  [`stringi::stri_trans_nfc()`](https://rdrr.io/pkg/stringi/man/stri_trans_nf.html)
  for more details.

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
this step, a tibble is returned with columns `terms`,
`normalization_form`, and `id`:

- terms:

  character, the selectors or variables selected

- normalization_form:

  character, type of normalization

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

[`step_texthash()`](https://textrecipes.tidymodels.org/dev/reference/step_texthash.md)
for feature hashing.

## Examples

``` r
library(recipes)

sample_data <- tibble(text = c("sch\U00f6n", "scho\U0308n"))

rec <- recipe(~., data = sample_data) |>
  step_text_normalization(text)

prepped <- rec |>
  prep()

bake(prepped, new_data = NULL, text) |>
  slice(1:2)
#> # A tibble: 2 × 1
#>   text 
#>   <fct>
#> 1 schön
#> 2 schön

bake(prepped, new_data = NULL) |>
  slice(2) |>
  pull(text)
#> [1] schön
#> Levels: schön

tidy(rec, number = 1)
#> # A tibble: 1 × 3
#>   terms normalization_form id                      
#>   <chr> <chr>              <chr>                   
#> 1 text  NA                 text_normalization_E0Xok
tidy(prepped, number = 1)
#> # A tibble: 1 × 3
#>   terms normalization_form id                      
#>   <chr> <chr>              <chr>                   
#> 1 text  nfc                text_normalization_E0Xok
```
