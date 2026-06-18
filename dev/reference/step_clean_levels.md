# Clean Categorical Levels

`step_clean_levels()` creates a *specification* of a recipe step that
will clean nominal data (character or factor) so the levels consist only
of letters, numbers, and the underscore.

## Usage

``` r
step_clean_levels(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  clean = NULL,
  skip = FALSE,
  id = rand_id("clean_levels")
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

  A named character vector to clean and recode categorical levels. This
  is `NULL` until computed by
  [`recipes::prep.recipe()`](https://recipes.tidymodels.org/reference/prep.html).
  Note that if the original variable is a character vector, it will be
  converted to a factor.

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

The new levels are cleaned and then reset with
[`dplyr::recode_factor()`](https://dplyr.tidyverse.org/reference/recode.html).
When data to be processed contains novel levels (i.e., not contained in
the training set), they are converted to missing.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `orginal`,
`value`, and `id`:

- terms:

  character, the selectors or variables selected

- original:

  character, the original levels

- value:

  character, the cleaned levels

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

[`step_clean_names()`](https://textrecipes.tidymodels.org/dev/reference/step_clean_names.md),
[`recipes::step_factor2string()`](https://recipes.tidymodels.org/reference/step_factor2string.html),
[`recipes::step_string2factor()`](https://recipes.tidymodels.org/reference/step_string2factor.html),
[`recipes::step_regex()`](https://recipes.tidymodels.org/reference/step_regex.html),
[`recipes::step_unknown()`](https://recipes.tidymodels.org/reference/step_unknown.html),
[`recipes::step_novel()`](https://recipes.tidymodels.org/reference/step_novel.html),
[`recipes::step_other()`](https://recipes.tidymodels.org/reference/step_other.html)

Other Steps for Text Cleaning:
[`step_clean_names()`](https://textrecipes.tidymodels.org/dev/reference/step_clean_names.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(Smithsonian)

smith_tr <- Smithsonian[1:15, ]
smith_te <- Smithsonian[16:20, ]

rec <- recipe(~., data = smith_tr)

rec <- rec |>
  step_clean_levels(name)
rec <- prep(rec, training = smith_tr)

cleaned <- bake(rec, smith_tr)

tidy(rec, number = 1)
#> # A tibble: 15 × 4
#>    terms original                                           value id   
#>    <chr> <chr>                                              <chr> <chr>
#>  1 name  Anacostia Community Museum                         anac… clea…
#>  2 name  Arthur M. Sackler Gallery                          arth… clea…
#>  3 name  Arts and Industries Building                       arts… clea…
#>  4 name  Cooper Hewitt, Smithsonian Design Museum           coop… clea…
#>  5 name  Freer Gallery of Art                               free… clea…
#>  6 name  George Gustav Heye Center                          geor… clea…
#>  7 name  Hirshhorn Museum and Sculpture Garden              hirs… clea…
#>  8 name  National Air and Space Museum                      nati… clea…
#>  9 name  National Museum of African American History and C… nati… clea…
#> 10 name  National Museum of African Art                     nati… clea…
#> 11 name  National Museum of American History                nati… clea…
#> 12 name  National Museum of Natural History                 nati… clea…
#> 13 name  National Museum of the American Indian             nati… clea…
#> 14 name  National Portrait Gallery                          nati… clea…
#> 15 name  Steven F. Udvar-Hazy Center                        stev… clea…

# novel levels are replaced with missing
bake(rec, smith_te)
#> # A tibble: 5 × 3
#>   name  latitude longitude
#>   <fct>    <dbl>     <dbl>
#> 1 NA        38.9     -77.0
#> 2 NA        38.9     -77.0
#> 3 NA        38.9     -77.0
#> 4 NA        38.9     -77.0
#> 5 NA        38.9     -77.1
```
