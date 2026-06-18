# Stemming of Token Variables

`step_stem()` creates a *specification* of a recipe step that will
convert a
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variable to have its stemmed version.

## Usage

``` r
step_stem(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  options = list(),
  custom_stemmer = NULL,
  skip = FALSE,
  id = rand_id("stem")
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

- options:

  A list of options passed to the stemmer function.

- custom_stemmer:

  A custom stemming function. If none is provided it will default to
  "SnowballC".

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

Words tend to have different forms depending on context, such as
organize, organizes, and organizing. In many situations it is beneficial
to have these words condensed into one to allow for a smaller pool of
words. Stemming is the act of chopping off the end of words using a set
of heuristics.

Note that the stemming will only be done at the end of the word and will
therefore not work reliably on ngrams or sentences.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`,
`is_custom_stemmer`, and `id`:

- terms:

  character, the selectors or variables selected

- is_custom_stemmer:

  logical, indicate if custom stemmer was used

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
[`step_stopwords()`](https://textrecipes.tidymodels.org/dev/reference/step_stopwords.md),
[`step_tokenfilter()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenfilter.md),
[`step_tokenmerge()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenmerge.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_stem(medium)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, new_data = NULL, medium) |>
  slice(1:2)
#> # A tibble: 2 × 1
#>       medium
#>    <tknlist>
#> 1 [8 tokens]
#> 2 [3 tokens]

bake(tate_obj, new_data = NULL) |>
  slice(2) |>
  pull(medium)
#> <textrecipes_tokenlist[1]>
#> [1] [3 tokens]
#> # Unique Tokens: 3

tidy(tate_rec, number = 2)
#> # A tibble: 1 × 3
#>   terms  is_custom_stemmer id        
#>   <chr>  <lgl>             <chr>     
#> 1 medium FALSE             stem_1fbgR
tidy(tate_obj, number = 2)
#> # A tibble: 1 × 3
#>   terms  is_custom_stemmer id        
#>   <chr>  <lgl>             <chr>     
#> 1 medium FALSE             stem_1fbgR

# Using custom stemmer. Here a custom stemmer that removes the last letter
# if it is a "s".
remove_s <- function(x) gsub("s$", "", x)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_stem(medium, custom_stemmer = remove_s)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, new_data = NULL, medium) |>
  slice(1:2)
#> # A tibble: 2 × 1
#>       medium
#>    <tknlist>
#> 1 [8 tokens]
#> 2 [3 tokens]

bake(tate_obj, new_data = NULL) |>
  slice(2) |>
  pull(medium)
#> <textrecipes_tokenlist[1]>
#> [1] [3 tokens]
#> # Unique Tokens: 3
```
