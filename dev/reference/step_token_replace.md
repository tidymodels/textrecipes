# Replace Tokens in a Token Variable

`step_token_replace()` creates a *specification* of a recipe step that
will replace tokens in a
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variable according to a table of patterns and replacements.

## Usage

``` r
step_token_replace(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  pattern,
  replacement,
  fixed = TRUE,
  skip = FALSE,
  id = rand_id("token_replace")
)
```

## Arguments

- recipe:

  A
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
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

  A character vector of the selected variable names. This is `NULL`
  until the step is trained by
  [`recipes::prep.recipe()`](https://recipes.tidymodels.org/reference/prep.html),
  and should not be set by hand.

- pattern:

  A character vector of tokens to look for. Must be the same length as
  `replacement`, or `replacement` must be length 1.

- replacement:

  A character vector of the tokens to replace the matches with.

- fixed:

  Logical. If `TRUE`, the default, a token is replaced when it is
  exactly equal to an element of `pattern`. If `FALSE`, `pattern` is
  treated as a vector of regular expressions, and a token is replaced
  when it matches one. Regular expressions are case-sensitive, and
  backreferences in `replacement` are not supported: `replacement` is
  always used literally.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)?
  While all operations are baked when
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is run, some operations may not be able to be conducted on new data
  (e.g. processing the outcome variable(s)). Care should be taken when
  using `skip = TRUE`, as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of existing steps (if any).

## Details

The whole token is replaced, not just the part that matched. If a token
matches more than one element of `pattern`, the first match is used.
Since the number of tokens is unchanged, the lemma and part-of-speech
attributes of the tokens are kept as is.

This is useful for normalizing spelling variants, mapping tokens to a
placeholder such as `"<number>"`, or applying a small dictionary of
corrections.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `pattern`,
`replacement`, and `id`:

- terms:

  character, the selectors or variables selected

- pattern:

  character, the patterns to look for

- replacement:

  character, the replacements

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
[`step_tokenfilter()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenfilter.md),
[`step_tokenmerge()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenmerge.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_token_replace(
    medium,
    pattern = c("paper", "canvas"),
    replacement = c("PAPER", "CANVAS")
  )

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, new_data = NULL) |>
  slice(1:2) |>
  pull(medium)
#> <textrecipes_tokenlist[2]>
#> [1] [8 tokens] [3 tokens]
#> # Unique Tokens: 11

tidy(tate_rec, number = 2)
#> # A tibble: 2 × 4
#>   terms  pattern replacement id                 
#>   <chr>  <chr>   <chr>       <chr>              
#> 1 medium paper   PAPER       token_replace_bjvPM
#> 2 medium canvas  CANVAS      token_replace_bjvPM
tidy(tate_obj, number = 2)
#> # A tibble: 2 × 4
#>   terms  pattern replacement id                 
#>   <chr>  <chr>   <chr>       <chr>              
#> 1 medium paper   PAPER       token_replace_bjvPM
#> 2 medium canvas  CANVAS      token_replace_bjvPM

# Using regular expressions
tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_token_replace(
    medium,
    pattern = "^[0-9]+$",
    replacement = "<number>",
    fixed = FALSE
  )
```
