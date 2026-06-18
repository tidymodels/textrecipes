# Filtering of Stop Words for Tokens Variables

`step_stopwords()` creates a *specification* of a recipe step that will
filter a
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variable for stop words.

## Usage

``` r
step_stopwords(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  language = "en",
  keep = FALSE,
  stopword_source = "snowball",
  custom_stopword_source = NULL,
  skip = FALSE,
  id = rand_id("stopwords")
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

- language:

  A character to indicate the language of stop words by ISO 639-1 coding
  scheme.

- keep:

  A logical. Specifies whether to keep the stop words or discard them.

- stopword_source:

  A character to indicate the stop words source as listed in
  [`stopwords::stopwords_getsources`](https://rdrr.io/pkg/stopwords/man/stopwords_getsources.html).

- custom_stopword_source:

  A character vector to indicate a custom list of words that cater to
  the users specific problem.

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

Stop words are words which sometimes are removed before natural language
processing tasks. While stop words usually refers to the most common
words in the language there is no universal stop word list.

The argument `custom_stopword_source` allows you to pass a character
vector to filter against. With the `keep` argument one can specify words
to keep instead of removing thus allowing you to select words with a
combination of these two arguments.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `value`, `keep`,
and `id`:

- terms:

  character, the selectors or variables selected

- value:

  character, name of stop word list

- keep:

  logical, whether stop words are removed or kept

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
[`step_tokenfilter()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenfilter.md),
[`step_tokenmerge()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenmerge.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(tate_text)
tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_stopwords(medium)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, new_data = NULL, medium) |>
  slice(1:2)
#> # A tibble: 2 × 1
#>       medium
#>    <tknlist>
#> 1 [6 tokens]
#> 2 [2 tokens]

bake(tate_obj, new_data = NULL) |>
  slice(2) |>
  pull(medium)
#> <textrecipes_tokenlist[1]>
#> [1] [2 tokens]
#> # Unique Tokens: 2

tidy(tate_rec, number = 2)
#> # A tibble: 1 × 4
#>   terms  value keep  id             
#>   <chr>  <chr> <lgl> <chr>          
#> 1 medium NA    NA    stopwords_69PQP
tidy(tate_obj, number = 2)
#> # A tibble: 1 × 4
#>   terms  value    keep  id             
#>   <chr>  <chr>    <lgl> <chr>          
#> 1 medium snowball FALSE stopwords_69PQP

# With a custom stop words list

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_stopwords(medium, custom_stopword_source = c("twice", "upon"))
tate_obj <- tate_rec |>
  prep(traimomg = tate_text)

bake(tate_obj, new_data = NULL) |>
  slice(2) |>
  pull(medium)
#> <textrecipes_tokenlist[1]>
#> [1] [3 tokens]
#> # Unique Tokens: 3
```
