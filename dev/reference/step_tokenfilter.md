# Filter Tokens Based on Term Frequency

`step_tokenfilter()` creates a *specification* of a recipe step that
will convert a
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variable to be filtered based on frequency.

## Usage

``` r
step_tokenfilter(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  max_times = Inf,
  min_times = 0,
  percentage = FALSE,
  max_tokens = 100,
  filter_fun = NULL,
  res = NULL,
  skip = FALSE,
  id = rand_id("tokenfilter")
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

- max_times:

  An integer. Maximal number of times a word can appear before getting
  removed.

- min_times:

  An integer. Minimum number of times a word can appear before getting
  removed.

- percentage:

  A logical. Should max_times and min_times be interpreted as a
  percentage instead of count.

- max_tokens:

  An integer. Will only keep the top max_tokens tokens after filtering
  done by max_times and min_times. Defaults to 100.

- filter_fun:

  A function. This function should take a vector of characters, and
  return a logical vector of the same length. This function will be
  applied to each observation of the data set. Defaults to `NULL`. All
  other arguments will be ignored if this argument is used.

- res:

  The words that will be keep will be stored here once this
  preprocessing step has be trained by
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

## Details

This step allows you to limit the tokens you are looking at by filtering
on their occurrence in the corpus. You are able to exclude tokens if
they appear too many times or too few times in the data. It can be
specified as counts using `max_times` and `min_times` or as percentages
by setting `percentage` as `TRUE`. In addition one can filter to only
use the top `max_tokens` used tokens. If `max_tokens` is set to `Inf`
then all the tokens will be used. This will generally lead to very large
data sets when then tokens are words or trigrams. A good strategy is to
start with a low token count and go up according to how much RAM you
want to use.

It is strongly advised to filter before using
[step_tf](https://textrecipes.tidymodels.org/dev/reference/step_tf.md)
or
[step_tfidf](https://textrecipes.tidymodels.org/dev/reference/step_tfidf.md)
to limit the number of variables created.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `value`, and `id`:

- terms:

  character, the selectors or variables selected

- value:

  integer, number of unique tokens

- id:

  character, id of this step

## Tuning Parameters

This step has 3 tuning parameters:

- `max_times`: Maximum Token Frequency (type: integer, default: Inf)

- `min_times`: Minimum Token Frequency (type: integer, default: 0)

- `max_tokens`: \# Retained Tokens (type: integer, default: 100)

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
[`step_tokenmerge()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenmerge.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_tokenfilter(medium)

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
#>   terms  value id               
#>   <chr>  <int> <chr>            
#> 1 medium    NA tokenfilter_tAJil
tidy(tate_obj, number = 2)
#> # A tibble: 1 × 3
#>   terms  value id               
#>   <chr>  <int> <chr>            
#> 1 medium   952 tokenfilter_tAJil
```
