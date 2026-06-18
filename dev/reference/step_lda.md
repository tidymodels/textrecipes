# Calculate LDA Dimension Estimates of Tokens

`step_lda()` creates a *specification* of a recipe step that will return
the lda dimension estimates of a text variable.

## Usage

``` r
step_lda(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  lda_models = NULL,
  num_topics = 10L,
  prefix = "lda",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("lda")
)
```

## Source

<https://arxiv.org/abs/1301.3781>

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

- lda_models:

  A WarpLDA model object from the text2vec package. If left to NULL, the
  default, it will train its model based on the training data. Look at
  the examples for how to fit a WarpLDA model.

- num_topics:

  integer desired number of latent topics.

- prefix:

  A prefix for generated column names, defaults to "lda".

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
this step, a tibble is returned with columns `terms`, `num_topics`, and
`id`:

- terms:

  character, the selectors or variables selected

- num_topics:

  integer, number of topics

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

Other Steps for Numeric Variables From Tokens:
[`step_texthash()`](https://textrecipes.tidymodels.org/dev/reference/step_texthash.md),
[`step_tf()`](https://textrecipes.tidymodels.org/dev/reference/step_tf.md),
[`step_tfidf()`](https://textrecipes.tidymodels.org/dev/reference/step_tfidf.md),
[`step_word_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_word_embeddings.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_lda(medium)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, new_data = NULL) |>
  slice(1:2)
#> # A tibble: 2 × 14
#>      id artist       title  year lda_medium_1 lda_medium_2 lda_medium_3
#>   <dbl> <fct>        <fct> <dbl>        <dbl>        <dbl>        <dbl>
#> 1 21926 Absalon      Prop…  1990          0.1       0.0429        0.114
#> 2 20472 Auerbach, F… Mich…  1990          0         0             0    
#> # ℹ 7 more variables: lda_medium_4 <dbl>, lda_medium_5 <dbl>,
#> #   lda_medium_6 <dbl>, lda_medium_7 <dbl>, lda_medium_8 <dbl>,
#> #   lda_medium_9 <dbl>, lda_medium_10 <dbl>
tidy(tate_rec, number = 2)
#> # A tibble: 1 × 3
#>   terms  num_topics id       
#>   <chr>       <int> <chr>    
#> 1 medium         10 lda_8HfdV
tidy(tate_obj, number = 2)
#> # A tibble: 1 × 3
#>   terms  num_topics id       
#>   <chr>       <int> <chr>    
#> 1 medium         10 lda_8HfdV

# Changing the number of topics.
recipe(~., data = tate_text) |>
  step_tokenize(medium, artist) |>
  step_lda(medium, artist, num_topics = 20) |>
  prep() |>
  bake(new_data = NULL) |>
  slice(1:2)
#> # A tibble: 2 × 43
#>      id title  year lda_medium_1 lda_medium_2 lda_medium_3 lda_medium_4
#>   <dbl> <fct> <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
#> 1 21926 Prop…  1990            0            0            0            0
#> 2 20472 Mich…  1990            0            0            0            0
#> # ℹ 36 more variables: lda_medium_5 <dbl>, lda_medium_6 <dbl>,
#> #   lda_medium_7 <dbl>, lda_medium_8 <dbl>, lda_medium_9 <dbl>,
#> #   lda_medium_10 <dbl>, lda_medium_11 <dbl>, lda_medium_12 <dbl>,
#> #   lda_medium_13 <dbl>, lda_medium_14 <dbl>, lda_medium_15 <dbl>,
#> #   lda_medium_16 <dbl>, lda_medium_17 <dbl>, lda_medium_18 <dbl>,
#> #   lda_medium_19 <dbl>, lda_medium_20 <dbl>, lda_artist_1 <dbl>,
#> #   lda_artist_2 <dbl>, lda_artist_3 <dbl>, lda_artist_4 <dbl>, …

# Supplying A pre-trained LDA model trained using text2vec
library(text2vec)
tokens <- word_tokenizer(tolower(tate_text$medium))
it <- itoken(tokens, ids = seq_along(tate_text$medium))
v <- create_vocabulary(it)
dtm <- create_dtm(it, vocab_vectorizer(v))
lda_model <- LDA$new(n_topics = 15)

recipe(~., data = tate_text) |>
  step_tokenize(medium, artist) |>
  step_lda(medium, artist, lda_models = lda_model) |>
  prep() |>
  bake(new_data = NULL) |>
  slice(1:2)
#> # A tibble: 2 × 33
#>      id title  year lda_medium_1 lda_medium_2 lda_medium_3 lda_medium_4
#>   <dbl> <fct> <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
#> 1 21926 Prop…  1990        0.214            0        0.257        0.286
#> 2 20472 Mich…  1990        0                0        0            0    
#> # ℹ 26 more variables: lda_medium_5 <dbl>, lda_medium_6 <dbl>,
#> #   lda_medium_7 <dbl>, lda_medium_8 <dbl>, lda_medium_9 <dbl>,
#> #   lda_medium_10 <dbl>, lda_medium_11 <dbl>, lda_medium_12 <dbl>,
#> #   lda_medium_13 <dbl>, lda_medium_14 <dbl>, lda_medium_15 <dbl>,
#> #   lda_artist_1 <dbl>, lda_artist_2 <dbl>, lda_artist_3 <dbl>,
#> #   lda_artist_4 <dbl>, lda_artist_5 <dbl>, lda_artist_6 <dbl>,
#> #   lda_artist_7 <dbl>, lda_artist_8 <dbl>, lda_artist_9 <dbl>, …
```
