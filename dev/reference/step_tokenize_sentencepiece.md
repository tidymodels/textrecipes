# Sentencepiece Tokenization of Character Variables

`step_tokenize_sentencepiece()` creates a *specification* of a recipe
step that will convert a character predictor into a
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variable using SentencePiece tokenization.

## Usage

``` r
step_tokenize_sentencepiece(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  vocabulary_size = 1000,
  model = NULL,
  options = list(),
  res = NULL,
  skip = FALSE,
  id = rand_id("tokenize_sentencepiece")
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

- vocabulary_size:

  An integer, indicating the number of tokens in the final vocabulary.
  Defaults to `1000`. Highly encouraged to be tuned. Ignored when
  `model` is supplied.

- model:

  A string giving the path to a pretrained sentencepiece model file,
  such as one downloaded with
  [`sentencepiece::sentencepiece_download_model()`](https://rdrr.io/pkg/sentencepiece/man/sentencepiece_download_model.html)
  or trained with
  [`sentencepiece::sentencepiece()`](https://rdrr.io/pkg/sentencepiece/man/sentencepiece.html).
  When supplied, no model is trained at
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) time
  and the same model is used for all selected columns. The model is read
  when the step is prepped, so the file is not needed afterwards.
  Defaults to `NULL`, which trains a new model.

- options:

  A list of options passed to
  [`sentencepiece::sentencepiece()`](https://rdrr.io/pkg/sentencepiece/man/sentencepiece.html)
  when the tokenizer is trained at
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) time.
  These options have no effect on already-trained models at
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html) time.
  Note that the arguments `x`, `model_dir`, and `vocab_size` should not
  be passed here, use `vocabulary_size` instead. Ignored when `model` is
  supplied. Defaults to [`list()`](https://rdrr.io/r/base/list.html).

- res:

  The fitted
  [`sentencepiece::sentencepiece()`](https://rdrr.io/pkg/sentencepiece/man/sentencepiece.html)
  model tokenizer. This is `NULL` until the step is trained by
  [`recipes::prep.recipe()`](https://recipes.tidymodels.org/reference/prep.html)
  and should not be set by hand.

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

If you are running into errors, you can investigate the progress of the
compiled code by setting `options = list(verbose = TRUE)`. This can
reveal if sentencepiece ran correctly or not.

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

[`step_untokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_untokenize.md)
to untokenize.

Other Steps for Tokenization:
[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md),
[`step_tokenize_bpe()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_bpe.md),
[`step_tokenize_wordpiece()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_wordpiece.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize_sentencepiece(medium)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, new_data = NULL, medium) |>
  slice(1:2)
#> # A tibble: 2 × 1
#>        medium
#>     <tknlist>
#> 1 [12 tokens]
#> 2  [3 tokens]

bake(tate_obj, new_data = NULL) |>
  slice(2) |>
  pull(medium)
#> <textrecipes_tokenlist[1]>
#> [1] [3 tokens]
#> # Unique Tokens: 3

tidy(tate_rec, number = 1)
#> # A tibble: 1 × 2
#>   terms  id                          
#>   <chr>  <chr>                       
#> 1 medium tokenize_sentencepiece_VB26i
tidy(tate_obj, number = 1)
#> # A tibble: 1 × 2
#>   terms  id                          
#>   <chr>  <chr>                       
#> 1 medium tokenize_sentencepiece_VB26i

# Using a pretrained model
model_dir <- tempfile()
dir.create(model_dir)
file <- tempfile(tmpdir = model_dir)
writeLines(as.character(tate_text$medium), file)
trained <- sentencepiece::sentencepiece(
  file,
  vocab_size = 100,
  model_dir = model_dir
)

recipe(~., data = tate_text) |>
  step_tokenize_sentencepiece(medium, model = trained$model_path) |>
  prep() |>
  bake(new_data = NULL, medium) |>
  slice(1:2)
#> # A tibble: 2 × 1
#>        medium
#>     <tknlist>
#> 1 [43 tokens]
#> 2  [3 tokens]
```
