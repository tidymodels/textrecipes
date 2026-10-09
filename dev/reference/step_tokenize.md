# Tokenization of Character Variables

`step_tokenize()` creates a *specification* of a recipe step that will
convert a character predictor into a
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variable.

## Usage

``` r
step_tokenize(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  training_options = list(),
  options = list(),
  token = "words",
  engine = "tokenizers",
  custom_token = NULL,
  skip = FALSE,
  id = rand_id("tokenize")
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

- training_options:

  A list of options passed to the tokenizer when it is being trained.
  Only applicable when `engine = "tokenizers.bpe"`. Note that the
  argument `x` should not be passed here. Defaults to
  [`list()`](https://rdrr.io/r/base/list.html).

- options:

  A list of options passed to the tokenizer. Note that the argument `x`
  should not be passed here. For `engine = "tokenizers.bpe"` these
  options only apply when the tokenizer is trained (at
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) time);
  they have no effect on already-trained models at
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html) time.
  For `engine = "spacyr"` the options are applied each time the data is
  tokenized, including at
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html) time.
  Defaults to [`list()`](https://rdrr.io/r/base/list.html).

- token:

  A single string giving the unit for tokenizing. See details for
  options. Defaults to `"words"`.

- engine:

  A single string giving the package that will be used for tokenization.
  See details for options. Defaults to `"tokenizers"`.

- custom_token:

  A user supplied tokenizer function. Must take a character vector as
  input and output a list of character vectors. If `NULL`, the default,
  `token` and `engine` determine the tokenizer; otherwise they are
  overwritten.

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

Tokenization is the act of splitting a character vector into smaller
parts to be further analyzed. This step uses the `tokenizers` package
which includes heuristics on how to to split the text into paragraphs
tokens, word tokens, among others. `textrecipes` keeps the tokens as a
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variable and other steps will do their tasks on those
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variables before transforming them back to numeric variables.

Working with `textrecipes` will almost always start by calling
`step_tokenize` followed by modifying and filtering steps. This is not
always the case as you sometimes want to apply pre-tokenization steps;
this can be done with
[`recipes::step_mutate()`](https://recipes.tidymodels.org/reference/step_mutate.html).

## Engines

The choice of `engine` determines the possible choices of `token`.

The following is some small example data used in the following examples

### tokenizers

The tokenizers package is the default `engine` and it comes with the
following unit of `token`. All of these options correspond to a function
in the tokenizers package.

- "words" (default)

- "characters"

- "character_shingles"

- "ngrams"

- "skip_ngrams"

- "sentences"

- "lines"

- "paragraphs"

- "regex"

- "ptb" (Penn Treebank)

- "skip_ngrams"

- "word_stems"

The default tokenizer is `"word"` which splits the text into a series of
words. By using `step_tokenize()` without setting any arguments you get
word tokens

This tokenizer has arguments that change how the tokenization occurs and
can accessed using the `options` argument by passing a named list. Here
we are telling
[tokenizers::tokenize_words](https://docs.ropensci.org/tokenizers/reference/basic-tokenizers.html)
that we don't want to turn the words to lowercase

We can also stop removing punctuation.

The tokenizer can be changed by setting a different `token`. Here we
change it to return character tokens.

It is worth noting that not all these token methods are appropriate but
are included for completeness.

### spacyr

- "words"

### tokenizers.bpe

The tokeenizers.bpe engine performs Byte Pair Encoding Text
Tokenization.

- "words"

This tokenizer is trained on the training set and will thus need to be
passed training arguments. These are passed to the `training_options`
argument and the most important one is `vocab_size`. The determines the
number of unique tokens the tokenizer will produce. It is generally set
to a much higher value, typically in the thousands, but is set to 22
here for demonstration purposes.

### udpipe

- "words"

### custom_token

Sometimes you need to perform tokenization that is not covered by the
supported engines. In that case you can use the `custom_token` argument
to pass a function in that performs the tokenization you want.

Below is an example of a very simple space tokenization. This is a very
fast way of tokenizing.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `value`, and `id`:

- terms:

  character, the selectors or variables selected

- value:

  character, unit of tokenization

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

[`step_untokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_untokenize.md)
to untokenize.

Other Steps for Tokenization:
[`step_tokenize_bpe()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_bpe.md),
[`step_tokenize_sentencepiece()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_sentencepiece.md),
[`step_tokenize_wordpiece()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_wordpiece.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium)

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

tidy(tate_rec, number = 1)
#> # A tibble: 1 × 3
#>   terms  value id            
#>   <chr>  <chr> <chr>         
#> 1 medium NA    tokenize_WI0lV
tidy(tate_obj, number = 1)
#> # A tibble: 1 × 3
#>   terms  value id            
#>   <chr>  <chr> <chr>         
#> 1 medium words tokenize_WI0lV

tate_obj_chars <- recipe(~., data = tate_text) |>
  step_tokenize(medium, token = "characters") |>
  prep()

bake(tate_obj, new_data = NULL) |>
  slice(2) |>
  pull(medium)
#> <textrecipes_tokenlist[1]>
#> [1] [3 tokens]
#> # Unique Tokens: 3
```
