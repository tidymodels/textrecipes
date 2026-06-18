# Term Frequency-Inverse Document Frequency of Tokens

`step_tfidf()` creates a *specification* of a recipe step that will
convert a
[`token`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)
variable into multiple variables containing the term frequency-inverse
document frequency of tokens.

## Usage

``` r
step_tfidf(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  vocabulary = NULL,
  res = NULL,
  smooth_idf = TRUE,
  norm = "l1",
  sublinear_tf = FALSE,
  prefix = "tfidf",
  sparse = "auto",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("tfidf")
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

- vocabulary:

  A character vector of strings to be considered.

- res:

  The words that will be used to calculate the term frequency will be
  stored here once this preprocessing step has be trained by
  [`recipes::prep.recipe()`](https://recipes.tidymodels.org/reference/prep.html).

- smooth_idf:

  TRUE smooth IDF weights by adding one to document frequencies, as if
  an extra document was seen containing every term in the collection
  exactly once. This prevents division by zero.

- norm:

  A character, defines the type of normalization to apply to term
  vectors. "l1" by default, i.e., scale by the number of words in the
  document. Must be one of c("l1", "l2", "none").

- sublinear_tf:

  A logical, apply sublinear term-frequency scaling, i.e., replace the
  term frequency with 1 + log(TF). Defaults to FALSE.

- prefix:

  A character string that will be the prefix to the resulting new
  variables. See notes below.

- sparse:

  A single string. Should the columns produced be sparse vectors. Can
  take the values `"yes"`, `"no"`, and `"auto"`. If `sparse = "auto"`
  then workflows can determine the best option. Defaults to `"auto"`.

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

## Details

It is strongly advised to use
[step_tokenfilter](https://textrecipes.tidymodels.org/dev/reference/step_tokenfilter.md)
before using step_tfidf to limit the number of variables created;
otherwise you may run into memory issues. A good strategy is to start
with a low token count and increase depending on how much RAM you want
to use.

Term frequency-inverse document frequency is the product of two
statistics: the term frequency (TF) and the inverse document frequency
(IDF).

Term frequency measures how many times each token appears in each
observation.

Inverse document frequency is a measure of how informative a word is,
e.g., how common or rare the word is across all the observations. If a
word appears in all the observations it might not give that much
insight, but if it only appears in some it might help differentiate
between observations.

The IDF is defined as follows: idf = log(1 + (# documents in the corpus)
/ (# documents where the term appears))

The new components will have names that begin with `prefix`, then the
name of the variable, followed by the tokens all separated by `-`. The
variable names are padded with zeros. For example if `prefix = "hash"`,
and if `num_terms < 10`, their names will be `hash1` - `hash9`. If
`num_terms = 101`, their names will be `hash001` - `hash101`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `token`, `weight`,
and `id`:

- terms:

  character, the selectors or variables selected

- token:

  character, name of token

- weight:

  numeric, the calculated IDF weight

- id:

  character, id of this step

## Sparse data

This step produces sparse columns if `sparse = "yes"` is being set. The
default value `"auto"` won't trigger production fo sparse columns if a
recipe is
[`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)ed,
but allows for a workflow to toggle to `"yes"` or `"no"` depending on
whether the model supports
[recipes::sparse_data](https://recipes.tidymodels.org/reference/sparse_data.html)
and if the model is is expected to run faster with the data.

The mechanism for determining how much sparsity is produced isn't
perfect, and there will be times when you want to manually overwrite by
setting `sparse = "yes"` or `sparse = "no"`.

## Case weights

The underlying operation does not allow for case weights.

## See also

[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
to turn characters into
[`tokens`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)

Other Steps for Numeric Variables From Tokens:
[`step_lda()`](https://textrecipes.tidymodels.org/dev/reference/step_lda.md),
[`step_texthash()`](https://textrecipes.tidymodels.org/dev/reference/step_texthash.md),
[`step_tf()`](https://textrecipes.tidymodels.org/dev/reference/step_tf.md),
[`step_word_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_word_embeddings.md)

## Examples

``` r
# \donttest{
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_tfidf(medium)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, tate_text)
#> # A tibble: 4,284 × 956
#>        id artist             title  year tfidf_medium_1 tfidf_medium_10
#>     <dbl> <fct>              <fct> <dbl>          <dbl>           <dbl>
#>  1  21926 Absalon            Prop…  1990              0               0
#>  2  20472 Auerbach, Frank    Mich…  1990              0               0
#>  3  20474 Auerbach, Frank    Geof…  1990              0               0
#>  4  20473 Auerbach, Frank    Jake   1990              0               0
#>  5  20513 Auerbach, Frank    To t…  1990              0               0
#>  6  21389 Ayres, OBE Gillian Phaë…  1990              0               0
#>  7 121187 Barlow, Phyllida   Unti…  1990              0               0
#>  8  19455 Baselitz, Georg    Gree…  1990              0               0
#>  9  20938 Beattie, Basil     Pres…  1990              0               0
#> 10 105941 Beuys, Joseph      Jose…  1990              0               0
#> # ℹ 4,274 more rows
#> # ℹ 950 more variables: tfidf_medium_100 <dbl>, tfidf_medium_11 <dbl>,
#> #   tfidf_medium_12 <dbl>, tfidf_medium_13 <dbl>,
#> #   tfidf_medium_133 <dbl>, tfidf_medium_14 <dbl>,
#> #   tfidf_medium_15 <dbl>, tfidf_medium_151 <dbl>,
#> #   tfidf_medium_16 <dbl>, tfidf_medium_160 <dbl>,
#> #   tfidf_medium_16mm <dbl>, tfidf_medium_18 <dbl>, …

tidy(tate_rec, number = 2)
#> # A tibble: 1 × 4
#>   terms  token weight id         
#>   <chr>  <chr>  <dbl> <chr>      
#> 1 medium NA        NA tfidf_nDoh2
tidy(tate_obj, number = 2)
#> # A tibble: 952 × 4
#>    terms  token weight id         
#>    <chr>  <chr>  <dbl> <chr>      
#>  1 medium 1       7.26 tfidf_nDoh2
#>  2 medium 10      7.26 tfidf_nDoh2
#>  3 medium 100     7.26 tfidf_nDoh2
#>  4 medium 11      7.67 tfidf_nDoh2
#>  5 medium 12      7.67 tfidf_nDoh2
#>  6 medium 13      8.36 tfidf_nDoh2
#>  7 medium 133     8.36 tfidf_nDoh2
#>  8 medium 14      6.75 tfidf_nDoh2
#>  9 medium 15      6.57 tfidf_nDoh2
#> 10 medium 151     8.36 tfidf_nDoh2
#> # ℹ 942 more rows
# }
```
