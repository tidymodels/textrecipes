# Using textrecipes as a sparse matrix engine

``` r

library(textrecipes)
#> Loading required package: recipes
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> 
#> Attaching package: 'recipes'
#> The following object is masked from 'package:stats':
#> 
#>     step
library(recipes)
```

This vignette walks through getting a document-term matrix, such as one
to hand to glmnet, xgboost, a clustering routine, or your own code,
straight out of a recipe. Build a recipe,
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) it on
your training text, then
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) with
`composition = "dgCMatrix"`.

## Creating a sparse matrix

We will use a handful of short documents.

``` r

train <- tibble::tibble(
  text = c(
    "the cat sat on the mat",
    "the dog chased the cat",
    "dogs and cats are pets",
    "a mat is a small rug"
  )
)

new_docs <- tibble::tibble(
  text = c("the cat chased a small dog", "a rug for the mat")
)
```

Set `sparse = "yes"` in the step to get sparse output.

``` r

rec <- recipe(~text, data = train) |>
  step_tokenize(text) |>
  step_tfidf(text, sparse = "yes") |>
  prep()
```

Baking the training data with `composition = "dgCMatrix"` returns a
sparse matrix from the **Matrix** package, one row per document and one
column per token.

``` r

x_train <- bake(rec, new_data = NULL, composition = "dgCMatrix")
x_train
#> 4 x 16 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 16 column names 'tfidf_text_a', 'tfidf_text_and', 'tfidf_text_are' ... ]]
#>                                                                 
#> [1,] .         .         .         0.1831020 .         .        
#> [2,] .         .         .         0.2197225 .         0.3218876
#> [3,] .         0.3218876 0.3218876 .         0.3218876 .        
#> [4,] 0.5364793 .         .         .         .         .        
#>                                                                
#> [1,] .         .         .         0.183102 0.2682397 .        
#> [2,] 0.3218876 .         .         .        .         .        
#> [3,] .         0.3218876 .         .        .         0.3218876
#> [4,] .         .         0.2682397 0.183102 .         .        
#>                                             
#> [1,] .         0.2682397 .         0.3662041
#> [2,] .         .         .         0.4394449
#> [3,] .         .         .         .        
#> [4,] 0.2682397 .         0.2682397 .
```

The prepped recipe remembers the vocabulary and the inverse document
frequencies learned from the training data, so new documents are mapped
onto the same columns.

``` r

x_new <- bake(rec, new_data = new_docs, composition = "dgCMatrix")
x_new
#> 2 x 16 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 16 column names 'tfidf_text_a', 'tfidf_text_and', 'tfidf_text_are' ... ]]
#>                                                                    
#> [1,] 0.2682397 . . 0.183102 . 0.2682397 0.2682397 . . .         . .
#> [2,] 0.4023595 . . .        . .         .         . . 0.2746531 . .
#>                                     
#> [1,] .         . 0.2682397 0.1831020
#> [2,] 0.4023595 . .         0.2746531

identical(colnames(x_train), colnames(x_new))
#> [1] TRUE
```

Tokens that weren’t seen during
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) are
dropped, so the columns always line up between the training and new
data.

The result is a regular `dgCMatrix`, which is the format that many R
packages take as input.

``` r

# Fit a lasso model with glmnet
fit <- glmnet::glmnet(x_train, y_train, family = "binomial")
predict(fit, newx = x_new)
```

## Steps that produce sparse data

These steps have a `sparse` argument, which needs to be set to `"yes"`
to get sparse output:

- [`step_tf()`](https://textrecipes.tidymodels.org/dev/reference/step_tf.md):
  term frequencies
- [`step_tfidf()`](https://textrecipes.tidymodels.org/dev/reference/step_tfidf.md):
  term frequency-inverse document frequency
- [`step_texthash()`](https://textrecipes.tidymodels.org/dev/reference/step_texthash.md):
  feature hashing of tokens
- [`step_dummy_hash()`](https://textrecipes.tidymodels.org/dev/reference/step_dummy_hash.md):
  feature hashing of nominal variables

## Controlling the number of columns

A vocabulary can grow very large, so you will often want to put a cap on
the number of columns. There are two options.

Use
[`step_tokenfilter()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenfilter.md)
to keep only the most frequent tokens.

``` r

recipe(~text, data = large_corpus) |>
  step_tokenize(text) |>
  step_tokenfilter(text, max_tokens = 5000) |>
  step_tf(text, sparse = "yes") |>
  prep() |>
  bake(new_data = NULL, composition = "dgCMatrix")
```

Or use feature hashing with
[`step_texthash()`](https://textrecipes.tidymodels.org/dev/reference/step_texthash.md).
The number of columns is fixed up front with `num_terms`, no matter how
large the vocabulary is, and no vocabulary has to be stored. This makes
it a good match for sparse output when the vocabulary is too big to hold
in memory.

``` r

recipe(~text, data = large_corpus) |>
  step_tokenize(text) |>
  step_texthash(text, num_terms = 4096, sparse = "yes") |>
  prep() |>
  bake(new_data = NULL, composition = "dgCMatrix")
```

[`step_dummy_hash()`](https://textrecipes.tidymodels.org/dev/reference/step_dummy_hash.md)
does the same for nominal predictors with many levels.

## Using workflows instead

If you are fitting a model with tidymodels, you don’t need to set
`sparse` yourself. A workflow turns it on for models that support sparse
data. See
[`?sparse_data`](https://recipes.tidymodels.org/reference/sparse_data.html)
in the **recipes** package for details.
