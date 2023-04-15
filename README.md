
<!-- README.md is generated from README.Rmd. Please edit that file -->

# textrecipes <a href='https://textrecipes.tidymodels.org'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidymodels/textrecipes/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/textrecipes/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/textrecipes/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/textrecipes?branch=main)
[![CRAN
status](http://www.r-pkg.org/badges/version/textrecipes)](https://CRAN.R-project.org/package=textrecipes)
[![Downloads](http://cranlogs.r-pkg.org/badges/textrecipes)](https://CRAN.R-project.org/package=textrecipes)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

## Introduction

**textrecipes** contain extra steps for the
[`recipes`](https://CRAN.R-project.org/package=recipes) package for
preprocessing text data.

## Installation

You can install the released version of textrecipes from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("textrecipes")
```

Install the development version from GitHub with:

``` r
# install.packages("pak")
pak::pak("tidymodels/textrecipes")
```

## Example

In the following example we will go through the steps needed, to convert
a character variable to the TF-IDF of its tokenized words after removing
stopwords, and, limiting ourself to only the 10 most used words. The
preprocessing will be conducted on the variable `medium` and `artist`.

``` r
library(recipes)
library(textrecipes)
library(modeldata)

data("tate_text")

okc_rec <- recipe(~ medium + artist, data = tate_text) %>%
  step_tokenize(medium, artist) %>%
  step_stopwords(medium, artist) %>%
  step_tokenfilter(medium, artist, max_tokens = 10) %>%
  step_tfidf(medium, artist)

okc_obj <- okc_rec %>%
  prep()

str(bake(okc_obj, tate_text))
#> tibble [4,284 × 20] (S3: tbl_df/tbl/data.frame)
#>  $ tfidf_medium_colour     : num [1:4284] 2.31 0 0 0 0 ...
#>  $ tfidf_medium_etching    : num [1:4284] 0 0.86 0.86 0.86 0 ...
#>  $ tfidf_medium_gelatin    : num [1:4284] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ tfidf_medium_lithograph : num [1:4284] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ tfidf_medium_paint      : num [1:4284] 0 0 0 0 2.35 ...
#>  $ tfidf_medium_paper      : num [1:4284] 0 0.422 0.422 0.422 0 ...
#>  $ tfidf_medium_photograph : num [1:4284] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ tfidf_medium_print      : num [1:4284] 0 0 0 0 0 ...
#>  $ tfidf_medium_screenprint: num [1:4284] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ tfidf_medium_silver     : num [1:4284] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ tfidf_artist_akram      : num [1:4284] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ tfidf_artist_beuys      : num [1:4284] 0 0 0 0 0 ...
#>  $ tfidf_artist_ferrari    : num [1:4284] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ tfidf_artist_john       : num [1:4284] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ tfidf_artist_joseph     : num [1:4284] 0 0 0 0 0 ...
#>  $ tfidf_artist_león       : num [1:4284] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ tfidf_artist_richard    : num [1:4284] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ tfidf_artist_schütte    : num [1:4284] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ tfidf_artist_thomas     : num [1:4284] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ tfidf_artist_zaatari    : num [1:4284] 0 0 0 0 0 0 0 0 0 0 ...
```

## Breaking changes

As of version 0.4.0, `step_lda()` no longer accepts character variables
and instead takes tokenlist variables.

the following recipe

``` r
recipe(~text_var, data = data) %>%
  step_lda(text_var)
```

can be replaced with the following recipe to achive the same results

``` r
lda_tokenizer <- function(x) text2vec::word_tokenizer(tolower(x))
recipe(~text_var, data = data) %>%
  step_tokenize(text_var,
    custom_token = lda_tokenizer
  ) %>%
  step_lda(text_var)
```

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

- For questions and discussions about tidymodels packages, modeling, and
  machine learning, please [post on RStudio
  Community](https://community.rstudio.com/new-topic?category_id=15&tags=tidymodels,question).

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/tidymodels/textrecipes/issues).

- Either way, learn how to create and share a
  [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
  (a minimal, reproducible example), to clearly communicate about your
  code.

- Check out further details on [contributing guidelines for tidymodels
  packages](https://www.tidymodels.org/contribute/) and [how to get
  help](https://www.tidymodels.org/help/).
