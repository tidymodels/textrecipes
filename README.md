
<!-- README.md is generated from README.Rmd. Please edit that file -->

# textrecipes <a href='https://textrecipes.tidymodels.org'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R build
status](https://github.com/tidymodels/textrecipes/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/textrecipes/actions)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/textrecipes/branch/master/graph/badge.svg)](https://codecov.io/github/tidymodels/textrecipes?branch=master)
[![CRAN
status](http://www.r-pkg.org/badges/version/textrecipes)](https://CRAN.R-project.org/package=textrecipes)
[![Downloads](http://cranlogs.r-pkg.org/badges/textrecipes)](https://CRAN.R-project.org/package=textrecipes)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
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
require("devtools")
install_github("tidymodels/textrecipes")
```

## Example

In the following example we will go through the steps needed, to convert
a character variable to the TF-IDF of its tokenized words after removing
stopwords, and, limiting ourself to only the 100 most used words. The
preprocessing will be conducted on the variable `essay0` and `essay1`.

``` r
library(recipes)
library(textrecipes)
library(modeldata)

data(okc_text)

okc_rec <- recipe(~  essay0 + essay1, data = okc_text) %>%
  step_tokenize(essay0, essay1) %>% # Tokenizes to words by default
  step_stopwords(essay0, essay1) %>% # Uses the english snowball list by default
  step_tokenfilter(essay0, essay1, max_tokens = 100) %>%
  step_tfidf(essay0, essay1)
   
okc_obj <- okc_rec %>%
  prep()
   
str(bake(okc_obj, okc_text), list.len = 15)
#> tibble [750 × 200] (S3: tbl_df/tbl/data.frame)
#>  $ tfidf_essay0_also      : num [1:750] 0 0 0.0252 0.2232 0 ...
#>  $ tfidf_essay0_always    : num [1:750] 0 0 0 0 0 ...
#>  $ tfidf_essay0_amp       : num [1:750] 0.47 0.583 0 0 0 ...
#>  $ tfidf_essay0_anything  : num [1:750] 0 0 0.113 0 0 ...
#>  $ tfidf_essay0_area      : num [1:750] 0 0 0 0 0 ...
#>  $ tfidf_essay0_around    : num [1:750] 0 0 0.0348 0 0 ...
#>  $ tfidf_essay0_art       : num [1:750] 0 0 0 0 0 ...
#>  $ tfidf_essay0_back      : num [1:750] 0 0 0 0 0 ...
#>  $ tfidf_essay0_bay       : num [1:750] 0 0 0 0 0 ...
#>  $ tfidf_essay0_believe   : num [1:750] 0 0 0 0 0.314 ...
#>  $ tfidf_essay0_big       : num [1:750] 0.0781 0 0 0 0 ...
#>  $ tfidf_essay0_bit       : num [1:750] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ tfidf_essay0_br        : num [1:750] 0.121 0.565 0.121 0 0 ...
#>  $ tfidf_essay0_can       : num [1:750] 0.0488 0 0.0244 0 0 ...
#>  $ tfidf_essay0_city      : num [1:750] 0 0 0 0 0 0 0 0 0 0 ...
#>   [list output truncated]
```

## Type chart

**textrecipes** includes a little departure in design from **recipes**,
in the sense that it allows for some input and output to be in the form
of list columns. To avoid confusion, here is a table of steps with their
expected input and output respectively. Notice how you need to end with
numeric for future analysis to work.

| Step                        | Input         | Output        |
| --------------------------- | ------------- | ------------- |
| `step_tokenize()`           | character     | `tokenlist()` |
| `step_untokenize()`         | `tokenlist()` | character     |
| `step_lemma()`              | `tokenlist()` | `tokenlist()` |
| `step_stem()`               | `tokenlist()` | `tokenlist()` |
| `step_stopwords()`          | `tokenlist()` | `tokenlist()` |
| `step_pos_filter()`         | `tokenlist()` | `tokenlist()` |
| `step_ngram()`              | `tokenlist()` | `tokenlist()` |
| `step_tokenfilter()`        | `tokenlist()` | `tokenlist()` |
| `step_tokenmerge()`         | `tokenlist()` | `tokenlist()` |
| `step_tfidf()`              | `tokenlist()` | numeric       |
| `step_tf()`                 | `tokenlist()` | numeric       |
| `step_texthash()`           | `tokenlist()` | numeric       |
| `step_word_embeddings()`    | `tokenlist()` | numeric       |
| `step_textfeature()`        | character     | numeric       |
| `step_sequence_onehot()`    | character     | numeric       |
| `step_lda()`                | character     | numeric       |
| `step_text_normalization()` | character     | character     |

This means that valid sequences includes

``` r
recipe(~ ., data = data) %>%
  step_tokenize(text) %>%
  step_stem(text) %>%
  step_stopwords(text) %>%
  step_topwords(text) %>%
  step_tf(text)

# or

recipe(~ ., data = data) %>%
  step_tokenize(text) %>%
  step_stem(text) %>%
  step_tfidf(text)
```

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

  - For questions and discussions about tidymodels packages, modeling,
    and machine learning, please [post on RStudio
    Community](https://rstd.io/tidymodels-community).

  - If you think you have encountered a bug, please [submit an
    issue](https://github.com/tidymodels/textrecipes/issues).

  - Either way, learn how to create and share a
    [reprex](https://rstd.io/reprex) (a minimal, reproducible example),
    to clearly communicate about your code.

  - Check out further details on [contributing guidelines for tidymodels
    packages](https://www.tidymodels.org/contribute/) and [how to get
    help](https://www.tidymodels.org/help/).
