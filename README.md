
<!-- README.md is generated from README.Rmd. Please edit that file -->

# textrecipes

[![Travis build
status](https://travis-ci.org/EmilHvitfeldt/textrecipes.svg?branch=master)](https://travis-ci.org/EmilHvitfeldt/textrecipes)
[![Coverage
status](https://codecov.io/gh/EmilHvitfeldt/textrecipes/branch/master/graph/badge.svg)](https://codecov.io/github/EmilHvitfeldt/textrecipes?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/textrecipes)](http://cran.r-project.org/web/packages/textrecipes)
[![Downloads](http://cranlogs.r-pkg.org/badges/textrecipes)](http://cran.rstudio.com/package=textrecipes)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

**textrecipes** contains extra steps for the
[`recipes`](http://cran.rstudio.com/package=recipes) package for
preprocessing text data.

## Installation

textrecipes is not avaliable from [CRAN](https://CRAN.R-project.org)
yet. But the development version can be downloaded with:

``` r
require("devtools")
install_github("emilhvitfeldt/textrecipes")
```

## Example

In the following example we will go through the steps needed to convert
a character variable to the TF-IDF of its tokenized words after removing
stopwords and limeting ourself to only the 500 most used words. We will
be conduction this preprosession on the variable `essay0`.

``` r
library(recipes)
#> Loading required package: broom
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
library(textrecipes)
 
data(okc_text)

example_data <- okc_text[, 1]

okc_rec <- recipe(~ ., data = example_data) %>%
  step_tokenize(essay0) %>% # Tokenizes to words by default
  step_stopwords(essay0) %>% # Uses the english snowball list by default
  step_tokenfilter(essay0, max.words = 500) %>%
  step_tfidf(essay0, tf.weight = "term frequency", idf.weight = "idf")
   
okc_obj <- okc_rec %>%
  prep(training = example_data, retain = TRUE)
   
bake(okc_obj, example_data)
#> # A tibble: 750 x 500
#>    `tfidf-essay0-2` `tfidf-essay0-2… `tfidf-essay0-3` `tfidf-essay0-4`
#>               <dbl>            <dbl>            <dbl>            <dbl>
#>  1           0                0                0                     0
#>  2           0                0                0                     0
#>  3           0                0                0                     0
#>  4           0                0                0                     0
#>  5           0                0                0                     0
#>  6           0                0                0                     0
#>  7           0                0.0364           0.0349                0
#>  8           0                0                0                     0
#>  9           0                0                0                     0
#> 10           0.0196           0.0224           0                     0
#> # ... with 740 more rows, and 496 more variables: `tfidf-essay0-5` <dbl>,
#> #   `tfidf-essay0-able` <dbl>, `tfidf-essay0-active` <dbl>,
#> #   `tfidf-essay0-activities` <dbl>, `tfidf-essay0-actually` <dbl>,
#> #   `tfidf-essay0-admit` <dbl>, `tfidf-essay0-adventure` <dbl>,
#> #   `tfidf-essay0-adventures` <dbl>, `tfidf-essay0-adventurous` <dbl>,
#> #   `tfidf-essay0-afraid` <dbl>, `tfidf-essay0-age` <dbl>,
#> #   `tfidf-essay0-ago` <dbl>, `tfidf-essay0-almost` <dbl>,
#> #   `tfidf-essay0-along` <dbl>, `tfidf-essay0-also` <dbl>,
#> #   `tfidf-essay0-although` <dbl>, `tfidf-essay0-always` <dbl>,
#> #   `tfidf-essay0-amazing` <dbl>, `tfidf-essay0-american` <dbl>,
#> #   `tfidf-essay0-amp` <dbl>, `tfidf-essay0-animals` <dbl>,
#> #   `tfidf-essay0-another` <dbl>, `tfidf-essay0-anyone` <dbl>,
#> #   `tfidf-essay0-anything` <dbl>, `tfidf-essay0-appreciate` <dbl>,
#> #   `tfidf-essay0-area` <dbl>, `tfidf-essay0-around` <dbl>,
#> #   `tfidf-essay0-art` <dbl>, `tfidf-essay0-artist` <dbl>,
#> #   `tfidf-essay0-artistic` <dbl>, `tfidf-essay0-ask` <dbl>,
#> #   `tfidf-essay0-athletic` <dbl>, `tfidf-essay0-attracted` <dbl>,
#> #   `tfidf-essay0-away` <dbl>, `tfidf-essay0-awesome` <dbl>,
#> #   `tfidf-essay0-back` <dbl>, `tfidf-essay0-bad` <dbl>,
#> #   `tfidf-essay0-balance` <dbl>, `tfidf-essay0-bar` <dbl>,
#> #   `tfidf-essay0-bay` <dbl>, `tfidf-essay0-beach` <dbl>,
#> #   `tfidf-essay0-beautiful` <dbl>, `tfidf-essay0-beauty` <dbl>,
#> #   `tfidf-essay0-become` <dbl>, `tfidf-essay0-beer` <dbl>,
#> #   `tfidf-essay0-believe` <dbl>, `tfidf-essay0-berkeley` <dbl>,
#> #   `tfidf-essay0-best` <dbl>, `tfidf-essay0-better` <dbl>,
#> #   `tfidf-essay0-big` <dbl>, `tfidf-essay0-bike` <dbl>,
#> #   `tfidf-essay0-bit` <dbl>, `tfidf-essay0-body` <dbl>,
#> #   `tfidf-essay0-book` <dbl>, `tfidf-essay0-books` <dbl>,
#> #   `tfidf-essay0-born` <dbl>, `tfidf-essay0-br` <dbl>,
#> #   `tfidf-essay0-business` <dbl>, `tfidf-essay0-california` <dbl>,
#> #   `tfidf-essay0-call` <dbl>, `tfidf-essay0-came` <dbl>,
#> #   `tfidf-essay0-camping` <dbl>, `tfidf-essay0-can` <dbl>,
#> #   `tfidf-essay0-car` <dbl>, `tfidf-essay0-care` <dbl>,
#> #   `tfidf-essay0-career` <dbl>, `tfidf-essay0-caring` <dbl>,
#> #   `tfidf-essay0-cat` <dbl>, `tfidf-essay0-challenge` <dbl>,
#> #   `tfidf-essay0-change` <dbl>, `tfidf-essay0-check` <dbl>,
#> #   `tfidf-essay0-chill` <dbl>, `tfidf-essay0-city` <dbl>,
#> #   `tfidf-essay0-class` <dbl>, `tfidf-essay0-climbing` <dbl>,
#> #   `tfidf-essay0-close` <dbl>, `tfidf-essay0-coast` <dbl>,
#> #   `tfidf-essay0-coffee` <dbl>, `tfidf-essay0-college` <dbl>,
#> #   `tfidf-essay0-come` <dbl>, `tfidf-essay0-comes` <dbl>,
#> #   `tfidf-essay0-comfortable` <dbl>, `tfidf-essay0-community` <dbl>,
#> #   `tfidf-essay0-company` <dbl>, `tfidf-essay0-computer` <dbl>,
#> #   `tfidf-essay0-concerts` <dbl>, `tfidf-essay0-consider` <dbl>,
#> #   `tfidf-essay0-conversation` <dbl>, `tfidf-essay0-cook` <dbl>,
#> #   `tfidf-essay0-cooking` <dbl>, `tfidf-essay0-cool` <dbl>,
#> #   `tfidf-essay0-country` <dbl>, `tfidf-essay0-couple` <dbl>,
#> #   `tfidf-essay0-crazy` <dbl>, `tfidf-essay0-creative` <dbl>,
#> #   `tfidf-essay0-culture` <dbl>, `tfidf-essay0-curious` <dbl>,
#> #   `tfidf-essay0-currently` <dbl>, `tfidf-essay0-cute` <dbl>,
#> #   `tfidf-essay0-dance` <dbl>, …
```

## Type chart

**textrecipes** includes a little departure in design from **recipes**
in the sense that it allows some input and output to be in the form of
list columns. To avoind confusion here is a table of steps with their
expected input and output respectively. Notice how you need to end with
numeric for future analysis to work.

| Step               | Input       | Output      | Status  |
| ------------------ | ----------- | ----------- | ------- |
| `step_tokenize`    | character   | list-column | working |
| `step_untokenize`  | list-column | character   | working |
| `step_stem`        | list-column | list-column | working |
| `step_stopwords`   | list-column | list-column | working |
| `step_tokenfilter` | list-column | list-column | working |
| `step_tfidf`       | list-column | numeric     | working |
| `step_tf`          | list-column | numeric     | working |
| `step_featurehash` | list-column | numeric     | working |
| `step_word2vec`    | character   | numeric     | TODO    |

(TODO = Yet to be implemented, bug = correnctly not working, working =
the step works but still not finished i.e. missing
document/tests/arguemnts, done = finished)

This means that valid sequences includes

``` r
recipe(~ ., data = data) %>%
  step_tokenize(text) %>%
  step_stem(text) %>%
  step_stopwords(text) %>%
  step_topwords(text) %>%
  step_tfidf(text)

# or

recipe(~ ., data = data) %>%
  step_tokenize(text) %>%
  step_stem(text) %>%
  step_tfidf(text)

# or

recipe(~ ., data = data) %>%
  step_word2vec(text)
```
