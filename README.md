
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
library(textrecipes)

data(okc_text)

okc_rec <- recipe(~ ., data = okc_text) %>%
  add_role(contains("essay"), new_role = "textual") %>%
  step_tokenize(has_role("textual")) %>% # Tokenizes to words by default
  step_stopwords(has_role("textual")) %>% # Uses the english snowball list by default
  step_tokenfilter(has_role("textual"), max_tokens = 100) %>%
  step_tfidf(has_role("textual"))
#> Warning: Changing role(s) for essay0, essay1, essay2, essay3, essay4,
#> essay5, essay6, essay7, essay8, essay9
   
okc_obj <- okc_rec %>%
  prep(training = okc_text)
   
str(bake(okc_obj, okc_text), list.len = 15)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    750 obs. of  1000 variables:
#>  $ tfidf_essay0_also         : num  0 0 0.0213 0.1888 0 ...
#>  $ tfidf_essay0_always       : num  0 0 0 0 0 ...
#>  $ tfidf_essay0_amp          : num  0.457 0.567 0 0 0 ...
#>  $ tfidf_essay0_anything     : num  0 0 0.108 0 0 ...
#>  $ tfidf_essay0_area         : num  0 0 0 0 0 ...
#>  $ tfidf_essay0_around       : num  0 0 0.0327 0 0 ...
#>  $ tfidf_essay0_art          : num  0 0 0 0 0 ...
#>  $ tfidf_essay0_back         : num  0 0 0 0 0 ...
#>  $ tfidf_essay0_bay          : num  0 0 0 0 0 ...
#>  $ tfidf_essay0_believe      : num  0 0 0 0 0.302 ...
#>  $ tfidf_essay0_big          : num  0.0747 0 0 0 0 ...
#>  $ tfidf_essay0_bit          : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ tfidf_essay0_br           : num  0.0573 0.2665 0.0573 0 0 ...
#>  $ tfidf_essay0_can          : num  0.0406 0 0.0203 0 0 ...
#>  $ tfidf_essay0_city         : num  0 0 0 0 0 0 0 0 0 0 ...
#>   [list output truncated]
```

## Type chart

**textrecipes** includes a little departure in design from **recipes**
in the sense that it allows some input and output to be in the form of
list columns. To avoind confusion here is a table of steps with their
expected input and output respectively. Notice how you need to end with
numeric for future analysis to work.

| Step               | Input       | Output      | Status |
| ------------------ | ----------- | ----------- | ------ |
| `step_tokenize`    | character   | list-column | Done   |
| `step_untokenize`  | list-column | character   | Done   |
| `step_stem`        | list-column | list-column | Done   |
| `step_stopwords`   | list-column | list-column | Done   |
| `step_tokenfilter` | list-column | list-column | Done   |
| `step_tfidf`       | list-column | numeric     | Done   |
| `step_tf`          | list-column | numeric     | Done   |
| `step_texthash`    | list-column | numeric     | Done   |
| `step_word2vec`    | character   | numeric     | TODO   |

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
  step_tf(text)

# or

recipe(~ ., data = data) %>%
  step_tokenize(text) %>%
  step_stem(text) %>%
  step_tfidf(text)
```
