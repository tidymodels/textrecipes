
<!-- README.md is generated from README.Rmd. Please edit that file -->

# textrecipes

[![Travis build
status](https://travis-ci.org/tidymodels/textrecipes.svg?branch=master)](https://travis-ci.org/tidymodels/textrecipes)
[![Coverage
status](https://codecov.io/gh/tidymodels/textrecipes/branch/master/graph/badge.svg)](https://codecov.io/github/tidymodels/textrecipes?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Introduction

**textrecipes** contains extra steps for the
[`recipes`](https://CRAN.R-project.org/package=recipes) package for
preprocessing text data.

## Installation

textrecipes is not avaliable from [CRAN](https://CRAN.R-project.org)
yet. But the development version can be downloaded with:

``` r
require("devtools")
install_github("tidymodels/textrecipes")
```

## Example

In the following example we will go through the steps needed to convert
a character variable to the TF-IDF of its tokenized words after removing
stopwords and limeting ourself to only the 100 most used words. We will
be conduction this preprosession on the variable `essay0` and `essay1`.

``` r
library(recipes)
library(textrecipes)

data(okc_text)

okc_rec <- recipe(~ ., data = okc_text) %>%
  step_tokenize(essay0, essay1) %>% # Tokenizes to words by default
  step_stopwords(essay0, essay1) %>% # Uses the english snowball list by default
  step_tokenfilter(essay0, essay1, max_tokens = 100) %>%
  step_tfidf(essay0, essay1)
   
okc_obj <- okc_rec %>%
  prep(training = okc_text)
   
str(bake(okc_obj, okc_text), list.len = 15)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    750 obs. of  208 variables:
#>  $ essay2                 : Factor w/ 749 levels "- being myself. i'm comfortable in my own skin.<br />\n- cooking, eating and washing dishes<br />\n- sleeping &"| __truncated__,..: 743 574 595 385 109 367 719 721 225 449 ...
#>  $ essay3                 : Factor w/ 737 levels "... is how batman i am.<br />\n<br />\ni'm a huge geek.<br />\n<br />\nrecently i've heard \"you're like a stra"| __truncated__,..: 655 192 523 403 675 698 51 46 417 309 ...
#>  $ essay4                 : Factor w/ 750 levels "- wealth of nations, the social contract, the prince.<br />\n<br />\n- coming to america, willy wonka and the c"| __truncated__,..: 611 634 695 638 104 113 378 86 293 323 ...
#>  $ essay5                 : Factor w/ 750 levels "- a tent<br />\n- a good pillow<br />\n- a funny hat in cold weather<br />\n- genuinely good and trustworthy fr"| __truncated__,..: 344 237 536 271 7 383 128 52 688 750 ...
#>  $ essay6                 : Factor w/ 749 levels "- being happy with simple things.<br />\n- whether lightness is unbearable.<br />\n- how to get to know someone"| __truncated__,..: 466 105 332 215 568 35 506 480 317 326 ...
#>  $ essay7                 : Factor w/ 750 levels "-out to dinner.<br />\n-at the movies.<br />\n-having drinks at a spot where i like the atmosphere.<br />\n-coo"| __truncated__,..: 658 419 50 292 552 248 530 116 144 461 ...
#>  $ essay8                 : Factor w/ 747 levels "-bad news everybody i received a message from the people of 2135,\nthey said the aliens attacked and devastated"| __truncated__,..: 254 704 622 548 709 497 347 298 76 42 ...
#>  $ essay9                 : Factor w/ 743 levels "- <em>you think i'm the bee's knees</em> (although obviously that\nwon't slim down the pool at all)<br />\n- <e"| __truncated__,..: 698 643 540 638 530 137 378 320 17 283 ...
#>  $ tfidf_essay0_also      : num  0 0 0.0213 0.1888 0 ...
#>  $ tfidf_essay0_always    : num  0 0 0 0 0 ...
#>  $ tfidf_essay0_amp       : num  0.457 0.567 0 0 0 ...
#>  $ tfidf_essay0_anything  : num  0 0 0.108 0 0 ...
#>  $ tfidf_essay0_area      : num  0 0 0 0 0 ...
#>  $ tfidf_essay0_around    : num  0 0 0.0327 0 0 ...
#>  $ tfidf_essay0_art       : num  0 0 0 0 0 ...
#>   [list output truncated]
```

## Type chart

**textrecipes** includes a little departure in design from **recipes**
in the sense that it allows some input and output to be in the form of
list columns. To avoind confusion here is a table of steps with their
expected input and output respectively. Notice how you need to end with
numeric for future analysis to work.

| Step               | Input       | Output      |
| ------------------ | ----------- | ----------- |
| `step_tokenize`    | character   | list-column |
| `step_untokenize`  | list-column | character   |
| `step_stem`        | list-column | list-column |
| `step_stopwords`   | list-column | list-column |
| `step_tokenfilter` | list-column | list-column |
| `step_tfidf`       | list-column | numeric     |
| `step_tf`          | list-column | numeric     |
| `step_texthash`    | list-column | numeric     |

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
