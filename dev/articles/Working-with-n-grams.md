# Working with n-grams

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
library(tokenizers)
```

If you want to use n-grams with textrecipes you have 2 options:

- Use a tokenizer in
  [`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
  that tokenizes to n-grams.
- Tokenize to words with
  [`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
  and use
  [`step_ngram()`](https://textrecipes.tidymodels.org/dev/reference/step_ngram.md)
  to turn them into n-grams.

Both of these methods come with pros and cons so it will be worthwhile
for you to be aware of both.

Before we get started let’s make sure we are on the same page of what we
mean when we are talking about n-grams. We normally tokenize our text
into words, which we can do with
[`tokenize_words()`](https://docs.ropensci.org/tokenizers/reference/basic-tokenizers.html)
from the tokenizers package (this is the default engine and token for
[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
in textrecipes)

``` r

abc <- c(
  "The Bank is a place where you put your money;",
  "The Bee is an insect that gathers honey."
)

tokenize_words(abc)
#> [[1]]
#>  [1] "the"   "bank"  "is"    "a"     "place" "where" "you"   "put"  
#>  [9] "your"  "money"
#> 
#> [[2]]
#> [1] "the"     "bee"     "is"      "an"      "insect"  "that"   
#> [7] "gathers" "honey"
```

N-grams are a contiguous sequence of n tokens. So to get 2-grams (or
bigrams as they are also called) we can use the
[`tokenize_ngrams()`](https://docs.ropensci.org/tokenizers/reference/ngram-tokenizers.html)
function to get them

``` r

tokenize_ngrams(abc, n = 2)
#> [[1]]
#> [1] "the bank"    "bank is"     "is a"        "a place"    
#> [5] "place where" "where you"   "you put"     "put your"   
#> [9] "your money" 
#> 
#> [[2]]
#> [1] "the bee"       "bee is"        "is an"         "an insect"    
#> [5] "insect that"   "that gathers"  "gathers honey"
```

Notice how the words appear in multiple n-grams as the window slides
across them. And by changing the `n` argument we can get any kind of
n-gram (notice how `n = 1` is the special case of tokenizing to words).

``` r

tokenize_ngrams(abc, n = 3)
#> [[1]]
#> [1] "the bank is"     "bank is a"       "is a place"     
#> [4] "a place where"   "place where you" "where you put"  
#> [7] "you put your"    "put your money" 
#> 
#> [[2]]
#> [1] "the bee is"          "bee is an"           "is an insect"       
#> [4] "an insect that"      "insect that gathers" "that gathers honey"

tokenize_ngrams(abc, n = 1)
#> [[1]]
#>  [1] "the"   "bank"  "is"    "a"     "place" "where" "you"   "put"  
#>  [9] "your"  "money"
#> 
#> [[2]]
#> [1] "the"     "bee"     "is"      "an"      "insect"  "that"   
#> [7] "gathers" "honey"
```

It can also be beneficial to specify a delimiter between the tokens in
your n-gram.

``` r

tokenize_ngrams(abc, n = 3, ngram_delim = "_")
#> [[1]]
#> [1] "the_bank_is"     "bank_is_a"       "is_a_place"     
#> [4] "a_place_where"   "place_where_you" "where_you_put"  
#> [7] "you_put_your"    "put_your_money" 
#> 
#> [[2]]
#> [1] "the_bee_is"          "bee_is_an"           "is_an_insect"       
#> [4] "an_insect_that"      "insect_that_gathers" "that_gathers_honey"
```

## Only using `step_tokenize()`

The first method works by using n-gram `token` from one of the built-in
engines in
[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md).
To get a full list of available tokens type `?step_tokenize()` and go
down to `Details`. We can use the `token="ngrams"` along with
`engine = "tokenizers"`(the default) to tokenize to n-grams. We finish
this [`recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
with
[`step_tokenfilter()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenfilter.md)
and
[`step_tf()`](https://textrecipes.tidymodels.org/dev/reference/step_tf.md).
The filtering doesn’t do anything to data of this size but it is a good
practice to use
[`step_tokenfilter()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenfilter.md)
before using
[`step_tf()`](https://textrecipes.tidymodels.org/dev/reference/step_tf.md)
or
[`step_tfidf()`](https://textrecipes.tidymodels.org/dev/reference/step_tfidf.md)
to control the size of the resulting data.frame.

``` r

abc_tibble <- tibble(text = abc)

rec <- recipe(~text, data = abc_tibble) |>
  step_tokenize(text, token = "ngrams") |>
  step_tokenfilter(text) |>
  step_tf(text)

abc_ngram <- rec |>
  prep() |>
  bake(new_data = NULL)

abc_ngram
#> # A tibble: 2 × 14
#>   `tf_text_a place where` `tf_text_an insect that` `tf_text_bank is a`
#>                     <int>                    <int>               <int>
#> 1                       1                        0                   1
#> 2                       0                        1                   0
#> # ℹ 11 more variables: `tf_text_bee is an` <int>,
#> #   `tf_text_insect that gathers` <int>, `tf_text_is a place` <int>,
#> #   `tf_text_is an insect` <int>, `tf_text_place where you` <int>,
#> #   `tf_text_put your money` <int>,
#> #   `tf_text_that gathers honey` <int>, `tf_text_the bank is` <int>,
#> #   `tf_text_the bee is` <int>, `tf_text_where you put` <int>,
#> #   `tf_text_you put your` <int>

names(abc_ngram)
#>  [1] "tf_text_a place where"       "tf_text_an insect that"     
#>  [3] "tf_text_bank is a"           "tf_text_bee is an"          
#>  [5] "tf_text_insect that gathers" "tf_text_is a place"         
#>  [7] "tf_text_is an insect"        "tf_text_place where you"    
#>  [9] "tf_text_put your money"      "tf_text_that gathers honey" 
#> [11] "tf_text_the bank is"         "tf_text_the bee is"         
#> [13] "tf_text_where you put"       "tf_text_you put your"
```

If you need to pass arguments to the underlying tokenizer function you
can pass a named list to the `options` argument in
[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)

``` r

abc_tibble <- tibble(text = abc)

rec <- recipe(~text, data = abc_tibble) |>
  step_tokenize(text, token = "ngrams", options = list(
    n = 2,
    ngram_delim = "_"
  )) |>
  step_tokenfilter(text) |>
  step_tf(text)

abc_ngram <- rec |>
  prep() |>
  bake(new_data = NULL)

abc_ngram
#> # A tibble: 2 × 16
#>   tf_text_a_place tf_text_an_insect tf_text_bank_is tf_text_bee_is
#>             <int>             <int>           <int>          <int>
#> 1               1                 0               1              0
#> 2               0                 1               0              1
#> # ℹ 12 more variables: tf_text_gathers_honey <int>,
#> #   tf_text_insect_that <int>, tf_text_is_a <int>,
#> #   tf_text_is_an <int>, tf_text_place_where <int>,
#> #   tf_text_put_your <int>, tf_text_that_gathers <int>,
#> #   tf_text_the_bank <int>, tf_text_the_bee <int>,
#> #   tf_text_where_you <int>, tf_text_you_put <int>,
#> #   tf_text_your_money <int>

names(abc_ngram)
#>  [1] "tf_text_a_place"       "tf_text_an_insect"    
#>  [3] "tf_text_bank_is"       "tf_text_bee_is"       
#>  [5] "tf_text_gathers_honey" "tf_text_insect_that"  
#>  [7] "tf_text_is_a"          "tf_text_is_an"        
#>  [9] "tf_text_place_where"   "tf_text_put_your"     
#> [11] "tf_text_that_gathers"  "tf_text_the_bank"     
#> [13] "tf_text_the_bee"       "tf_text_where_you"    
#> [15] "tf_text_you_put"       "tf_text_your_money"
```

Lastly you can also supply a custom tokenizer to
[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
using the `custom_token` argument.

``` r

abc_tibble <- tibble(text = abc)

bigram <- function(x) {
  tokenizers::tokenize_ngrams(x, lowercase = FALSE, n = 2, ngram_delim = ".")
}

rec <- recipe(~text, data = abc_tibble) |>
  step_tokenize(text, custom_token = bigram) |>
  step_tokenfilter(text) |>
  step_tf(text)

abc_ngram <- rec |>
  prep() |>
  bake(new_data = NULL)

abc_ngram
#> # A tibble: 2 × 16
#>   tf_text_a.place tf_text_an.insect tf_text_Bank.is tf_text_Bee.is
#>             <int>             <int>           <int>          <int>
#> 1               1                 0               1              0
#> 2               0                 1               0              1
#> # ℹ 12 more variables: tf_text_gathers.honey <int>,
#> #   tf_text_insect.that <int>, tf_text_is.a <int>,
#> #   tf_text_is.an <int>, tf_text_place.where <int>,
#> #   tf_text_put.your <int>, tf_text_that.gathers <int>,
#> #   tf_text_The.Bank <int>, tf_text_The.Bee <int>,
#> #   tf_text_where.you <int>, tf_text_you.put <int>,
#> #   tf_text_your.money <int>

names(abc_ngram)
#>  [1] "tf_text_a.place"       "tf_text_an.insect"    
#>  [3] "tf_text_Bank.is"       "tf_text_Bee.is"       
#>  [5] "tf_text_gathers.honey" "tf_text_insect.that"  
#>  [7] "tf_text_is.a"          "tf_text_is.an"        
#>  [9] "tf_text_place.where"   "tf_text_put.your"     
#> [11] "tf_text_that.gathers"  "tf_text_The.Bank"     
#> [13] "tf_text_The.Bee"       "tf_text_where.you"    
#> [15] "tf_text_you.put"       "tf_text_your.money"
```

Pros:

- Only uses 1 step
- Simple to use

Cons:

- Minimal flexibility,
  ([`tokenizers::tokenize_ngrams()`](https://docs.ropensci.org/tokenizers/reference/ngram-tokenizers.html)
  doesn’t let you control how the words are tokenized.)
- You are not able to tune the number of tokens in your n-gram

## Using `step_tokenize()` and `step_ngram()`

As of version 0.2.0 you can use
[`step_ngram()`](https://textrecipes.tidymodels.org/dev/reference/step_ngram.md)
along with
[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
to gain higher control over how your n-grams are being generated.

``` r

abc_tibble <- tibble(text = abc)

rec <- recipe(~text, data = abc_tibble) |>
  step_tokenize(text) |>
  step_ngram(text, num_tokens = 3) |>
  step_tokenfilter(text) |>
  step_tf(text)

abc_ngram <- rec |>
  prep() |>
  bake(new_data = NULL)

abc_ngram
#> # A tibble: 2 × 14
#>   tf_text_a_place_where tf_text_an_insect_that tf_text_bank_is_a
#>                   <int>                  <int>             <int>
#> 1                     1                      0                 1
#> 2                     0                      1                 0
#> # ℹ 11 more variables: tf_text_bee_is_an <int>,
#> #   tf_text_insect_that_gathers <int>, tf_text_is_a_place <int>,
#> #   tf_text_is_an_insect <int>, tf_text_place_where_you <int>,
#> #   tf_text_put_your_money <int>, tf_text_that_gathers_honey <int>,
#> #   tf_text_the_bank_is <int>, tf_text_the_bee_is <int>,
#> #   tf_text_where_you_put <int>, tf_text_you_put_your <int>

names(abc_ngram)
#>  [1] "tf_text_a_place_where"       "tf_text_an_insect_that"     
#>  [3] "tf_text_bank_is_a"           "tf_text_bee_is_an"          
#>  [5] "tf_text_insect_that_gathers" "tf_text_is_a_place"         
#>  [7] "tf_text_is_an_insect"        "tf_text_place_where_you"    
#>  [9] "tf_text_put_your_money"      "tf_text_that_gathers_honey" 
#> [11] "tf_text_the_bank_is"         "tf_text_the_bee_is"         
#> [13] "tf_text_where_you_put"       "tf_text_you_put_your"
```

Now you are able to perform additional steps between the tokenization
and the n-gram creation such as stemming the tokens.

``` r

abc_tibble <- tibble(text = abc)

rec <- recipe(~text, data = abc_tibble) |>
  step_tokenize(text) |>
  step_stem(text) |>
  step_ngram(text, num_tokens = 3) |>
  step_tokenfilter(text) |>
  step_tf(text)

abc_ngram <- rec |>
  prep() |>
  bake(new_data = NULL)

abc_ngram
#> # A tibble: 2 × 14
#>   tf_text_a_place_where tf_text_an_insect_that tf_text_bank_i_a
#>                   <int>                  <int>            <int>
#> 1                     1                      0                1
#> 2                     0                      1                0
#> # ℹ 11 more variables: tf_text_bee_i_an <int>,
#> #   tf_text_i_a_place <int>, tf_text_i_an_insect <int>,
#> #   tf_text_insect_that_gather <int>, tf_text_place_where_you <int>,
#> #   tf_text_put_your_monei <int>, tf_text_that_gather_honei <int>,
#> #   tf_text_the_bank_i <int>, tf_text_the_bee_i <int>,
#> #   tf_text_where_you_put <int>, tf_text_you_put_your <int>

names(abc_ngram)
#>  [1] "tf_text_a_place_where"      "tf_text_an_insect_that"    
#>  [3] "tf_text_bank_i_a"           "tf_text_bee_i_an"          
#>  [5] "tf_text_i_a_place"          "tf_text_i_an_insect"       
#>  [7] "tf_text_insect_that_gather" "tf_text_place_where_you"   
#>  [9] "tf_text_put_your_monei"     "tf_text_that_gather_honei" 
#> [11] "tf_text_the_bank_i"         "tf_text_the_bee_i"         
#> [13] "tf_text_where_you_put"      "tf_text_you_put_your"
```

This also works great for cases where you need higher flexibility or
when you want to use a more powerful engine such as **spacyr** that
doesn’t come with an n-gram tokenizer.

Furthermore the `num_tokens` argument is tunable with the **dials** and
**tune** package.

Pros:

- Full flexibility
- Number of tokens is tunable

Cons:

- 1 Additional step is needed
