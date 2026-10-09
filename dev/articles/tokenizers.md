# Tokenizers

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

Tokenization is the first step in almost every textrecipes workflow. It
splits a character column into smaller pieces, called tokens, that later
steps can filter, modify, and turn into numeric features. This vignette
gives an overview of the tokenization options in the package and how
each choice changes the tokens you get.

We will use two short texts throughout.

``` r

text_tibble <- tibble::tibble(
  text = c(
    "This is a sentence. It has two!",
    "They're e-mailing Dr. Smith about the 3 cats."
  )
)
```

## The basics

[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
converts a character column into a token column (see
[`?tokenlist`](https://textrecipes.tidymodels.org/dev/reference/tokenlist.md)).
The `token` argument picks the unit of tokenization and the `engine`
argument picks the package doing the work. With no arguments you get
word tokens from the tokenizers package.

``` r

recipe(~text, data = text_tibble) |>
  step_tokenize(text) |>
  show_tokens(text, n = 2)
#> [[1]]
#> [1] "this"     "is"       "a"        "sentence" "it"       "has"     
#> [7] "two"     
#> 
#> [[2]]
#> [1] "they're" "e"       "mailing" "dr"      "smith"   "about"  
#> [7] "the"     "3"       "cats"
```

[`show_tokens()`](https://textrecipes.tidymodels.org/dev/reference/show_tokens.md)
is a convenient way to see what a recipe produces. Each element of the
result is the token vector for one row.

## Engines and tokens

The `engine` determines which values of `token` are available.

| Engine | Tokens | Trained on data |
|:---|:---|:---|
| `tokenizers` | `"words"`, `"characters"`, `"character_shingle"`, `"ngrams"`, `"skip_ngrams"`, `"sentences"`, `"lines"`, `"paragraphs"`, `"regex"`, `"ptb"`, `"word_stems"` | No |
| `spacyr` | `"words"` | No |
| `tokenizers.bpe` | `"words"` | Yes |
| `udpipe` | `"words"` | No (needs a pre-trained model) |

Subword tokenizers such as wordpiece, sentencepiece, and byte pair
encoding also have dedicated steps:
[`step_tokenize_wordpiece()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_wordpiece.md),
[`step_tokenize_sentencepiece()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_sentencepiece.md),
and
[`step_tokenize_bpe()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_bpe.md).

## The tokenizers engine

The tokenizers engine is the default. Every `token` option corresponds
to a function in the tokenizers package, and the `options` argument
takes a named list of arguments passed to that function.

### Words

Word tokenization lowercases the text and removes punctuation by
default.

``` r

recipe(~text, data = text_tibble) |>
  step_tokenize(text, token = "words") |>
  show_tokens(text, n = 2)
#> [[1]]
#> [1] "this"     "is"       "a"        "sentence" "it"       "has"     
#> [7] "two"     
#> 
#> [[2]]
#> [1] "they're" "e"       "mailing" "dr"      "smith"   "about"  
#> [7] "the"     "3"       "cats"
```

Both behaviors are controlled through `options`, see
[`?tokenizers::tokenize_words`](https://docs.ropensci.org/tokenizers/reference/basic-tokenizers.html).
Here we keep the original case and the punctuation.

``` r

recipe(~text, data = text_tibble) |>
  step_tokenize(
    text,
    options = list(lowercase = FALSE, strip_punct = FALSE)
  ) |>
  show_tokens(text, n = 2)
#> [[1]]
#> [1] "This"     "is"       "a"        "sentence" "."        "It"      
#> [7] "has"      "two"      "!"       
#> 
#> [[2]]
#>  [1] "They're" "e"       "-"       "mailing" "Dr"      "."      
#>  [7] "Smith"   "about"   "the"     "3"       "cats"    "."
```

Note that `"e-mailing"` is split at the hyphen into `"e"` and
`"mailing"` by default. Numbers can be removed by setting
`strip_numeric = TRUE`.

``` r

recipe(~text, data = text_tibble) |>
  step_tokenize(text, options = list(strip_numeric = TRUE)) |>
  show_tokens(text, n = 2)
#> [[1]]
#> [1] "this"     "is"       "a"        "sentence" "it"       "has"     
#> [7] "two"     
#> 
#> [[2]]
#> [1] "they're" "e"       "mailing" "dr"      "smith"   "about"  
#> [7] "the"     "cats"
```

### Penn Treebank

The `"ptb"` tokenizer follows the Penn Treebank conventions. It splits
contractions such as `"They're"` into `"They"` and `"'re"` and keeps
hyphenated words intact, which is often what you want for linguistic
work.

``` r

recipe(~text, data = text_tibble) |>
  step_tokenize(text, token = "ptb") |>
  show_tokens(text, n = 2)
#> [[1]]
#> [1] "This"      "is"        "a"         "sentence." "It"       
#> [6] "has"       "two"       "!"        
#> 
#> [[2]]
#>  [1] "They"      "'re"       "e-mailing" "Dr."       "Smith"    
#>  [6] "about"     "the"       "3"         "cats"      "."
```

### Characters and character shingles

`"characters"` returns single characters, and `"character_shingle"`
returns overlapping character n-grams whose size is set with `n`.

``` r

recipe(~text, data = text_tibble) |>
  step_tokenize(text, token = "characters") |>
  show_tokens(text, n = 2)
#> [[1]]
#>  [1] "t" "h" "i" "s" "i" "s" "a" "s" "e" "n" "t" "e" "n" "c" "e" "i"
#> [17] "t" "h" "a" "s" "t" "w" "o"
#> 
#> [[2]]
#>  [1] "t" "h" "e" "y" "r" "e" "e" "m" "a" "i" "l" "i" "n" "g" "d" "r"
#> [17] "s" "m" "i" "t" "h" "a" "b" "o" "u" "t" "t" "h" "e" "3" "c" "a"
#> [33] "t" "s"
```

``` r

recipe(~text, data = text_tibble) |>
  step_tokenize(
    text,
    token = "character_shingle",
    options = list(n = 3)
  ) |>
  show_tokens(text, n = 2)
#> [[1]]
#>  [1] "thi" "his" "isi" "sis" "isa" "sas" "ase" "sen" "ent" "nte" "ten"
#> [12] "enc" "nce" "cei" "eit" "ith" "tha" "has" "ast" "stw" "two"
#> 
#> [[2]]
#>  [1] "the" "hey" "eyr" "yre" "ree" "eem" "ema" "mai" "ail" "ili" "lin"
#> [12] "ing" "ngd" "gdr" "drs" "rsm" "smi" "mit" "ith" "tha" "hab" "abo"
#> [23] "bou" "out" "utt" "tth" "the" "he3" "e3c" "3ca" "cat" "ats"
```

Character-level tokens are useful for short strings with typos or
without clear word boundaries, but they are rarely appropriate for long
documents.

### N-grams and skip n-grams

`"ngrams"` returns word n-grams. Use `n` and `n_min` to choose the range
of sizes.

``` r

recipe(~text, data = text_tibble) |>
  step_tokenize(text, token = "ngrams", options = list(n = 2, n_min = 1)) |>
  show_tokens(text, n = 2)
#> [[1]]
#>  [1] "this"        "this is"     "is"          "is a"       
#>  [5] "a"           "a sentence"  "sentence"    "sentence it"
#>  [9] "it"          "it has"      "has"         "has two"    
#> [13] "two"        
#> 
#> [[2]]
#>  [1] "they're"     "they're e"   "e"           "e mailing"  
#>  [5] "mailing"     "mailing dr"  "dr"          "dr smith"   
#>  [9] "smith"       "smith about" "about"       "about the"  
#> [13] "the"         "the 3"       "3"           "3 cats"     
#> [17] "cats"
```

`"skip_ngrams"` allows gaps between the words, controlled by `k`.

``` r

recipe(~text, data = text_tibble) |>
  step_tokenize(
    text,
    token = "skip_ngrams",
    options = list(n = 2, n_min = 2, k = 1)
  ) |>
  show_tokens(text, n = 2)
#> [[1]]
#>  [1] "this is"      "this a"       "is a"         "is sentence" 
#>  [5] "a sentence"   "a it"         "sentence it"  "sentence has"
#>  [9] "it has"       "it two"       "has two"     
#> 
#> [[2]]
#>  [1] "they're e"       "they're mailing" "e mailing"      
#>  [4] "e dr"            "mailing dr"      "mailing smith"  
#>  [7] "dr smith"        "dr about"        "smith about"    
#> [10] "smith the"       "about the"       "about 3"        
#> [13] "the 3"           "the cats"        "3 cats"
```

If you want n-grams of tokens you have already modified, for example
after removing stop words, tokenize to words first and use
[`step_ngram()`](https://textrecipes.tidymodels.org/dev/reference/step_ngram.md)
instead. See
[`vignette("Working-with-n-grams")`](https://textrecipes.tidymodels.org/dev/articles/Working-with-n-grams.md).

### Sentences, lines, and paragraphs

These tokenizers split at larger boundaries. They are mostly useful as a
first step before splitting further, or when each sentence or paragraph
is the unit you want to model.

``` r

recipe(~text, data = text_tibble) |>
  step_tokenize(text, token = "sentences") |>
  show_tokens(text, n = 2)
#> [[1]]
#> [1] "This is a sentence." "It has two!"        
#> 
#> [[2]]
#> [1] "They're e-mailing Dr."   "Smith about the 3 cats."
```

`"lines"` splits on line breaks and `"paragraphs"` on blank lines.

### Regular expressions

`"regex"` splits the text wherever a pattern matches. The pattern is
given with `pattern`, and the default is a single space.

``` r

recipe(~text, data = text_tibble) |>
  step_tokenize(text, token = "regex", options = list(pattern = "[.!]\\s*")) |>
  show_tokens(text, n = 2)
#> [[1]]
#> [1] "This is a sentence" "It has two"        
#> 
#> [[2]]
#> [1] "They're e-mailing Dr"   "Smith about the 3 cats"
```

### Word stems

`"word_stems"` tokenizes into words and stems each of them in one go. If
you want to control the stemmer, or stem after other filtering, tokenize
to words and use
[`step_stem()`](https://textrecipes.tidymodels.org/dev/reference/step_stem.md).

``` r

recipe(~text, data = text_tibble) |>
  step_tokenize(text, token = "word_stems") |>
  show_tokens(text, n = 2)
#> [[1]]
#> [1] "this"    "is"      "a"       "sentenc" "it"      "has"    
#> [7] "two"    
#> 
#> [[2]]
#> [1] "they'r" "e"      "mail"   "dr"     "smith"  "about"  "the"   
#> [8] "3"      "cat"
```

## Training a tokenizer

Some tokenizers learn their vocabulary from the data. Their training
arguments are passed in `training_options`, while `options` is used when
the tokenizer is applied. The tokenizers.bpe engine learns a byte pair
encoding on the training set, and `vocab_size` is the most important
training option. It is usually in the thousands, but is kept small here
to fit our tiny data.

``` r

recipe(~text, data = text_tibble) |>
  step_tokenize(
    text,
    engine = "tokenizers.bpe",
    training_options = list(vocab_size = 40)
  ) |>
  show_tokens(text, n = 2)
```

Because the vocabulary is learned during
[`prep()`](https://recipes.tidymodels.org/reference/prep.html), the same
tokenizer is applied to new data when you
[`bake()`](https://recipes.tidymodels.org/reference/bake.html). The
dedicated steps
[`step_tokenize_bpe()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_bpe.md),
[`step_tokenize_sentencepiece()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_sentencepiece.md),
and
[`step_tokenize_wordpiece()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_wordpiece.md)
give you the same kind of subword tokens with arguments specific to each
method.

## Other engines

The spacyr engine tokenizes with spaCy and requires a working Python
installation with spaCy. Its options are applied each time the data is
tokenized, including at
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) time.

``` r

recipe(~text, data = text_tibble) |>
  step_tokenize(text, engine = "spacyr") |>
  show_tokens(text, n = 2)
```

The udpipe engine requires a pre-trained model, loaded with
[`udpipe::udpipe_load_model()`](https://rdrr.io/pkg/udpipe/man/udpipe_load_model.html),
passed as `model` in `training_options`.

``` r

model <- udpipe::udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

recipe(~text, data = text_tibble) |>
  step_tokenize(
    text,
    engine = "udpipe",
    training_options = list(model = model)
  ) |>
  show_tokens(text, n = 2)
```

## Custom tokenizers

When none of the engines do what you need, pass your own function to
`custom_token`. It must take a character vector and return a list of
character vectors with one element per input.

``` r

space_tokenizer <- function(x) {
  strsplit(x, " +")
}

recipe(~text, data = text_tibble) |>
  step_tokenize(text, custom_token = space_tokenizer) |>
  show_tokens(text, n = 2)
#> [[1]]
#> [1] "This"      "is"        "a"         "sentence." "It"       
#> [6] "has"       "two!"     
#> 
#> [[2]]
#> [1] "They're"   "e-mailing" "Dr."       "Smith"     "about"    
#> [6] "the"       "3"         "cats."
```

## Choosing a tokenizer

There is rarely a single correct choice, and the best tokenizer depends
on your data and model. Some rules of thumb:

- Start with `"words"`. It is fast, easy to interpret, and works well
  for many bag-of-words models.
- Use `"ptb"` or the spacyr and udpipe engines when linguistic detail
  such as contractions or part of speech matters.
- Use character-based tokens or subword tokenizers for noisy text, many
  languages, or when out-of-vocabulary words are a problem.
- Use a custom tokenizer for domain-specific formats such as hashtags,
  code, or identifiers.

Since the tokenizer is a recipe argument, `token` can also be tuned with
the tunable method for
[`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md).

## Next steps

After tokenizing you will typically filter and modify the tokens, for
example with
[`step_stopwords()`](https://textrecipes.tidymodels.org/dev/reference/step_stopwords.md),
[`step_stem()`](https://textrecipes.tidymodels.org/dev/reference/step_stem.md),
[`step_tokenfilter()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenfilter.md),
or
[`step_ngram()`](https://textrecipes.tidymodels.org/dev/reference/step_ngram.md),
and then turn them into numeric features with
[`step_tf()`](https://textrecipes.tidymodels.org/dev/reference/step_tf.md),
[`step_tfidf()`](https://textrecipes.tidymodels.org/dev/reference/step_tfidf.md),
or
[`step_texthash()`](https://textrecipes.tidymodels.org/dev/reference/step_texthash.md).
See
[`vignette("cookbook---using-more-complex-recipes-involving-text")`](https://textrecipes.tidymodels.org/dev/articles/cookbook---using-more-complex-recipes-involving-text.md)
for complete examples.
