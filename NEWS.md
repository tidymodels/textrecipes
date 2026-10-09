# textrecipes (development version)

* `bake()` methods for steps that transform columns in place now use `recipes_map_cols()`, which avoids copying the data frame once per column and is much faster with many columns. Requires recipes >= 1.4.0. (#312)

* Corrected the documentation for `step_tf(weight_scheme = "term frequency")` and `step_tfidf(norm = "l1")` to state that the denominator is the number of in-vocabulary tokens, not the total number of words, in the document (#326)

* `step_tokenize()`, `step_tokenize_bpe()`, `step_tokenize_sentencepiece()`, and `step_stem()` now validate their `options` argument with `recipes::check_options()` and give informative errors for unnamed, non-list, or disallowed elements. (#291)

* Documentation now links to the external functions that do the underlying work in several steps, such as `janitor::make_clean_names()` in `step_clean_levels()` and `step_clean_names()`. (#198)

* Argument documentation has been revised across all steps to follow the tidyverse style guide: defaults are now stated, fixed sets of values are listed, code is formatted consistently, and the descriptions of `res`, `smooth_idf`, `norm`, and `vocabulary` are now accurate. (#263)

* Added a vignette, "Using textrecipes as a sparse matrix engine", documenting which steps produce sparse data and how to use them. (#302)

* Added a vignette, "Tokenizers", giving an overview of the tokenization options in the package and how they change the resulting tokens. (#158)

* The documentation for `step_tokenize()` now states that `training_options` also applies to `engine = "udpipe"`, where it must contain the loaded `model`, and includes an example. (#237)

## Bug Fixes

* All steps now work with 0 and 1 row data in `bake()`. This fixes errors in `step_texthash()`, `step_dummy_hash()`, `step_lda()`, and `step_pos_filter()` with 0 rows, and zero-length tokenlists no longer lose their lemma and pos attributes, which broke `step_lemma()`. (#290)

* Fixed documentation for `step_word_embeddings()` to describe the actual default `prefix` and column-naming behavior. (#331)

* Fixed a crash (segfault) in `step_texthash()` and `step_dummy_hash()` when `num_terms = 0` was used; this now errors cleanly during `prep()`. (#314)

* Fixed a bug in `step_texthash()` where baking with `sparse = "yes"` and `signed = TRUE` (the default) could error with "values value must not be equal to the default 0" when signed hash collisions cancelled out to a true zero. (#289)

* Fixed a bug in `step_dummy_hash()` where `keep_original_cols = TRUE` failed to restore the original columns when `collapse = TRUE`, and where the intermediate collapsed column could leak into the output. (#304)

* Fixed a bug in `step_clean_levels()` where a backwards-compatibility shim never actually restored the trained column names, causing `bake()` to silently skip cleaning for legacy trained objects. (#321)

* Fixed a bug in `step_clean_levels()` where character columns were never given a trained cleaning dictionary, causing `bake()` to clean row values inconsistently (via `janitor::make_clean_names()` applied to the batch of values being baked) instead of using a fixed lookup learned at `prep()` time. (#320)

* `step_lda()` produced non-deterministic and degenerate topic features at bake time: `bake()` re-fit topic assignments instead of projecting new documents onto the fitted model, and rebuilt the vocabulary from `new_data` alone with different pruning than was used at `prep()` time. `bake()` now reuses the vocabulary learned during `prep()` and projects each document independently, so topic weights sum to ~1 and no longer depend on what else is in the same `bake()` call. (#315)

* Custom tokenizer functions passed to `custom_token` are now called positionally, so functions whose first argument isn't named `x` no longer error with "unused argument" at bake time. (#248)

* `step_tokenize()`, `step_tokenize_bpe()`, `step_tokenize_sentencepiece()`, and `step_tokenize_wordpiece()` now error informatively if a tokenizer returns a list whose length doesn't match the number of rows in the input, instead of silently recycling and duplicating rows. (#318)

* Fixed a bug where `bake()` could error with "unused argument" for `step_tokenize(engine = "spacyr")`, `step_tokenize(engine = "tokenizers.bpe")`, `step_tokenize_bpe()`, and `step_tokenize_sentencepiece()` when `options` was non-empty, because the underlying tokenizer closures didn't accept additional arguments. (#319)

* `step_stem()` no longer silently drops the `lemma`/`pos` attributes carried by a tokenlist, so it can now be safely used upstream of `step_lemma()` or `step_pos_filter()` (#327).

* Fixed `first_person()`, `first_personp()`, `second_person()`, `second_personp()`, `third_person()`, `to_be()`, and `prepositions()` (used by `step_textfeature()`) which tested whether a word matched the *entire* untokenized document rather than whether the document contained that word, causing them to almost always return 0. They now tokenize the document into words before checking membership. (#316)

* Fixed several inconsistencies in the counting functions used by `step_textfeature()` (#329):
    - `n_uq_urls()` now counts distinct full urls instead of being capped at 2 distinct matches of the literal substring "http"/"https".
    - `n_charS()` and `n_uq_charS()` now actually exclude urls, hashtags, and mentions from the character count, as documented.
    - `n_extraspaces()` no longer counts a single tab or newline as "extra spaces"; it now only counts runs of 2 or more consecutive whitespace characters.
    - All counting functions now consistently return `NA` when given `NA` input.

* Fixed `show_tokens()` so that the `n` argument's upper bound (`nrow(rec$template)`) is actually validated, instead of being silently swallowed into `check_number_whole()`'s `...` and producing a confusing, unrelated error when validation failed. Added test coverage for `show_tokens()`, which previously had none. (#322)

* `step_ngram()` and `step_sequence_onehot()` now error clearly when given `num_tokens`, `min_num_tokens`, or `sequence_length` less than 1, and `step_ngram()` now errors clearly when `min_num_tokens` is greater than `num_tokens`, instead of surfacing an unrelated low-level error. (#324)

* `step_ngram()` no longer mislabels non-UTF-8 (e.g. latin1) input as UTF-8, which could cause `nchar()`, `toupper()`, and other string operations on the output to fail with encoding errors. (#323)

* Fixed bug in `step_tfidf()` where `sublinear_tf = TRUE` could produce negative TF-IDF values because the sublinear transform was applied after normalization instead of before. (#317)

# textrecipes 1.1.0

## Improvements

* The following steps has gained the argument `sparse`. When set to `"yes"`, they will produce sparse vectors. (#277)
    - `step_dummy_hash()`
    - `step_texthash()`
    - `step_tf()`
    - `step_tfidf()`

# textrecipes 1.0.7

## Improvements

* Documentation for tidy methods for all steps has been improved to describe the return value more accurately. (#262)

* Calling `?tidy.step_*()` now sends you to the documentation for `step_*()` where the outcome is documented. (#261)

* `step_textfeatures()` has been made faster and more robust. (#265)

## Bug Fixes

* Fixed bug in `step_clean_levels()` where it would produce NAs for character columns. (#274)

# textrecipes 1.0.6

* textfeatures has been removed from Suggests. (#255)

* `step_textfeatures()` no longer returns a politeness feature. (#254)

# textrecipes 1.0.5

* `step_untokenize()` and `step_normalization()` now returns factors instead of strings. (#247)

# textrecipes 1.0.4

## Improvements

* `step_clean_names()` now throw an informative error if needed non-standard role columns are missing during `bake()`. (#235)

* The `keep_original_cols` argument has been added to `step_tokenmerge`. This change should mean that every step that produces new columns has the `keep_original_cols` argument. (#242)

* Many internal changes to improve consistency and slight speed increases.

## Bug Fixes

* Fixed bug where `step_dummy_hash()` and `step_texthash()` would add new columns before old columns. (#235)

* Fixed bug where `vocabulary_size` wasn't tunable in `step_tokenize_bpe()`. (#239)

# textrecipes 1.0.3

## Improvements

* Steps with tunable arguments now have those arguments listed in the documentation.

* All steps that add new columns will now informatively error if name collision occurs.

## Bug Fixes

* Fixed bug where `step_tf()` wasn't tunable for `weight` argument.

# textrecipes 1.0.2

* Setting `token = "tweets"` in `step_tokenize()` have been deprecated due to `tokenizers::tokenize_tweets()` being deprecated. (#209)

* `step_sequence_onehot()`, `step_dummy_hash()`, `step_dummy_texthash()` now return integers. `step_tf()` returns integer when `weight_scheme` is `"binary"` or `"raw count"`.

* All steps now have `required_pkgs()` methods.

# textrecipes 1.0.1

* Examples no longer include `if (require(...))` code.

# textrecipes 1.0.0

* Indicate which steps support case weights (none), to align documentation with other packages.

# textrecipes 0.5.2

* Remove use of okc_text in vignette

* Fix bug in printing of tokenlists

# textrecipes 0.5.1

* `step_tfidf()` now correctly saves the idf values and applies them to the testing data set.

* `tidy.step_tfidf()` now returns calculated IDF weights.

# textrecipes 0.5.0

## New steps

* `step_dummy_hash()` generates binary indicators (possibly signed) from simple factor or character vectors. 

* `step_tokenize()` has gotten a couple of cousin functions `step_tokenize_bpe()`,  `step_tokenize_sentencepiece()` and `step_tokenize_wordpiece()` which wraps {tokenizers.bpe}, {sentencepiece} and {wordpiece} respectively (#147).

## Improvements and Other Changes

* Added `all_tokenized()` and `all_tokenized_predictors()` to more easily select tokenized columns (#132).

* Use `show_tokens()` to more easily debug a recipe involving tokenization.

* Reorganize documentation for all recipe step `tidy` methods (#126).

* Steps now have a dedicated subsection detailing what happens when `tidy()` is applied. (#163)

* All recipe steps now officially support empty selections to be more aligned with dplyr and other packages that use tidyselect (#141).

* `step_ngram()` has been given a speed increase to put it in line with other packages performance. 

* `step_tokenize()` will now try to error if vocabulary size is too low when using `engine = "tokenizers.bpe"` (#119).

* Warning given by `step_tokenfilter()` when filtering failed to apply now correctly refers to the right argument name (#137).

* `step_tf()` now returns 0 instead of NaN when there aren't any tokens present (#118).

* `step_tokenfilter()` now has a new argument `filter_fun` will takes a function which can be used to filter tokens. (#164)

* `tidy.step_stem()` now correctly shows if custom stemmer was used.

* Added `keep_original_cols` argument to `step_lda`, `step_texthash()`, `step_tf()`, `step_tfidf()`, `step_word_embeddings()`, `step_dummy_hash()`, `step_sequence_onehot()`, and `step_textfeatures()` (#139).

## Breaking Changes

* Steps with `prefix` argument now creates names according to the pattern `prefix_variablename_name/number`. (#124)

# textrecipes 0.4.1

## Bug fixes

* Fixed a bug in `step_tokenfilter()` and `step_sequence_onehot()` that sometimes caused crashes in R 4.1.0.

# textrecipes 0.4.0

## Breaking Changes

* `step_lda()` now takes a tokenlist instead of a character variable. See readme for more detail.

## New Features

* `step_sequence_onehot()` now takes tokenlists as input.
* added {tokenizers.bpe} engine to `step_tokenize()`.
* added {udpipe} engine to `step_tokenize()`.
* added new steps for cleaning variable names or levels with {janitor}, `step_clean_names()` and `step_clean_levels()`. (#101)

# textrecipes 0.3.0

* stopwords package have been moved from Imports to Suggests.
* `step_ngram()` gained an argument `min_num_tokens` to be able to return multiple n-grams together. (#90)
* Adds `step_text_normalization()` to perform unicode normalization on character vectors. (#86)

# textrecipes 0.2.3

# textrecipes 0.2.2

* `step_word_embeddings()` got a argument `aggregation_default` to specify value in cases where no words matches embedding.

# textrecipes 0.2.1

# textrecipes 0.2.0

* `step_tokenize()` got an `engine` argument to specify packages other then tokenizers to tokenize.
* `spacyr` have been added as an engine to `step_tokenize()`.
* `step_lemma()` has been added to extract lemma attribute from tokenlists.
* `step_pos_filter()` has been added to allow filtering of tokens bases on their pat of speech tags.
* `step_ngram()` has been added to generate ngrams from tokenlists.
* `step_stem()` not correctly uses the options argument. (Thanks to @grayskripko for finding bug, #64)

# textrecipes 0.1.0

* `step_word2vec()` have been changed to `step_lda()` to reflect what is actually happening.
* `step_word_embeddings()` has been added. Allows for use of pre-trained word embeddings to convert token columns to vectors in a high-dimensional "meaning" space. (@jonthegeek, #20)
* text2vec have been changed from Imports to Suggests.
* textfeatures have been changed from Imports to Suggests.
* `step_tfidf()` calculations are slightly changed due to flaw in original implementation https://github.com/dselivanov/text2vec/issues/280.

# textrecipes 0.0.2

* Custom stemming function can now be used in step_stem using the custom_stemmer argument.
* `step_textfeatures()` have been added, allows for multiple numerical features to be pulled from text.
* `step_sequence_onehot()` have been added, allows for one hot encoding of sequences of fixed width.
* `step_word2vec()` have been added, calculates word2vec dimensions.
* `step_tokenmerge()` have been added, combines multiple list columns into one list-columns.
* `step_texthash()` now correctly accepts `signed` argument.
* Documentation have been improved to showcase the importance of filtering tokens before applying `step_tf()` and `step_tfidf()`.

# textrecipes 0.0.1
 
First CRAN version
