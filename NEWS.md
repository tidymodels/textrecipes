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
