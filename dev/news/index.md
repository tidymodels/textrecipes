# Changelog

## textrecipes (development version)

## textrecipes 1.1.0

CRAN release: 2025-03-18

### Improvements

- The following steps has gained the argument `sparse`. When set to
  `"yes"`, they will produce sparse vectors.
  ([\#277](https://github.com/tidymodels/textrecipes/issues/277))
  - [`step_dummy_hash()`](https://textrecipes.tidymodels.org/dev/reference/step_dummy_hash.md)
  - [`step_texthash()`](https://textrecipes.tidymodels.org/dev/reference/step_texthash.md)
  - [`step_tf()`](https://textrecipes.tidymodels.org/dev/reference/step_tf.md)
  - [`step_tfidf()`](https://textrecipes.tidymodels.org/dev/reference/step_tfidf.md)

## textrecipes 1.0.7

CRAN release: 2025-01-23

### Improvements

- Documentation for tidy methods for all steps has been improved to
  describe the return value more accurately.
  ([\#262](https://github.com/tidymodels/textrecipes/issues/262))

- Calling `?tidy.step_*()` now sends you to the documentation for
  `step_*()` where the outcome is documented.
  ([\#261](https://github.com/tidymodels/textrecipes/issues/261))

- `step_textfeatures()` has been made faster and more robust.
  ([\#265](https://github.com/tidymodels/textrecipes/issues/265))

### Bug Fixes

- Fixed bug in
  [`step_clean_levels()`](https://textrecipes.tidymodels.org/dev/reference/step_clean_levels.md)
  where it would produce NAs for character columns.
  ([\#274](https://github.com/tidymodels/textrecipes/issues/274))

## textrecipes 1.0.6

CRAN release: 2023-11-15

- textfeatures has been removed from Suggests.
  ([\#255](https://github.com/tidymodels/textrecipes/issues/255))

- `step_textfeatures()` no longer returns a politeness feature.
  ([\#254](https://github.com/tidymodels/textrecipes/issues/254))

## textrecipes 1.0.5

CRAN release: 2023-10-20

- [`step_untokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_untokenize.md)
  and `step_normalization()` now returns factors instead of strings.
  ([\#247](https://github.com/tidymodels/textrecipes/issues/247))

## textrecipes 1.0.4

CRAN release: 2023-08-17

### Improvements

- [`step_clean_names()`](https://textrecipes.tidymodels.org/dev/reference/step_clean_names.md)
  now throw an informative error if needed non-standard role columns are
  missing during
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html).
  ([\#235](https://github.com/tidymodels/textrecipes/issues/235))

- The `keep_original_cols` argument has been added to `step_tokenmerge`.
  This change should mean that every step that produces new columns has
  the `keep_original_cols` argument.
  ([\#242](https://github.com/tidymodels/textrecipes/issues/242))

- Many internal changes to improve consistency and slight speed
  increases.

### Bug Fixes

- Fixed bug where
  [`step_dummy_hash()`](https://textrecipes.tidymodels.org/dev/reference/step_dummy_hash.md)
  and
  [`step_texthash()`](https://textrecipes.tidymodels.org/dev/reference/step_texthash.md)
  would add new columns before old columns.
  ([\#235](https://github.com/tidymodels/textrecipes/issues/235))

- Fixed bug where `vocabulary_size` wasn’t tunable in
  [`step_tokenize_bpe()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_bpe.md).
  ([\#239](https://github.com/tidymodels/textrecipes/issues/239))

## textrecipes 1.0.3

CRAN release: 2023-04-14

### Improvements

- Steps with tunable arguments now have those arguments listed in the
  documentation.

- All steps that add new columns will now informatively error if name
  collision occurs.

### Bug Fixes

- Fixed bug where
  [`step_tf()`](https://textrecipes.tidymodels.org/dev/reference/step_tf.md)
  wasn’t tunable for `weight` argument.

## textrecipes 1.0.2

CRAN release: 2022-12-21

- Setting `token = "tweets"` in
  [`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
  have been deprecated due to `tokenizers::tokenize_tweets()` being
  deprecated.
  ([\#209](https://github.com/tidymodels/textrecipes/issues/209))

- [`step_sequence_onehot()`](https://textrecipes.tidymodels.org/dev/reference/step_sequence_onehot.md),
  [`step_dummy_hash()`](https://textrecipes.tidymodels.org/dev/reference/step_dummy_hash.md),
  `step_dummy_texthash()` now return integers.
  [`step_tf()`](https://textrecipes.tidymodels.org/dev/reference/step_tf.md)
  returns integer when `weight_scheme` is `"binary"` or `"raw count"`.

- All steps now have
  [`required_pkgs()`](https://generics.r-lib.org/reference/required_pkgs.html)
  methods.

## textrecipes 1.0.1

CRAN release: 2022-10-06

- Examples no longer include `if (require(...))` code.

## textrecipes 1.0.0

CRAN release: 2022-07-02

- Indicate which steps support case weights (none), to align
  documentation with other packages.

## textrecipes 0.5.2

CRAN release: 2022-05-04

- Remove use of okc_text in vignette

- Fix bug in printing of tokenlists

## textrecipes 0.5.1

CRAN release: 2022-03-29

- [`step_tfidf()`](https://textrecipes.tidymodels.org/dev/reference/step_tfidf.md)
  now correctly saves the idf values and applies them to the testing
  data set.

- [`tidy.step_tfidf()`](https://textrecipes.tidymodels.org/dev/reference/step_tfidf.md)
  now returns calculated IDF weights.

## textrecipes 0.5.0

CRAN release: 2022-03-20

### New steps

- [`step_dummy_hash()`](https://textrecipes.tidymodels.org/dev/reference/step_dummy_hash.md)
  generates binary indicators (possibly signed) from simple factor or
  character vectors.

- [`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
  has gotten a couple of cousin functions
  [`step_tokenize_bpe()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_bpe.md),
  [`step_tokenize_sentencepiece()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_sentencepiece.md)
  and
  [`step_tokenize_wordpiece()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize_wordpiece.md)
  which wraps {tokenizers.bpe}, {sentencepiece} and {wordpiece}
  respectively
  ([\#147](https://github.com/tidymodels/textrecipes/issues/147)).

### Improvements and Other Changes

- Added
  [`all_tokenized()`](https://textrecipes.tidymodels.org/dev/reference/all_tokenized.md)
  and
  [`all_tokenized_predictors()`](https://textrecipes.tidymodels.org/dev/reference/all_tokenized.md)
  to more easily select tokenized columns
  ([\#132](https://github.com/tidymodels/textrecipes/issues/132)).

- Use
  [`show_tokens()`](https://textrecipes.tidymodels.org/dev/reference/show_tokens.md)
  to more easily debug a recipe involving tokenization.

- Reorganize documentation for all recipe step `tidy` methods
  ([\#126](https://github.com/tidymodels/textrecipes/issues/126)).

- Steps now have a dedicated subsection detailing what happens when
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) is applied.
  ([\#163](https://github.com/tidymodels/textrecipes/issues/163))

- All recipe steps now officially support empty selections to be more
  aligned with dplyr and other packages that use tidyselect
  ([\#141](https://github.com/tidymodels/textrecipes/issues/141)).

- [`step_ngram()`](https://textrecipes.tidymodels.org/dev/reference/step_ngram.md)
  has been given a speed increase to put it in line with other packages
  performance.

- [`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
  will now try to error if vocabulary size is too low when using
  `engine = "tokenizers.bpe"`
  ([\#119](https://github.com/tidymodels/textrecipes/issues/119)).

- Warning given by
  [`step_tokenfilter()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenfilter.md)
  when filtering failed to apply now correctly refers to the right
  argument name
  ([\#137](https://github.com/tidymodels/textrecipes/issues/137)).

- [`step_tf()`](https://textrecipes.tidymodels.org/dev/reference/step_tf.md)
  now returns 0 instead of NaN when there aren’t any tokens present
  ([\#118](https://github.com/tidymodels/textrecipes/issues/118)).

- [`step_tokenfilter()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenfilter.md)
  now has a new argument `filter_fun` will takes a function which can be
  used to filter tokens.
  ([\#164](https://github.com/tidymodels/textrecipes/issues/164))

- [`tidy.step_stem()`](https://textrecipes.tidymodels.org/dev/reference/step_stem.md)
  now correctly shows if custom stemmer was used.

- Added `keep_original_cols` argument to `step_lda`,
  [`step_texthash()`](https://textrecipes.tidymodels.org/dev/reference/step_texthash.md),
  [`step_tf()`](https://textrecipes.tidymodels.org/dev/reference/step_tf.md),
  [`step_tfidf()`](https://textrecipes.tidymodels.org/dev/reference/step_tfidf.md),
  [`step_word_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_word_embeddings.md),
  [`step_dummy_hash()`](https://textrecipes.tidymodels.org/dev/reference/step_dummy_hash.md),
  [`step_sequence_onehot()`](https://textrecipes.tidymodels.org/dev/reference/step_sequence_onehot.md),
  and `step_textfeatures()`
  ([\#139](https://github.com/tidymodels/textrecipes/issues/139)).

### Breaking Changes

- Steps with `prefix` argument now creates names according to the
  pattern `prefix_variablename_name/number`.
  ([\#124](https://github.com/tidymodels/textrecipes/issues/124))

## textrecipes 0.4.1

CRAN release: 2021-07-11

### Bug fixes

- Fixed a bug in
  [`step_tokenfilter()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenfilter.md)
  and
  [`step_sequence_onehot()`](https://textrecipes.tidymodels.org/dev/reference/step_sequence_onehot.md)
  that sometimes caused crashes in R 4.1.0.

## textrecipes 0.4.0

CRAN release: 2020-11-12

### Breaking Changes

- [`step_lda()`](https://textrecipes.tidymodels.org/dev/reference/step_lda.md)
  now takes a tokenlist instead of a character variable. See readme for
  more detail.

### New Features

- [`step_sequence_onehot()`](https://textrecipes.tidymodels.org/dev/reference/step_sequence_onehot.md)
  now takes tokenlists as input.
- added {tokenizers.bpe} engine to
  [`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md).
- added {udpipe} engine to
  [`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md).
- added new steps for cleaning variable names or levels with {janitor},
  [`step_clean_names()`](https://textrecipes.tidymodels.org/dev/reference/step_clean_names.md)
  and
  [`step_clean_levels()`](https://textrecipes.tidymodels.org/dev/reference/step_clean_levels.md).
  ([\#101](https://github.com/tidymodels/textrecipes/issues/101))

## textrecipes 0.3.0

CRAN release: 2020-07-08

- stopwords package have been moved from Imports to Suggests.
- [`step_ngram()`](https://textrecipes.tidymodels.org/dev/reference/step_ngram.md)
  gained an argument `min_num_tokens` to be able to return multiple
  n-grams together.
  ([\#90](https://github.com/tidymodels/textrecipes/issues/90))
- Adds
  [`step_text_normalization()`](https://textrecipes.tidymodels.org/dev/reference/step_text_normalization.md)
  to perform unicode normalization on character vectors.
  ([\#86](https://github.com/tidymodels/textrecipes/issues/86))

## textrecipes 0.2.3

CRAN release: 2020-05-22

## textrecipes 0.2.2

CRAN release: 2020-05-10

- [`step_word_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_word_embeddings.md)
  got a argument `aggregation_default` to specify value in cases where
  no words matches embedding.

## textrecipes 0.2.1

CRAN release: 2020-05-04

## textrecipes 0.2.0

CRAN release: 2020-04-14

- [`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md)
  got an `engine` argument to specify packages other then tokenizers to
  tokenize.
- `spacyr` have been added as an engine to
  [`step_tokenize()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenize.md).
- [`step_lemma()`](https://textrecipes.tidymodels.org/dev/reference/step_lemma.md)
  has been added to extract lemma attribute from tokenlists.
- [`step_pos_filter()`](https://textrecipes.tidymodels.org/dev/reference/step_pos_filter.md)
  has been added to allow filtering of tokens bases on their pat of
  speech tags.
- [`step_ngram()`](https://textrecipes.tidymodels.org/dev/reference/step_ngram.md)
  has been added to generate ngrams from tokenlists.
- [`step_stem()`](https://textrecipes.tidymodels.org/dev/reference/step_stem.md)
  not correctly uses the options argument. (Thanks to
  [@grayskripko](https://github.com/grayskripko) for finding bug,
  [\#64](https://github.com/tidymodels/textrecipes/issues/64))

## textrecipes 0.1.0

CRAN release: 2020-03-05

- `step_word2vec()` have been changed to
  [`step_lda()`](https://textrecipes.tidymodels.org/dev/reference/step_lda.md)
  to reflect what is actually happening.
- [`step_word_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_word_embeddings.md)
  has been added. Allows for use of pre-trained word embeddings to
  convert token columns to vectors in a high-dimensional “meaning”
  space. ([@jonthegeek](https://github.com/jonthegeek),
  [\#20](https://github.com/tidymodels/textrecipes/issues/20))
- text2vec have been changed from Imports to Suggests.
- textfeatures have been changed from Imports to Suggests.
- [`step_tfidf()`](https://textrecipes.tidymodels.org/dev/reference/step_tfidf.md)
  calculations are slightly changed due to flaw in original
  implementation <https://github.com/dselivanov/text2vec/issues/280>.

## textrecipes 0.0.2

CRAN release: 2019-09-07

- Custom stemming function can now be used in step_stem using the
  custom_stemmer argument.
- `step_textfeatures()` have been added, allows for multiple numerical
  features to be pulled from text.
- [`step_sequence_onehot()`](https://textrecipes.tidymodels.org/dev/reference/step_sequence_onehot.md)
  have been added, allows for one hot encoding of sequences of fixed
  width.
- `step_word2vec()` have been added, calculates word2vec dimensions.
- [`step_tokenmerge()`](https://textrecipes.tidymodels.org/dev/reference/step_tokenmerge.md)
  have been added, combines multiple list columns into one list-columns.
- [`step_texthash()`](https://textrecipes.tidymodels.org/dev/reference/step_texthash.md)
  now correctly accepts `signed` argument.
- Documentation have been improved to showcase the importance of
  filtering tokens before applying
  [`step_tf()`](https://textrecipes.tidymodels.org/dev/reference/step_tf.md)
  and
  [`step_tfidf()`](https://textrecipes.tidymodels.org/dev/reference/step_tfidf.md).

## textrecipes 0.0.1

CRAN release: 2018-12-17

First CRAN version
