# S3 methods for tracking which additional packages are needed for steps.

Recipe-adjacent packages always list themselves as a required package so
that the steps can function properly within parallel processing schemes.

## Usage

``` r
# S3 method for class 'step_clean_levels'
required_pkgs(x, ...)

# S3 method for class 'step_clean_names'
required_pkgs(x, ...)

# S3 method for class 'step_dummy_hash'
required_pkgs(x, ...)

# S3 method for class 'step_lda'
required_pkgs(x, ...)

# S3 method for class 'step_lemma'
required_pkgs(x, ...)

# S3 method for class 'step_ngram'
required_pkgs(x, ...)

# S3 method for class 'step_pos_filter'
required_pkgs(x, ...)

# S3 method for class 'step_sequence_onehot'
required_pkgs(x, ...)

# S3 method for class 'step_stem'
required_pkgs(x, ...)

# S3 method for class 'step_stopwords'
required_pkgs(x, ...)

# S3 method for class 'step_text_normalization'
required_pkgs(x, ...)

# S3 method for class 'step_textfeature'
required_pkgs(x, ...)

# S3 method for class 'step_texthash'
required_pkgs(x, ...)

# S3 method for class 'step_tf'
required_pkgs(x, ...)

# S3 method for class 'step_tfidf'
required_pkgs(x, ...)

# S3 method for class 'step_tokenfilter'
required_pkgs(x, ...)

# S3 method for class 'step_tokenize'
required_pkgs(x, ...)

# S3 method for class 'step_tokenize_bpe'
required_pkgs(x, ...)

# S3 method for class 'step_tokenize_sentencepiece'
required_pkgs(x, ...)

# S3 method for class 'step_tokenize_wordpiece'
required_pkgs(x, ...)

# S3 method for class 'step_tokenmerge'
required_pkgs(x, ...)

# S3 method for class 'step_untokenize'
required_pkgs(x, ...)

# S3 method for class 'step_word_embeddings'
required_pkgs(x, ...)
```

## Arguments

- x:

  A recipe step

## Value

A character vector
