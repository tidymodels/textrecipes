# tunable methods for textrecipes

These functions define what parameters *can* be tuned for specific
steps. They also define the recommended objects from the `dials` package
that can be used to generate new parameter values and other
characteristics.

## Usage

``` r
# S3 method for class 'step_dummy_hash'
tunable(x, ...)

# S3 method for class 'step_ngram'
tunable(x, ...)

# S3 method for class 'step_texthash'
tunable(x, ...)

# S3 method for class 'step_tf'
tunable(x, ...)

# S3 method for class 'step_tokenfilter'
tunable(x, ...)

# S3 method for class 'step_tokenize'
tunable(x, ...)

# S3 method for class 'step_tokenize_bpe'
tunable(x, ...)
```

## Arguments

- x:

  A recipe step object

- ...:

  Not used.

## Value

A tibble object.
