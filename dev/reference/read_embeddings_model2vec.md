# Read Pretrained Static Embeddings From Files

These functions read downloaded static embeddings into the tibble that
the `embeddings` argument of
[`step_word_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_word_embeddings.md)
expects: the first column holds the tokens and the other columns hold
the vectors. They read files that are already on disk and do not
download anything. Use
[`download_embeddings_model2vec()`](https://textrecipes.tidymodels.org/dev/reference/download_embeddings_model2vec.md)
to download a Model2Vec model first.

## Usage

``` r
read_embeddings_model2vec(path)

read_embeddings_bpemb(path)
```

## Arguments

- path:

  For `read_embeddings_model2vec()`, a string giving the path to a
  folder holding a Model2Vec model, which must contain the files
  `model.safetensors` and `vocab.txt`. For `read_embeddings_bpemb()`, a
  string giving the path to an unpacked BPEmb vectors file, which ends
  in `.w2v.txt`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with a
character column `token` followed by one numeric column for each
dimension of the embeddings.

## Details

The vectors belong to the tokens of one specific tokenizer, and tokens
that are not found in the table are dropped without a warning. The text
therefore has to be tokenized in the same way the model was trained. See
[`vignette("static-embeddings", package = "textrecipes")`](https://textrecipes.tidymodels.org/dev/articles/static-embeddings.md)
for the recipes to use with each model, and for where to download the
files.

### Model2Vec

`read_embeddings_model2vec()` reads the `embeddings` tensor of
`model.safetensors` and uses the lines of `vocab.txt` as the tokens.
This needs the safetensors and torch packages. Models that store
quantized weights are not supported. The columns of the vectors are
named `d1`, `d2`, and so on.

### BPEmb

`read_embeddings_bpemb()` reads the word2vec text format, where the
first line gives the number of rows and columns, and each other line is
a token followed by its numbers. The columns of the vectors are named
`d1`, `d2`, and so on.

## See also

[`step_word_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_word_embeddings.md)
to use the embeddings in a recipe.

## Examples

``` r
# A small file in the BPEmb format
file <- tempfile(fileext = ".w2v.txt")
writeLines(
  c("3 2", "<unk> 0.1 0.2", "▁the 0.3 0.4", "▁cat 0.5 0.6"),
  file
)

read_embeddings_bpemb(file)
#> # A tibble: 3 × 3
#>   token    d1    d2
#>   <chr> <dbl> <dbl>
#> 1 <unk>   0.1   0.2
#> 2 ▁the    0.3   0.4
#> 3 ▁cat    0.5   0.6
```
