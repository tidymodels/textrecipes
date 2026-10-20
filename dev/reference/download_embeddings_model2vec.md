# Download a Pretrained Model2Vec Model

`download_embeddings_model2vec()` downloads the files of a
[Model2Vec](https://github.com/MinishLab/model2vec) model, so they can
be read with
[`read_embeddings_model2vec()`](https://textrecipes.tidymodels.org/dev/reference/read_embeddings_model2vec.md).

## Usage

``` r
download_embeddings_model2vec(url, path = tempfile("model2vec-"))
```

## Arguments

- url:

  A string giving the URL of the folder that holds the model files. For
  a model on the Hugging Face Hub this has the form
  `https://huggingface.co/<owner>/<model>/resolve/<revision>`, where
  `<revision>` is a branch name such as `main` or a commit hash. Use a
  commit hash to always get the same version of the model.

- path:

  A string giving the path to the folder to download the files into. The
  folder is created if it doesn't exist, and files in it with the same
  names are overwritten. Defaults to a new temporary folder, which is
  removed when the R session ends.

## Value

The path to the folder holding the files, invisibly.

## Details

Two files are downloaded: `model.safetensors`, which holds the vectors,
and `vocab.txt`, which holds the tokens. The files of a small model are
about 30 MB, so the download can take a while. The downloaded files are
not needed once a recipe using the embeddings has been prepped.

The embeddings are tied to the tokenizer the model was trained with. See
[`vignette("static-embeddings", package = "textrecipes")`](https://textrecipes.tidymodels.org/dev/articles/static-embeddings.md)
for how to use a downloaded model in a recipe.

## See also

[`read_embeddings_model2vec()`](https://textrecipes.tidymodels.org/dev/reference/read_embeddings_model2vec.md)
to read the downloaded files.

## Examples

``` r
if (FALSE) {
url <- paste0(
  "https://huggingface.co/minishlab/potion-base-8M/resolve/",
  "bf8b056651a2c21b8d2565580b8569da283cab23"
)

model_dir <- download_embeddings_model2vec(url)
embeddings <- read_embeddings_model2vec(model_dir)
}
```
