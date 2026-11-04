# Model2Vec Embedding Engine

`embedding_model2vec()` creates an engine for
[`step_sentence_embeddings()`](https://textrecipes.tidymodels.org/reference/step_sentence_embeddings.md)
that embeds text with a
[Model2Vec](https://github.com/MinishLab/model2vec) model. The model is
a table of token vectors that is read from disk and used in R, so it
needs neither Python nor a server, and text never leaves your machine.

## Usage

``` r
embedding_model2vec(path, normalize = TRUE, max_tokens = 512)
```

## Arguments

- path:

  A single string giving the path to a folder holding a Model2Vec model,
  which must contain the files `model.safetensors` and `vocab.txt`.
  [`download_embeddings_model2vec()`](https://textrecipes.tidymodels.org/reference/download_embeddings_model2vec.md)
  downloads such a folder. The path is expanded and stored in the
  engine.

- normalize:

  A single logical. If `TRUE`, each embedding is scaled to have unit
  length, as the Model2Vec Python package does by default. Defaults to
  `TRUE`.

- max_tokens:

  A single positive whole number giving the maximum number of tokens of
  a text that are used. Later tokens are ignored. Defaults to `512`, as
  in the Model2Vec Python package.

## Value

An object of class `sentence_embedding_engine`.

## Details

The embedding of a text is the mean of the vectors of its tokens, scaled
to unit length if `normalize = TRUE`. This is what Model2Vec does. The
text is tokenized with the WordPiece vocabulary of the model using
[`wordpiece::wordpiece_tokenize()`](https://rdrr.io/pkg/wordpiece/man/wordpiece_tokenize.html),
tokens that are not in the vocabulary (`"[UNK]"`) are ignored, and no
special tokens are added. A text without any known tokens gets a vector
of zeros.

This follows the way Model2Vec builds an embedding, but it is not
guaranteed to match the Python `model2vec` package exactly. Tokenizer
details such as the handling of accents or unusual characters may
differ, so compare a few texts with the Python package if you need
identical values. Only models that use a WordPiece vocabulary, such as
the English potion models, are supported. Models with a different
tokenizer, with quantized weights, or with extra weights for the tokens
can't be used.

The engine stores the path to the model folder, not the model. A prepped
recipe therefore needs the model files to be at the same path where it
is baked. The model is read the first time it is used and is then kept
for the rest of the R session. Each parallel worker reads its own copy.

Requires the wordpiece, safetensors and torch packages. The torch
package is only used to read the file, and the model is not run with it.

## See also

[`step_sentence_embeddings()`](https://textrecipes.tidymodels.org/reference/step_sentence_embeddings.md),
[`download_embeddings_model2vec()`](https://textrecipes.tidymodels.org/reference/download_embeddings_model2vec.md),
and
[`vignette("static-embeddings", package = "textrecipes")`](https://textrecipes.tidymodels.org/articles/static-embeddings.md)
for using the same model with
[`step_word_embeddings()`](https://textrecipes.tidymodels.org/reference/step_word_embeddings.md).

Other Embedding Engines:
[`embedding_custom()`](https://textrecipes.tidymodels.org/reference/embedding_custom.md),
[`embedding_huggingface()`](https://textrecipes.tidymodels.org/reference/embedding_huggingface.md),
[`embedding_ollama()`](https://textrecipes.tidymodels.org/reference/embedding_ollama.md)

## Examples

``` r
if (FALSE) {
# Downloads the model files
library(recipes)

url <- paste0(
  "https://huggingface.co/minishlab/potion-base-8M/resolve/",
  "bf8b056651a2c21b8d2565580b8569da283cab23"
)
model_dir <- download_embeddings_model2vec(url)

sample_data <- tibble(
  text = c("The cat sat.", "A dog ran.", "Stocks fell sharply today.")
)

rec <- recipe(~text, data = sample_data) |>
  step_sentence_embeddings(text, engine = embedding_model2vec(model_dir))

rec |>
  prep() |>
  bake(new_data = NULL)
}
```
