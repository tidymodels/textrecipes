# Ollama Embedding Engine

`embedding_ollama()` creates an engine for
[`step_sentence_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_sentence_embeddings.md)
that gets embeddings from a running [Ollama](https://ollama.com) server.
No Python environment or API key is needed, the engine sends HTTP
requests to the server, which is usually running on your own machine.

## Usage

``` r
embedding_ollama(
  model,
  host = "http://localhost:11434",
  text_prefix = NULL,
  normalize = FALSE,
  batch_size = 64
)
```

## Arguments

- model:

  A single string giving the name of the Ollama model, such as
  `"all-minilm"`. The model must already be available on the server,
  which you can do with `ollama pull <model>`. There is no default, so
  the model is always visible in your code.

- host:

  A single string giving the address of the Ollama server. Defaults to
  `"http://localhost:11434"`.

- text_prefix:

  A single string added to the start of every text before it is
  embedded, or `NULL` for no prefix. Some models are trained with
  prefixes, for example `"query: "` or `"passage: "`. Defaults to
  `NULL`.

- normalize:

  A single logical. If `TRUE`, each embedding is scaled to have unit
  length. Defaults to `FALSE`, which returns the vectors as the server
  returns them.

- batch_size:

  A single positive whole number giving the maximum number of texts sent
  in one request. Defaults to `64`.

## Value

An object of class `sentence_embedding_engine`.

## Details

Embeddings are requested from the `/api/embed` endpoint. Requests that
fail with a transient error (too many requests, or the server being
unavailable) are retried a few times. Any other failure stops
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) with an
error, so partial results are never returned. Texts longer than the
context length of the model are an error rather than being silently
truncated.

Before any text is embedded, the engine checks that the server can be
reached and that `model` is available on it, and gives an error that
says how to fix it if not. The text is sent to the server at `host`. For
a server you don't control, keep in mind that this is where your data
goes.

The engine stores the model name and the server address, not the model.
A prepped recipe therefore needs the same model to be available on the
server when it is baked, and Ollama model names, such as `"all-minilm"`,
can point to different model versions over time. The engine doesn't pin
a version.

Requires the httr2 package.

## Setting up Ollama

The engine needs an Ollama server that is running and has the model.

1.  Install Ollama from <https://ollama.com/download>. See the [Ollama
    documentation](https://docs.ollama.com) for your system.

2.  Make sure the server is running. The desktop apps start it when you
    open them. Otherwise run `ollama serve` in a terminal and leave it
    running.

3.  Pull the model once, for example `ollama pull all-minilm`. Models
    are listed at <https://ollama.com/library>.

You can check which models are on the server with `ollama list`.

## See also

[`step_sentence_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_sentence_embeddings.md)

Other Embedding Engines:
[`embedding_custom()`](https://textrecipes.tidymodels.org/dev/reference/embedding_custom.md)

## Examples

``` r
if (FALSE) {
# Needs a running Ollama server with the model pulled:
#   ollama pull all-minilm
library(recipes)

sample_data <- tibble(
  text = c("The cat sat.", "A dog ran.", "Stocks fell sharply today.")
)

rec <- recipe(~text, data = sample_data) |>
  step_sentence_embeddings(
    text,
    engine = embedding_ollama("all-minilm")
  )

rec |>
  prep() |>
  bake(new_data = NULL)
}
```
