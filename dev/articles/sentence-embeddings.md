# Sentence embeddings

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

[`step_word_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_word_embeddings.md)
looks up a vector for each token in a table you supply and aggregates
them.
[`step_sentence_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_sentence_embeddings.md)
is different: it takes a whole character column, runs each string
through an embedding model, and returns one dense vector per row. This
vignette covers how to use it, what the available engines are, and what
to think about before putting a recipe that uses it into production.

## The basics

The step needs an engine, which says how strings are turned into
vectors. The simplest engine,
[`embedding_custom()`](https://textrecipes.tidymodels.org/dev/reference/embedding_custom.md),
takes any function that maps a character vector to a numeric matrix with
one row per string. Here a toy function stands in for a real model.

``` r

toy_embedder <- function(x) {
  cbind(
    nchar = nchar(x),
    vowels = nchar(gsub("[^aeiou]", "", x))
  )
}

sample_data <- tibble::tibble(
  text = c("The cat sat.", "A dog ran.", "Stocks fell sharply today.")
)

rec <- recipe(~text, data = sample_data) |>
  step_sentence_embeddings(text, engine = embedding_custom(toy_embedder))

rec |>
  prep() |>
  bake(new_data = NULL)
#> # A tibble: 3 × 2
#>   sentembed_text_1 sentembed_text_2
#>              <dbl>            <dbl>
#> 1               12                3
#> 2               10                2
#> 3               26                5
```

The new columns are named from the `prefix` argument, the variable, and
the position of the dimension. Missing text stays missing: a row with
`NA` gets `NA` in all of its embedding columns and is never sent to the
engine.

## Engines

[`embedding_custom()`](https://textrecipes.tidymodels.org/dev/reference/embedding_custom.md)
is the escape hatch. Use it for any backend that doesn’t have its own
engine, such as a hosted API or a model you run yourself.

[`embedding_ollama()`](https://textrecipes.tidymodels.org/dev/reference/embedding_ollama.md)
gets embeddings from a running [Ollama](https://ollama.com) server. It
needs no Python environment and no API key, only the httr2 package and a
server with the model pulled.

To set it up:

1.  Install Ollama from <https://ollama.com/download>. The [Ollama
    documentation](https://docs.ollama.com) has instructions for each
    system.
2.  Make sure the server is running. The desktop apps start it when you
    open them. Otherwise run `ollama serve` in a terminal and leave it
    running.
3.  Pull the model once with `ollama pull all-minilm`. You can see which
    models are on the server with `ollama list`, and browse more at
    <https://ollama.com/library>.

Then use the engine in a recipe.

``` r

rec <- recipe(~text, data = sample_data) |>
  step_sentence_embeddings(
    text,
    engine = embedding_ollama("all-minilm")
  )
```

There is no default model, so the model is always visible in your code.
`all-minilm` is one of the smallest embedding models, which makes it a
good choice for trying things out. Larger models usually give better
embeddings, at the cost of download size and speed.

Some models expect a prefix on the text, for example `"query: "` or
`"passage: "`. Use the `text_prefix` argument of the engine for this, so
the prefix is visible in the recipe. Use `normalize = TRUE` to scale
each embedding to unit length.

[`embedding_huggingface()`](https://textrecipes.tidymodels.org/dev/reference/embedding_huggingface.md)
runs a [sentence-transformers](https://sbert.net) model from the Hugging
Face Hub in Python, through reticulate. It gives you control over the
device and batch size, and the model runs inside your R session with no
server to set up. It needs Python with the `sentence_transformers`
module, which brings in torch. Install it into the environment
reticulate uses with `reticulate::py_install("sentence-transformers")`,
or run `reticulate::py_require("sentence-transformers")` before Python
starts in the session.

``` r

rec <- recipe(~text, data = sample_data) |>
  step_sentence_embeddings(
    text,
    engine = embedding_huggingface(
      "sentence-transformers/all-MiniLM-L6-v2",
      revision = "1110a243fdf4706b3f48f1d95db1a4f5529b4d41",
      device = "cpu"
    )
  )
```

Both `model` and `revision` are required. The revision is the hash of a
commit of the model repository, and it fixes the version of the model,
so the embeddings don’t change if the model is updated on the Hub. You
can find the hash under *Files and versions* on the model page. The
model is downloaded the first time it is used, and after that your text
stays on your machine. Loading a model takes a while, so it is kept for
the rest of the R session.

On macOS, loading torch in a session that has recipes loaded can abort R
with an `OMP: Error #15` message about `libomp`. The engine sets the
environment variables `KMP_DUPLICATE_LIB_OK` and `OMP_NUM_THREADS` to
avoid this, which makes the model run on one thread, and says so when it
does. See
[`?embedding_huggingface`](https://textrecipes.tidymodels.org/dev/reference/embedding_huggingface.md)
for the details and for what to do if Python was already started in your
session.

## Local models and hosted services

Where the model runs decides what you have to set up and what happens to
your text.

- A local model, such as one served by Ollama on your own machine, costs
  nothing per request, but needs local compute and setup, and large
  models can be slow without a GPU.
- A hosted service needs no local setup, but usually costs money per
  request, and the text in your data is sent to a third party.

[`step_word_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_word_embeddings.md)
never had this second concern, because it only looks up vectors in a
table that is already on your machine. With this step, the text in your
data leaves R whenever an engine talks to a server, so check that this
is acceptable for your data before using a server you don’t control. The
`host` argument of
[`embedding_ollama()`](https://textrecipes.tidymodels.org/dev/reference/embedding_ollama.md)
controls where the text goes.

## What a recipe stores

Embedding models are large, so a prepped recipe never stores the model.
It stores the engine, which is a reference to the model: the model name
and the server address. This keeps recipes small, but it changes what
“the recipe works on another machine” means. A prepped recipe can only
be baked where its engine can run.

``` r

# Fine to save, the recipe holds the engine settings, not the model
rec_prepped <- prep(rec)
saveRDS(rec_prepped, "recipe.rds")
```

For
[`embedding_ollama()`](https://textrecipes.tidymodels.org/dev/reference/embedding_ollama.md),
the machine that bakes the recipe needs:

- the httr2 package,
- an Ollama server it can reach at the same `host`,
- the same model pulled on that server.

The engine doesn’t pin a model version. An Ollama model name such as
`"all-minilm"` can point to a different version after the model is
updated. If the new version has a different number of dimensions,
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) stops
with an error, but if it has the same number of dimensions the change
goes unnoticed, and the features will differ from the ones the
downstream model was trained on. When this matters, pin the model to a
specific tag, and update it deliberately.

For
[`embedding_huggingface()`](https://textrecipes.tidymodels.org/dev/reference/embedding_huggingface.md),
the machine that bakes the recipe needs:

- the reticulate package and a Python environment with the
  `sentence_transformers` module,
- the model at the pinned `revision`, which is downloaded from the Hub
  on first use, so the machine needs access to it unless the model is
  already in the Hugging Face cache,
- the device the engine asks for, if you set one.

The loaded model is a reference to memory in the Python process, which
can’t be saved, so the recipe stores only the model id, the revision,
and the other settings.
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) shows the
model and the revision.

The checks that run before any text is embedded make these problems easy
to find. If the server can’t be reached, or the model isn’t on it,
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) stops
right away and says how to fix it, instead of failing partway through
the data. If anything fails while embedding, the whole
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) stops,
and you never get a data frame with some rows missing.

For
[`embedding_custom()`](https://textrecipes.tidymodels.org/dev/reference/embedding_custom.md),
the function is stored in the recipe. If the function captures a live
object, such as a Python model through reticulate, that object will not
survive [`saveRDS()`](https://rdrr.io/r/base/readRDS.html). Have the
function load the model from a path or an identifier when it is called,
instead of capturing it.

## Speed, caching, and tuning

Embedding is usually the slowest part of a recipe. To keep repeated work
down, embeddings are cached within the R session, per engine and per
string:

- [`prep()`](https://recipes.tidymodels.org/reference/prep.html) embeds
  the training data, and baking the training data afterwards reuses
  those embeddings.
- A string that appears more than once is only embedded once.
- The cache is not stored in the recipe, and is gone when the R session
  ends.

The cache doesn’t help across sessions or across parallel workers. When
a recipe is used in tuning, it is prepped once per resample, and each
worker has its own cache and needs access to the engine. Embedding the
text once before tuning, and treating the embeddings as ordinary numeric
columns, is often more practical for large datasets than embedding
inside the recipe.

## Limitations

The step doesn’t batch work for datasets that don’t fit in memory, and
it doesn’t do any fine-tuning of the model. Each selected column is
embedded on its own, so features that compare two texts, such as a query
and a document, are not supported.
