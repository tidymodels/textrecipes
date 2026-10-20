# Hugging Face Embedding Engine

`embedding_huggingface()` creates an engine for
[`step_sentence_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_sentence_embeddings.md)
that runs a [sentence-transformers](https://sbert.net) model from the
[Hugging Face
Hub](https://huggingface.co/models?library=sentence-transformers) on
your own machine, using Python through reticulate.

## Usage

``` r
embedding_huggingface(
  model,
  revision,
  device = NULL,
  text_prefix = NULL,
  normalize = FALSE,
  batch_size = 32
)
```

## Arguments

- model:

  A single string giving the id of the model on the Hugging Face Hub,
  such as `"sentence-transformers/all-MiniLM-L6-v2"`. There is no
  default, so the model is always visible in your code.

- revision:

  A single string giving the version of the model to use, preferably the
  hash of a commit. A branch name such as `"main"` is accepted, but it
  can point to a different version later on, which defeats the purpose
  of pinning. See the details for how to find the hash. There is no
  default, so the version is always visible in your code.

- device:

  A single string giving the device to run the model on: `"cpu"`,
  `"cuda"`, `"mps"`, or a numbered device such as `"cuda:1"`. Defaults
  to `NULL`, which lets sentence-transformers choose a device. The
  choice then depends on the machine, so set it if you want the recipe
  to behave in the same way everywhere.

- text_prefix:

  A single string added to the start of every text before it is
  embedded, or `NULL` for no prefix. Some models are trained with
  prefixes, for example `"query: "` or `"passage: "` for the e5 models.
  Defaults to `NULL`.

- normalize:

  A single logical. If `TRUE`, each embedding is scaled to have unit
  length. Defaults to `FALSE`, which returns the vectors as the model
  returns them. Some models already return vectors of unit length.

- batch_size:

  A single positive whole number giving the maximum number of texts the
  model embeds at once. Larger batches are faster, up to the memory of
  the device. Defaults to `32`.

## Value

An object of class `sentence_embedding_engine`.

## Details

The embeddings are what `SentenceTransformer$encode()` returns for the
model, so the pooling and normalization steps that are part of the model
are used. Texts longer than the maximum sequence length of the model are
cut off by the model, as they are in sentence-transformers.

The model is downloaded from the Hub the first time it is used, and is
kept in the Hugging Face cache on your machine, which is usually
`~/.cache/huggingface`. After that the model runs locally, and your text
is not sent anywhere. Loading a model into memory takes a while, so the
loaded model is kept for the rest of the R session. Each parallel worker
loads its own copy.

The engine stores the model id, the revision, and the other settings,
not the model. A prepped recipe therefore needs the Python environment
and the model at the same revision to be available where it is baked.
They are downloaded on first use if the machine can reach the Hub.

Before any text is embedded, the engine checks that the Python module
can be imported, that the device is available, and that the model can be
loaded at the revision, and gives an error that says what is wrong if
not.

Requires the reticulate package and the Python module
`sentence_transformers`. Models that need `trust_remote_code` are not
supported.

## Pinning the revision

A model repository on the Hub can be updated, so the same model id can
give different embeddings over time. The `revision` argument fixes the
version. To find the hash of the latest commit, open the model page and
then *Files and versions*, or request
`https://huggingface.co/api/models/<model>` and read the `sha` field.
Models that are pinned to a commit give the same embeddings every time,
up to small differences between devices.

## Setting up Python

The engine needs the Python module `sentence_transformers`, which brings
in torch. If you don't already have a Python environment you want to
use, either of these works:

- Install it into the environment reticulate uses, for example with
  `reticulate::py_install("sentence-transformers")`.

- Before Python is started in the R session, run
  `reticulate::py_require("sentence-transformers")`, and reticulate sets
  up a temporary environment for you.

See the [reticulate
documentation](https://rstudio.github.io/reticulate/) for choosing an
environment.

## macOS

On macOS, loading torch in an R session that already has some R packages
loaded, such as data.table (which recipes depends on), can abort R with
the error
`OMP: Error #15: Initializing libomp.dylib, but found libomp.dylib already initialized`.
This happens because both load their own OpenMP library. To avoid it,
the engine sets the environment variables `KMP_DUPLICATE_LIB_OK` to
`"TRUE"` and `OMP_NUM_THREADS` to `"1"` before the model is loaded,
unless you have already set them, and tells you when it does. The model
then runs on one thread, which is slower. The OpenMP developers describe
`KMP_DUPLICATE_LIB_OK` as an unsafe workaround that can in principle
give wrong results. In testing, the embeddings matched those of Python
to within `1e-6`. Set both variables yourself, for example in
`.Renviron`, to use other values or to stop the engine from setting
them.

The variables only help if they are set before Python is started. If you
have already used reticulate to start Python in the R session, for
example with
[`reticulate::py_config()`](https://rstudio.github.io/reticulate/reference/py_config.html),
the engine is too late to prevent the crash. Restart R and set the
variables in `.Renviron` in that case.

## See also

[`step_sentence_embeddings()`](https://textrecipes.tidymodels.org/dev/reference/step_sentence_embeddings.md)

Other Embedding Engines:
[`embedding_custom()`](https://textrecipes.tidymodels.org/dev/reference/embedding_custom.md),
[`embedding_ollama()`](https://textrecipes.tidymodels.org/dev/reference/embedding_ollama.md)

## Examples

``` r
if (FALSE) {
# Needs Python with the sentence-transformers module, and downloads the model
library(recipes)

sample_data <- tibble(
  text = c("The cat sat.", "A dog ran.", "Stocks fell sharply today.")
)

rec <- recipe(~text, data = sample_data) |>
  step_sentence_embeddings(
    text,
    engine = embedding_huggingface(
      "sentence-transformers/all-MiniLM-L6-v2",
      revision = "1110a243fdf4706b3f48f1d95db1a4f5529b4d41",
      device = "cpu"
    )
  )

rec |>
  prep() |>
  bake(new_data = NULL)
}
```
