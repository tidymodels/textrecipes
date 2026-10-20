# embedding_huggingface() validates its arguments

    Code
      embedding_huggingface(1, "abc")
    Condition
      Error in `embedding_huggingface()`:
      ! `model` must be a single string, not the number 1.

---

    Code
      embedding_huggingface("", "abc")
    Condition
      Error in `embedding_huggingface()`:
      ! `model` must not be an empty string.

---

    Code
      embedding_huggingface("org/model")
    Condition
      Error in `embedding_huggingface()`:
      ! `revision` must be a single string, not absent.

---

    Code
      embedding_huggingface("org/model", "")
    Condition
      Error in `embedding_huggingface()`:
      ! `revision` must not be an empty string.

---

    Code
      embedding_huggingface("org/model", "abc", device = "cuda:x")
    Condition
      Error in `embedding_huggingface()`:
      ! `device` must be a torch device such as "cpu", "mps", "cuda", or a numbered device such as "cuda:1".
      x It is "cuda:x".

---

    Code
      embedding_huggingface("org/model", "abc", batch_size = 0)
    Condition
      Error in `embedding_huggingface()`:
      ! `batch_size` must be a whole number larger than or equal to 1, not the number 0.

# the engine prints its label

    Code
      engine
    Output
      <huggingface>

# a missing Python module is reported with how to fix it

    Code
      prep(step_sentence_embeddings(rec_base, text, engine = engine))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      ! The huggingface engine can't be used.
      Caused by error in `engine_check()`:
      ! The Python module "sentence_transformers" isn't available.
      i Install it into the Python environment reticulate uses, for example with `reticulate::py_install("sentence-transformers")`.
      i Or run `reticulate::py_require("sentence-transformers")` before Python is started in this R session.

# an unavailable device is reported

    Code
      prep(step_sentence_embeddings(rec_base, text, engine = eng))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      ! The huggingface engine can't be used.
      Caused by error in `engine_check()`:
      ! The device "cuda" isn't available.
      i Use "cpu", or a device that torch can see on this machine.

# a model that can't be loaded is reported with model and revision

    Code
      prep(step_sentence_embeddings(rec_base, text, engine = engine))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      ! The huggingface engine can't be used.
      Caused by error:
      ! Can't load the model "org/model" at revision "abc123".
      i See the error below for the cause. Common ones are a wrong model id or revision (<https://huggingface.co/org/model>), no access to the Hub (gated models need you to be logged in), too little memory, or a broken torch install.
      Caused by error in `huggingface_sentence_transformer()`:
      ! 404 Client Error: Repository Not Found

# a failure while embedding aborts the whole bake

    Code
      prep(step_sentence_embeddings(rec_base, text, engine = engine))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      ! Embedding failed in the huggingface engine.
      Caused by error in `model$encode()`:
      ! out of memory

# OpenMP variables are set on macOS when they are unset

    Code
      huggingface_avoid_openmp_conflict()
    Message
      i Set `KMP_DUPLICATE_LIB_OK` and `OMP_NUM_THREADS` to avoid a crash caused by R and PyTorch both loading an OpenMP library on macOS.
      i This runs the model on one thread. To use other values, set the variables yourself, for example in '.Renviron'.

# OpenMP variables the user has set are left alone

    Code
      huggingface_avoid_openmp_conflict()
    Message
      i Set `KMP_DUPLICATE_LIB_OK` to avoid a crash caused by R and PyTorch both loading an OpenMP library on macOS.
      i This runs the model on one thread. To use other values, set the variables yourself, for example in '.Renviron'.

