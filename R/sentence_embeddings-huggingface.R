#' Hugging Face Embedding Engine
#'
#' `embedding_huggingface()` creates an engine for [step_sentence_embeddings()]
#' that runs a [sentence-transformers](https://sbert.net) model from the
#' [Hugging Face Hub](https://huggingface.co/models?library=sentence-transformers)
#' on your own machine, using Python through reticulate.
#'
#' @param model A single string giving the id of the model on the Hugging Face
#'   Hub, such as `"sentence-transformers/all-MiniLM-L6-v2"`. There is no
#'   default, so the model is always visible in your code.
#' @param revision A single string giving the version of the model to use,
#'   preferably the hash of a commit. A branch name such as `"main"` is accepted,
#'   but it can point to a different version later on, which defeats the purpose
#'   of pinning. See the details for how to find the hash. There is no default,
#'   so the version is always visible in your code.
#' @param device A single string giving the device to run the model on: `"cpu"`,
#'   `"cuda"`, `"mps"`, or a numbered device such as `"cuda:1"`. Defaults to
#'   `NULL`, which lets sentence-transformers choose a device. The choice then
#'   depends on the machine, so set it if you want the recipe to behave in the
#'   same way everywhere.
#' @param text_prefix A single string added to the start of every text before it
#'   is embedded, or `NULL` for no prefix. Some models are trained with
#'   prefixes, for example `"query: "` or `"passage: "` for the e5 models.
#'   Defaults to `NULL`.
#' @param normalize A single logical. If `TRUE`, each embedding is scaled to
#'   have unit length. Defaults to `FALSE`, which returns the vectors as the
#'   model returns them. Some models already return vectors of unit length.
#' @param batch_size A single positive whole number giving the maximum number of
#'   texts the model embeds at once. Larger batches are faster, up to the memory
#'   of the device. Defaults to `32`.
#'
#' @details
#'
#' The embeddings are what `SentenceTransformer$encode()` returns for the model,
#' so the pooling and normalization steps that are part of the model are used.
#' Texts longer than the maximum sequence length of the model are cut off by
#' the model, as they are in sentence-transformers.
#'
#' The model is downloaded from the Hub the first time it is used, and is kept
#' in the Hugging Face cache on your machine, which is usually
#' `~/.cache/huggingface`. After that the model runs locally, and your text is
#' not sent anywhere. Loading a model into memory takes a while, so the loaded
#' model is kept for the rest of the R session. Each parallel worker loads its
#' own copy.
#'
#' The engine stores the model id, the revision, and the other settings, not the
#' model. A prepped recipe therefore needs the Python environment and the model
#' at the same revision to be available where it is baked. They are downloaded
#' on first use if the machine can reach the Hub.
#'
#' Before any text is embedded, the engine checks that the Python module can be
#' imported, that the device is available, and that the model can be loaded at
#' the revision, and gives an error that says what is wrong if not.
#'
#' Requires the reticulate package and the Python module `sentence_transformers`.
#' Models that need `trust_remote_code` are not supported.
#'
#' # Pinning the revision
#'
#' A model repository on the Hub can be updated, so the same model id can give
#' different embeddings over time. The `revision` argument fixes the version.
#' To find the hash of the latest commit, open the model page and then
#' *Files and versions*, or request `https://huggingface.co/api/models/<model>`
#' and read the `sha` field. Models that are pinned to a commit give the same
#' embeddings every time, up to small differences between devices.
#'
#' # Setting up Python
#'
#' The engine needs the Python module `sentence_transformers`, which brings in
#' torch. If you don't already have a Python environment you want to use, either
#' of these works:
#'
#' * Install it into the environment reticulate uses, for example with
#'   `reticulate::py_install("sentence-transformers")`.
#' * Before Python is started in the R session, run
#'   `reticulate::py_require("sentence-transformers")`, and reticulate sets up a
#'   temporary environment for you.
#'
#' See the [reticulate documentation](https://rstudio.github.io/reticulate/) for
#' choosing an environment.
#'
#' # macOS
#'
#' On macOS, loading torch in an R session that already has some R packages
#' loaded, such as data.table (which recipes depends on), can abort R with the
#' error `OMP: Error #15: Initializing libomp.dylib, but found libomp.dylib
#' already initialized`. This happens because both load their own OpenMP
#' library. To avoid it, the engine sets the environment variables
#' `KMP_DUPLICATE_LIB_OK` to `"TRUE"` and `OMP_NUM_THREADS` to `"1"` before the
#' model is loaded, unless you have already set them, and tells you when it
#' does. The model then runs on one thread, which is slower. The OpenMP
#' developers describe `KMP_DUPLICATE_LIB_OK` as an unsafe workaround that
#' can in principle give wrong results. In testing, the embeddings matched
#' those of Python to within `1e-6`. Set both variables yourself, for example in
#' `.Renviron`, to use other values or to stop the engine from setting them.
#'
#' The variables only help if they are set before Python is started. If you
#' have already used reticulate to start Python in the R session, for example
#' with `reticulate::py_config()`, the engine is too late to prevent the
#' crash. Restart R and set the variables in `.Renviron` in that case.
#'
#' @return An object of class `sentence_embedding_engine`.
#'
#' @seealso [step_sentence_embeddings()]
#' @family Embedding Engines
#'
#' @examplesIf FALSE
#' # Needs Python with the sentence-transformers module, and downloads the model
#' library(recipes)
#'
#' sample_data <- tibble(
#'   text = c("The cat sat.", "A dog ran.", "Stocks fell sharply today.")
#' )
#'
#' rec <- recipe(~text, data = sample_data) |>
#'   step_sentence_embeddings(
#'     text,
#'     engine = embedding_huggingface(
#'       "sentence-transformers/all-MiniLM-L6-v2",
#'       revision = "1110a243fdf4706b3f48f1d95db1a4f5529b4d41",
#'       device = "cpu"
#'     )
#'   )
#'
#' rec |>
#'   prep() |>
#'   bake(new_data = NULL)
#' @export
embedding_huggingface <- function(
  model,
  revision,
  device = NULL,
  text_prefix = NULL,
  normalize = FALSE,
  batch_size = 32
) {
  check_string(model, arg = "model")
  check_string(revision, arg = "revision")
  check_string(device, allow_null = TRUE, arg = "device")
  check_string(text_prefix, allow_null = TRUE, arg = "text_prefix")
  check_bool(normalize, arg = "normalize")
  check_number_whole(batch_size, min = 1, arg = "batch_size")

  if (!nzchar(model)) {
    cli::cli_abort("{.arg model} must not be an empty string.")
  }
  if (!nzchar(revision)) {
    cli::cli_abort("{.arg revision} must not be an empty string.")
  }
  if (!is.null(device) && !grepl("^[a-z]+(:[0-9]+)?$", device)) {
    cli::cli_abort(
      c(
        "{.arg device} must be a torch device such as {.val cpu},
        {.val mps}, {.val cuda}, or a numbered device such as {.val cuda:1}.",
        "x" = "It is {.val {device}}."
      )
    )
  }

  new_embedding_engine(
    "embedding_huggingface",
    model = model,
    revision = revision,
    device = device,
    text_prefix = text_prefix,
    normalize = normalize,
    batch_size = as.integer(batch_size)
  )
}

#' @export
engine_label.embedding_huggingface <- function(engine) {
  "huggingface"
}

#' @export
engine_model.embedding_huggingface <- function(engine) {
  engine$model
}

#' @export
engine_revision.embedding_huggingface <- function(engine) {
  engine$revision
}

#' @export
engine_required_pkgs.embedding_huggingface <- function(engine) {
  "reticulate"
}

# Everything that changes the embeddings goes into the key. `device` and
# `batch_size` only change them by rounding error.
#' @export
engine_cache_key.embedding_huggingface <- function(engine) {
  rlang::hash(list(
    "huggingface",
    engine$model,
    engine$revision,
    engine$text_prefix,
    engine$normalize
  ))
}

#' @export
engine_check.embedding_huggingface <- function(engine) {
  rlang::check_installed(
    "reticulate",
    reason = "to use `embedding_huggingface()`."
  )

  huggingface_avoid_openmp_conflict()

  if (!huggingface_module_available()) {
    cli::cli_abort(
      c(
        "The Python module {.val sentence_transformers} isn't available.",
        "i" = "Install it into the Python environment reticulate uses, for
               example with {.code reticulate::py_install(\"sentence-transformers\")}.",
        "i" = "Or run {.code reticulate::py_require(\"sentence-transformers\")}
               before Python is started in this R session."
      ),
      class = "huggingface_module_missing"
    )
  }

  if (!is.null(engine$device) && !huggingface_device_available(engine$device)) {
    cli::cli_abort(
      c(
        "The device {.val {engine$device}} isn't available.",
        "i" = "Use {.val cpu}, or a device that torch can see on this machine."
      ),
      class = "huggingface_device_unavailable"
    )
  }

  # Loading is the check that the model exists at the revision. The loaded model
  # is cached, so `engine_embed()` doesn't load it again.
  huggingface_model(engine)

  invisible(engine)
}

#' @export
engine_embed.embedding_huggingface <- function(engine, x) {
  if (anyNA(x)) {
    cli::cli_abort("{.arg x} can't contain missing values.")
  }

  if (length(x) == 0L) {
    return(matrix(NA_real_, nrow = 0L, ncol = 0L))
  }

  if (!is.null(engine$text_prefix)) {
    x <- paste0(engine$text_prefix, x)
  }

  model <- huggingface_model(engine)

  # A list, because reticulate turns a character vector of length 1 into a
  # Python string, and `encode()` then returns a single vector, not a matrix.
  res <- model$encode(
    as.list(x),
    batch_size = engine$batch_size,
    normalize_embeddings = engine$normalize,
    convert_to_numpy = TRUE,
    show_progress_bar = FALSE
  )

  res <- matrix(as.numeric(res), nrow = length(x))
  res
}

# Python -----------------------------------------------------------------------

# Session-level cache of loaded models. A loaded model is a reference to Python
# memory, so it can't be saved with a recipe. It lives outside the step and is
# never serialized.
huggingface_model_cache <- new.env(parent = emptyenv())

clear_huggingface_model_cache <- function() {
  rm(
    list = ls(huggingface_model_cache, all.names = TRUE),
    envir = huggingface_model_cache
  )
  invisible()
}

huggingface_model <- function(engine) {
  key <- paste(
    engine$model,
    engine$revision,
    engine$device %||% "auto",
    sep = "|"
  )

  if (!exists(key, envir = huggingface_model_cache, inherits = FALSE)) {
    assign(
      key,
      huggingface_load_model(engine),
      envir = huggingface_model_cache
    )
  }

  get(key, envir = huggingface_model_cache, inherits = FALSE)
}

is_macos <- function() {
  Sys.info()[["sysname"]] == "Darwin"
}

# Whether reticulate has already started Python in this R session. Doesn't start
# it.
python_started <- function() {
  reticulate::py_available()
}

# On macOS, R packages such as data.table (which recipes depends on) load R's
# OpenMP runtime, and torch loads its own. The second one aborts the R session
# with "OMP: Error #15". Allowing the duplicate and running on one thread avoids
# it. This has to be set before torch is imported, and variables the user has
# already set are left alone.
huggingface_avoid_openmp_conflict <- function() {
  if (!is_macos()) {
    return(invisible())
  }

  wanted <- c(KMP_DUPLICATE_LIB_OK = "TRUE", OMP_NUM_THREADS = "1")
  unset <- names(wanted)[!nzchar(Sys.getenv(names(wanted)))]
  if (length(unset) == 0L) {
    return(invisible())
  }

  do.call(Sys.setenv, as.list(wanted[unset]))

  if (python_started()) {
    cli::cli_warn(
      c(
        "Python was already started in this R session, so setting
         {.envvar {unset}} now may be too late to avoid an OpenMP crash on
         macOS.",
        "i" = "If R aborts with {.val OMP: Error #15}, set the variables in
               {.file .Renviron} and restart R."
      ),
      class = "huggingface_openmp_late"
    )
    return(invisible())
  }

  cli::cli_inform(
    c(
      "i" = "Set {.envvar {unset}} to avoid a crash caused by R and PyTorch
             both loading an OpenMP library on macOS.",
      "i" = "This runs the model on one thread. To use other values, set the
             variables yourself, for example in {.file .Renviron}."
    ),
    class = "huggingface_openmp_env"
  )

  invisible()
}

huggingface_module_available <- function() {
  reticulate::py_module_available("sentence_transformers")
}

huggingface_device_available <- function(device) {
  if (device == "cpu") {
    return(TRUE)
  }

  torch <- reticulate::import("torch")
  type <- sub(":.*$", "", device)

  if (type == "mps") {
    return(isTRUE(torch$backends$mps$is_available()))
  }

  if (type == "cuda") {
    if (!isTRUE(torch$cuda$is_available())) {
      return(FALSE)
    }
    if (grepl(":", device, fixed = TRUE)) {
      index <- as.integer(sub("^.*:", "", device))
      return(index < torch$cuda$device_count())
    }
    return(TRUE)
  }

  # Other backends (such as xpu) are left for sentence-transformers to report.
  TRUE
}

huggingface_sentence_transformer <- function(engine) {
  reticulate::import("sentence_transformers")$SentenceTransformer(
    engine$model,
    revision = engine$revision,
    device = engine$device
  )
}

huggingface_load_model <- function(engine) {
  tryCatch(
    huggingface_sentence_transformer(engine),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Can't load the model {.val {engine$model}} at revision
          {.val {engine$revision}}.",
          "i" = "See the error below for the cause. Common ones are a wrong
                 model id or revision ({.url https://huggingface.co/{engine$model}}),
                 no access to the Hub (gated models need you to be logged in),
                 too little memory, or a broken torch install."
        ),
        parent = cnd,
        call = NULL,
        class = "huggingface_model_not_found"
      )
    }
  )
}
