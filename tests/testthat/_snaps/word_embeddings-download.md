# download_embeddings_model2vec() errors informatively

    Code
      download_embeddings_model2vec(1)
    Condition
      Error in `download_embeddings_model2vec()`:
      ! `url` must be a single string, not the number 1.

---

    Code
      download_embeddings_model2vec("https://example.com", path = 1)
    Condition
      Error in `download_embeddings_model2vec()`:
      ! `path` must be a single string, not the number 1.

---

    Code
      download_embeddings_model2vec("https://example.com", withr::local_tempdir())
    Condition
      Error in `download_embeddings_model2vec()`:
      ! Can't download 'model.safetensors' from <https://example.com/model.safetensors>.
      Caused by error in `download_file()`:
      ! cannot open URL

