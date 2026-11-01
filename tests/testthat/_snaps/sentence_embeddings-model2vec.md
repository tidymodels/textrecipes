# embedding_model2vec() errors on bad arguments

    Code
      embedding_model2vec(1)
    Condition
      Error in `embedding_model2vec()`:
      ! `path` must be a single string, not the number 1.

---

    Code
      embedding_model2vec("")
    Condition
      Error in `embedding_model2vec()`:
      ! `path` must not be an empty string.

---

    Code
      embedding_model2vec("a", normalize = "yes")
    Condition
      Error in `embedding_model2vec()`:
      ! `normalize` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      embedding_model2vec("a", max_tokens = 0)
    Condition
      Error in `embedding_model2vec()`:
      ! `max_tokens` must be a whole number larger than or equal to 1, not the number 0.

