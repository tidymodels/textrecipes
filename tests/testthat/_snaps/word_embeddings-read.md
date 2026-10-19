# read_embeddings_bpemb() errors on bad input

    Code
      read_embeddings_bpemb(1)
    Condition
      Error in `read_embeddings_bpemb()`:
      ! `path` must be a single string, not the number 1.

---

    Code
      read_embeddings_bpemb("not-a-file.w2v.txt")
    Condition
      Error in `read_embeddings_bpemb()`:
      ! Can't find the file 'not-a-file.w2v.txt'.

---

    Code
      read_embeddings_bpemb(write_bpemb(c("1 2 3", "a 1 2")))
    Condition
      Error in `read_embeddings_bpemb()`:
      ! The first line of '<file>' must give the number of rows and columns.

---

    Code
      read_embeddings_bpemb(write_bpemb(c("5 2", "a 1 2")))
    Condition
      Error in `read_embeddings_bpemb()`:
      ! The first line of '<file>' gives 5 rows and 2 vector columns, but the file has 1 row and 2 vector columns.

---

    Code
      read_embeddings_bpemb(write_bpemb(c("1 3", "a 1 2")))
    Condition
      Error in `read_embeddings_bpemb()`:
      ! The first line of '<file>' gives 1 row and 3 vector columns, but the file has 1 row and 2 vector columns.

# read_embeddings_model2vec() errors on bad input

    Code
      read_embeddings_model2vec(1)
    Condition
      Error in `read_embeddings_model2vec()`:
      ! `path` must be a single string, not the number 1.

---

    Code
      read_embeddings_model2vec(empty)
    Condition
      Error in `read_embeddings_model2vec()`:
      ! Can't find 'model.safetensors' in '<dir>'.

---

    Code
      read_embeddings_model2vec(write_model2vec(vectors, c("a", "b", "c")))
    Condition
      Error in `read_embeddings_model2vec()`:
      ! 'vocab.txt' has 3 tokens but 'model.safetensors' has 2 vectors.

---

    Code
      read_embeddings_model2vec(write_model2vec(vectors, c("a", "b"), tensor_name = "weights"))
    Condition
      Error in `read_embeddings_model2vec()`:
      ! 'model.safetensors' must contain a tensor named "embeddings".
      i It contains "weights".
      i Models with quantized weights are not supported.

