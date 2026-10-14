# a missing model gives an error that says how to pull it

    Code
      httr2::with_mocked_responses(fake$mock, prep(step_sentence_embeddings(rec_base,
        text, engine = engine)))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      ! The ollama engine can't be used.
      Caused by error in `engine_check()`:
      ! The model "all-minilm" isn't available on the Ollama server.
      i Pull it with `ollama pull all-minilm`.
      i Server: <http://localhost:11434>

# an unreachable server gives an informative error

    Code
      httr2::with_mocked_responses(unreachable, prep(step_sentence_embeddings(
        rec_base, text, engine = engine)))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      ! The ollama engine can't be used.
      Caused by error in `ollama_models()`:
      ! Can't reach the Ollama server at <http://localhost:11434>.
      i Is it running? Start it with `ollama serve`.
      i To use a server at another address, set `host`.
      Caused by error in `mock()`:
      ! Couldn't connect to server

# HTTP errors stop bake() with the server's message

    Code
      httr2::with_mocked_responses(failing, prep(step_sentence_embeddings(rec_base,
        text, engine = engine)))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      ! Embedding failed in the ollama engine.
      Caused by error in `httr2::req_perform()`:
      ! HTTP 400 Bad Request.
      i input exceeds the context length

# a response with the wrong number of embeddings is an error

    Code
      httr2::with_mocked_responses(fake$mock, prep(step_sentence_embeddings(rec_base,
        text, engine = engine)))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      ! Embedding failed in the ollama engine.
      Caused by error in `ollama_embed_batch()`:
      ! The Ollama server returned an unexpected response.
      i It was sent 4 texts and returned 1 embedding.

# a changed dimension is caught when baking

    Code
      httr2::with_mocked_responses(changed$mock, bake(rec, new_data = tibble(text = "unseen text")))
    Condition
      Error in `bake()`:
      ! The embedding dimension changed since the recipe was prepped.
      i The ollama engine returned 5 dimensions but 3 were expected.
      i The model behind the engine may have changed.

# embedding_ollama() validates its arguments

    Code
      embedding_ollama()
    Condition
      Error in `embedding_ollama()`:
      ! `model` must be a single string, not absent.
    Code
      embedding_ollama(1)
    Condition
      Error in `embedding_ollama()`:
      ! `model` must be a single string, not the number 1.
    Code
      embedding_ollama("")
    Condition
      Error in `embedding_ollama()`:
      ! `model` must not be an empty string.
    Code
      embedding_ollama("all-minilm", host = "")
    Condition
      Error in `embedding_ollama()`:
      ! `host` must not be an empty string.
    Code
      embedding_ollama("all-minilm", text_prefix = 1)
    Condition
      Error in `embedding_ollama()`:
      ! `text_prefix` must be a single string or `NULL`, not the number 1.
    Code
      embedding_ollama("all-minilm", normalize = "yes")
    Condition
      Error in `embedding_ollama()`:
      ! `normalize` must be `TRUE` or `FALSE`, not the string "yes".
    Code
      embedding_ollama("all-minilm", batch_size = 0)
    Condition
      Error in `embedding_ollama()`:
      ! `batch_size` must be a whole number larger than or equal to 1, not the number 0.

# embedding_ollama() prints

    Code
      embedding_ollama("all-minilm")
    Output
      <ollama>

# printing of the step shows the engine

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Sentence embeddings (ollama) for: text

