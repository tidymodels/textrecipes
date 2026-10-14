# errors when all text is missing, as the dimension is unknown

    Code
      prep(step_sentence_embeddings(recipe(~., data = dat), text, engine = fake$
      engine))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      ! Can't determine the embedding dimension.
      i All of the selected columns only contain missing values.

# a changed dimension is caught when baking

    Code
      bake(rec, new_data = tibble(text = "something unseen"))
    Condition
      Error in `bake()`:
      ! The embedding dimension changed since the recipe was prepped.
      i The custom function engine returned 5 dimensions but 3 were expected.
      i The model behind the engine may have changed.

# engine output is validated

    Code
      prep(step_sentence_embeddings(rec_base, text, engine = not_matrix))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      ! The custom function engine must return a numeric matrix.
      i It returned a string.
    Code
      prep(step_sentence_embeddings(rec_base, text, engine = wrong_rows))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      ! The custom function engine must return one row per string.
      i It was given 4 strings and returned 1 row.
    Code
      prep(step_sentence_embeddings(rec_base, text, engine = no_cols))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      ! The custom function engine returned a matrix with 0 columns.

# engine failures are wrapped with the engine name

    Code
      prep(step_sentence_embeddings(rec_base, text, engine = failing))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      ! Embedding failed in the custom function engine.
      Caused by error in `engine$fun()`:
      ! server on fire

# non-text columns are rejected

    Code
      prep(step_sentence_embeddings(recipe(~., data = tibble(x = 1:3)), x, engine = fake$
        engine))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be string, factor, or ordered.
      * 1 integer variable found: `x`

---

    Code
      bake(rec, new_data = tibble(text = 1:2))
    Condition
      Error in `bake()`:
      ! text must be a character or factor vector.
      i It is <integer>.

# bad args

    Code
      step_sentence_embeddings(rec_base, text)
    Condition
      Error in `step_sentence_embeddings()`:
      ! `engine` must be an embedding engine.
      i Create one with a function such as `embedding_custom()`.
    Code
      step_sentence_embeddings(rec_base, text, engine = "ollama")
    Condition
      Error in `step_sentence_embeddings()`:
      ! `engine` must be an embedding engine.
      i Create one with a function such as `embedding_custom()`.
    Code
      prep(step_sentence_embeddings(rec_base, text, engine = fake$engine, prefix = 1))
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not the number 1.
    Code
      embedding_custom("not a function")
    Condition
      Error in `embedding_custom()`:
      ! `fun` must be a function, not the string "not a function".

# embedding_custom() prints

    Code
      embedding_custom(function(x) x)
    Output
      <custom function>

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Sentence embeddings (custom function) for: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Sentence embeddings (custom function) for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Sentence embeddings (custom function) for: text

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Sentence embeddings (custom function) for: text | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_sentence_embeddings()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_sentence_embeddings()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `sentembed_text_1`

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = test_data[, -1])
    Condition
      Error in `step_sentence_embeddings()`:
      ! The following required column is missing from `new_data`: text.

