# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_word_embeddings()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `wordembed_text_d1`

# bad args

    Code
      prep(step_word_embeddings(recipe(~., data = mtcars), embeddings = embeddings,
      aggregation = "wrong"))
    Condition
      Error in `step_word_embeddings()`:
      ! `aggregation` must be one of "sum", "mean", "min", or "max", not "wrong".

---

    Code
      prep(step_word_embeddings(recipe(~., data = mtcars), embeddings = embeddings,
      aggregation_default = "yes"))
    Condition
      Error in `step_word_embeddings()`:
      Caused by error in `prep()`:
      ! `aggregation_default` must be a number, not the string "yes".

---

    Code
      prep(step_word_embeddings(recipe(~., data = mtcars), embeddings = embeddings,
      prefix = NULL))
    Condition
      Error in `step_word_embeddings()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = tokenized_test_data[, -1])
    Condition
      Error in `step_word_embeddings()`:
      ! The following required column is missing from `new_data`: text.

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
      * Word embeddings aggregated from: <none>

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
      * Word embeddings aggregated from: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_word_embeddings()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Tokenization for: text
      * Word embeddings aggregated from: text

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
      * Tokenization for: text | Trained
      * Word embeddings aggregated from: text | Trained

