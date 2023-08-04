# check_name() is used

    Code
      prep(rec, training = dat)
    Error <recipes_error_step>
      Error in `step_word_embeddings()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  wordembed_text_d1

# empty printing

    Code
      rec
    Message <cliMessage>
      
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
    Message <cliMessage>
      
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
    Warning <rlang_warning>
      'keep_original_cols' was added to `step_word_embeddings()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message <cliMessage>
      
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
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: text | Trained
      * Word embeddings aggregated from: text | Trained

