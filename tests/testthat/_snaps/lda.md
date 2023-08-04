# check_name() is used

    Code
      prep(rec, training = dat)
    Error <recipes_error_step>
      Error in `step_lda()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  lda_text_1

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
      * Text feature extraction for: <none>

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
      * Text feature extraction for: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Warning <rlang_warning>
      'keep_original_cols' was added to `step_lda()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * Tokenization for: medium
      * Text feature extraction for: medium

---

    Code
      prep(rec)
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 100 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: medium | Trained
      * Text feature extraction for: medium | Trained

