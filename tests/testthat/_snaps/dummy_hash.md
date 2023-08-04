# check_name() is used

    Code
      prep(rec, training = dat)
    Error <recipes_error_step>
      Error in `step_dummy_hash()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  dummyhash_text_01

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
      * Feature hashing with: <none>

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
      * Feature hashing with: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Warning <rlang_warning>
      'keep_original_cols' was added to `step_dummy_hash()` after this recipe was created.
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
      * Feature hashing with: sponsor_code

---

    Code
      prep(rec)
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 20 data points and no incomplete rows.
      
      -- Operations 
      * Feature hashing with: sponsor_code | Trained

