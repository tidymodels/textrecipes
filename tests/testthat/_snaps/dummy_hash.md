# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_dummy_hash()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `dummyhash_text_01`

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = test_data[, -2])
    Condition
      Error in `step_dummy_hash()`:
      ! The following required column is missing from `new_data`: sponsor_code.

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
      * Feature hashing with: <none>

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
      * Feature hashing with: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_dummy_hash()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * Feature hashing with: sponsor_code

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 20 data points and no incomplete rows.
      
      -- Operations 
      * Feature hashing with: sponsor_code | Trained

