# it complains when the selected column isn't a tokenlist

    Code
      prep(rec)
    Condition
      Error in `step_tokenmerge()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be tokenlist.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_tokenmerge()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  tokenmerge

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
      * Merging tokens for: <none>

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
      * Merging tokens for: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      'keep_original_cols' was added to `step_tokenmerge()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * Tokenization for: text1, text2
      * Merging tokens for: text1, text2

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: text1, text2 | Trained
      * Merging tokens for: text1, text2 | Trained

