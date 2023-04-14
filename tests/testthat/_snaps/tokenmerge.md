# it complains when the selected column isn't a tokenlist

    Code
      prep(rec)
    Error <recipes_error_step>
      Error in `step_tokenmerge()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be tokenlist.

# check_name() is used

    Code
      prep(rec, training = dat)
    Error <recipes_error_step>
      Error in `step_tokenmerge()`:
      Caused by error in `check_name()`:
      ! Name collision occured in `step_tokenmerge`. The following variable names already exists: tokenmerge.

# printing

    Code
      print(rec)
    Message <cliMessage>
      
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
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: text1, text2 | Trained
      * Merging tokens for: text1, text2 | Trained

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
      * Merging tokens for: <none>

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
      * Merging tokens for: <none> | Trained

