# tokenfilter removes words correctly using min_times and max_times

    Code
      obj <- rec %>% prep()
    Warning <rlang_warning>
      max_tokens was set to '100', but only 3 was available and selected.

# removes words correctly with min_times, max_times and procentage

    Code
      obj <- rec %>% prep()
    Warning <rlang_warning>
      max_tokens was set to '100', but only 12 was available and selected.

# tokenfilter throws warning when max_tokens > words

    Code
      rec %>% prep()
    Warning <rlang_warning>
      max_tokens was set to '10000', but only 17 was available and selected.
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: text | Trained
      * Text filtering for: text | Trained

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
      * Text filtering for: <none>

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
      * Text filtering for: <none> | Trained

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
      * Text filtering for: text

---

    Code
      prep(rec)
    Warning <rlang_warning>
      max_tokens was set to '100', but only 17 was available and selected.
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: text | Trained
      * Text filtering for: text | Trained

