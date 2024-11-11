# bad args

    Code
      recipe(~., data = mtcars) %>% step_text_normalization(normalization_form = "wrong") %>%
        prep()
    Condition
      Error in `step_text_normalization()`:
      Caused by error in `prep()`:
      ! `normalization_form` must be one of "nfc", "nfd", "nfkd", "nfkc", or "nfkc_casefold", not "wrong".

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = ex_dat[, -1])
    Condition
      Error in `step_text_normalization()`:
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
      * Text Normalization for: <none>

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
      * Text Normalization for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Text Normalization for: text

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 2 data points and no incomplete rows.
      
      -- Operations 
      * Text Normalization for: text | Trained

